(in-package #:png)

(defun read-u2le (in)
  "Reads 2 unsigned BYTES in little-endian from IN stream without
testing for end of file."
  (let ((u 0))
    (setf (ldb (byte 8 0) u) (read-byte in))
    (setf (ldb (byte 8 8) u) (read-byte in))
    u))

(defun read-u4le (in)
  "Reads 4 unsigned BYTES in little-endian from IN stream without
testing for end of file."
  (let ((u 0))
    (setf (ldb (byte 8  0) u) (read-byte in))
    (setf (ldb (byte 8  8) u) (read-byte in))
    (setf (ldb (byte 8 16) u) (read-byte in))
    (setf (ldb (byte 8 24) u) (read-byte in))
    u))

(defun write-u2le (out u)
  "Writes 2 unsigned BYTES in little-endian to OUT stream."
  (write-byte (ldb (byte 8  0) u) out)
  (write-byte (ldb (byte 8  8) u) out))

(defun write-u4le (out u)
  "Writes 4 unsigned BYTES in little-endian to OUT stream."
  (write-byte (ldb (byte 8  0) u) out)
  (write-byte (ldb (byte 8  8) u) out)
  (write-byte (ldb (byte 8 16) u) out)
  (write-byte (ldb (byte 8 24) u) out))

(defstruct bmp-header
  bm
  file-sz
  reserve
  raster-data-offset
  sz
  cols
  rows
  planes
  bitcount
  compression
  image-sz
  ppm
  colors)

(defun bmp-pixels (hd)
  "Calculates the number of pixels in a BMP image"
  (* (bmp-header-rows hd) (bmp-header-cols hd)))

(defun decode-bmp (input &key flip)
  "Reads an image in BMP format from input and returns an array of
type IMAGE.  The bit depth of the returned IMAGE will be either 8 or
16.

Flips image vertically if FLIP set.

The current version...

Signals an error if reading the image fails."
  ;; Check for "BM" signature at beginning
  (unless (= #x4d42 (read-u2le input))
    (error "~s Not a BMP bitmap image stream" input))
  ;; Read rest of header
  (let ((hd (make-bmp-header)))
    (setf (bmp-header-file-sz hd) (read-u4le input)
          (bmp-header-reserve hd) (read-u4le input)
          (bmp-header-raster-data-offset hd) (read-u4le input)
          (bmp-header-sz hd) (read-u4le input)
          (bmp-header-cols hd) (read-u4le input)
          (bmp-header-rows hd) (read-u4le input)
          (bmp-header-planes hd) (read-u2le input)
          (bmp-header-bitcount hd) (read-u2le input)
          (bmp-header-compression hd) (read-u4le input)
          (bmp-header-image-sz hd) (read-u4le input)
          (bmp-header-ppm hd) (list (read-u4le input)
                                    (read-u4le input))
          (bmp-header-colors hd) (list (read-u4le input)
                                       (read-u4le input)))
    ;; Note: the image-sz is only set if compression is being used,
    ;; otherwise it us generally set to 0, so it should be calculated
    ;; from the filesize and the raster-data-offset, both of which
    ;; must be accurate.
    (setf (bmp-header-image-sz hd) (- (bmp-header-file-sz hd)
                                      (bmp-header-raster-data-offset hd)))
    (let* ((channels (floor (bmp-header-image-sz hd)
                            (bmp-pixels hd)))
           (bit-depth (/ (bmp-header-bitcount hd) channels))
           (image (make-image (bmp-header-rows hd)
                              (bmp-header-cols hd)
                              channels
                              (if (= 16 bit-depth) 16 8)))
           (np (mod (bmp-header-cols hd) 4))
           (npad (when (> np 0) np)))
      (dotimes (row (image-height image))
        (dotimes (col (image-width image))
          (dotimes (chan channels)
            (setf (aref image
                        (if flip (- (image-height image) row 1) row)
                        col
                        chan)
                  (read-byte input))))
        (when npad
          (dotimes (c npad)
            (read-byte input))))
      image)))

(defun decode-bmp-file (pathname &key flip)
  "Reads file PATHNAME, decodes as BMP file and returns IMAGE.

Flips the image vertically if FLIP is set.

The current version only decodes RGB and ARGB BMP files (24 and 32 bit
pixels, respectively). Palletted versions remain to be dealt
with. They aren't terribly common.

Signals an error if reading the image fails."
  (with-open-file (input pathname :element-type '(unsigned-byte 8))
    (decode-bmp input :flip flip)))

(defun encode-bmp (image output &key flip (xppi 72) (yppi 72)
                   xppm yppm (reserve 0))
  "Writes IMAGE in BMP format to OUTPUT.

Flips image vertically if FLIP set.
XPPI and YPPI specify pixels per inch

The current version only encodes RGB and ARGB BMP files (24 and 32 bit
pixels, respectively). Palletted versions remain to be written. They
aren't terribly common.

Signals an error if writing the image fails."
  (check-type image (or rgb-image png::argb-image))
  ;; Notes:
  ;;      reserve: 4-bytes that can be set by caller arbitrarily.
  ;;      
  ;;  compression: 0= BI_RGB (none: most common)
  ;;               1= BI_RLE8 palletized (8-bits/pixel)
  ;;               2= BI_RLE4 palletized (4-bits/pixel)
  ;;               3= BI_BITFIELDS (16,32-bits/pixel bitmaps)
  ;;               4= BI_JPEG (not supported)
  ;;               5= BI_PNG (not supported)
  ;;
  ;;    xppm,yppm: horizontal,vertical resolution in pixels/meter
  ;;
  ;;  colors_used: 0 defaults to 2**n
  ;;  colors_important is generally ignored
  ;;  
  (let ((raster-data-offset 54)
        (sz                 40)
        (planes             1)
        (compression        0)
        (imagesize          0)
        (colors-used        0)
        (colors-important   0))
    ;; Convert the resolution if necessary
    (unless (numberp xppm)
      (setf xppm (floor xppi 0.0254)))
    (unless (numberp yppm)
      (setf yppm (floor yppi 0.0254)))
    ;; Rows of pixels are padded out to 4-byte boundaries, so we have
    ;; to calculate the number of pad bytes
    (let* ((bitcount (* (image-channels image) (image-bit-depth image)))
           ;; (bytes/pixel (bytes-per-pixel image))
           ;; Note: different "pixel"??? RGB=24-bits=3-bytes/pixel, not 1 (jkc)
           (bytes/pixel (/ bitcount 8))
           (bytes/row   (* bytes/pixel (image-width image)))
           (np          (mod bytes/row 4))
           (padbytes    (> np 0))
           (npad        (if padbytes (- 4 np) 0))
           (nbytes      (* (image-height image)
                           (+ npad (* (image-channels image)
                                      (image-width image)))))
           (filesize    (+ nbytes raster-data-offset)))
      ;; Write "BM" signature
      (write-u2le output 19778)
      ;; Write primary header
      (write-u4le output filesize)
      (write-u4le output reserve)
      (write-u4le output raster-data-offset)
      ;; Write DIB header
      (write-u4le output sz)
      (write-u4le output (image-width image))
      (write-u4le output (image-height image))
      (write-u2le output planes)
      (write-u2le output bitcount)
      (write-u4le output compression)
      (write-u4le output imagesize)
      (write-u4le output xppm)
      (write-u4le output yppm)
      (write-u4le output colors-used)
      (write-u4le output colors-important)
      ;; Color table gets written here if bitsperpixel <= 8
      (assert (member bitcount '(24 32) ))
      ;; (Haven't implemented this yet)
#|      
      (format t "~&##xppm=~a~%" xppm)
      (format t "~&##bitcount=~a~%" bitcount)
      (format t "~&##bytes/pixel=~a~%" bytes/pixel)
      (format t "~&##bytes/row=~a~%" bytes/row)
      (format t "~&##npad=~a~%"npad)
      (format t "~&##nbytes=~a~%" nbytes)
      (format t "~&##filesize=~a~%" filesize
|#
      ;; Write raster data
      (dotimes (row (image-height image))
        (dotimes (col (image-width image))
          (dotimes (chan (image-channels image))
            (write-byte (aref image
                              (if flip (- (image-height image) row 1) row)
                              col chan)
                        output)))
        (when padbytes
          (dotimes (c npad)
            (write-byte 0 output)))))))

(defun encode-bmp-file (image pathname &key flip)
  "Encodes IMAGE as BMP and writes to PATHNAME.

Flips image vertically if FLIP set.

See notes on state of current version in ENCODE-BMP.

Signals an error if writing the image fails."
  (with-open-file (output pathname :element-type '(unsigned-byte 8)
			  :direction :output :if-exists :supersede)
    (encode-bmp image output :flip flip)))