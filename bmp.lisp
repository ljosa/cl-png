(in-package #:png)

(defun read-u2le (in)
  "Read 2 unsigned BYTES in little-endian from IN stream. This version
does not test for end of file."
  (let ((u2 0))
    (setf (ldb (byte 8 0) u2) (read-byte in))
    (setf (ldb (byte 8 8) u2) (read-byte in))
    u2))

(defun read-u4le (in)
  "Read 4 unsigned BYTES in little-endian from IN stream. This version
does not test for end of file."
  (let ((u 0))
    (setf (ldb (byte 8  0) u) (read-byte in))
    (setf (ldb (byte 8  8) u) (read-byte in))
    (setf (ldb (byte 8 16) u) (read-byte in))
    (setf (ldb (byte 8 24) u) (read-byte in))
    u))

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
16. The image will be vertically flipped if FLIP is not NULL.

Signals an error if reading the image fails."
  ;; Check for "BM" signature at beginning
  (unless (= 19778 (read-u2le input))
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
    ;; Note: one cannot always count on the image-sz being set
    ;; in a BMP file, but it can be calculated from the filesize and
    ;; the raster-data-offset, both of which must be.
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
  "Function reads BMP file from PATHNAME and returns an IMAGE. Flips
the image vertically if FLIP not NULL. "
  (with-open-file (input pathname :element-type '(unsigned-byte 8))
    (decode-bmp input :flip flip)))


