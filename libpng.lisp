;;; Bugs:
;;;  * Needs unit tests.
;;;  * Endianness may be wrong?  See png_set_swap.
;;;  * Does not handle bit depths 3, 5-7; see png_set_shift.
;;;  * Reconsider bit depth philosophy of image type.

(in-package #:png)

(defun image-displaced-to-buffer-p (image)
  (multiple-value-bind (displacement index) (array-displacement image)
    (and displacement
	 (zerop index)
	 (typep (array-displacement image) 
		(list 'simple-array '(unsigned-byte 8)
		      (list (reduce #'* (array-dimensions image))))))))
  
(deftype image (&optional height width channels)
  "An image is a three-dimensional array of (unsigned-byte 8)
displaced to a one-dimensional simple-array with the same total number
of elements."
  `(and (array (unsigned-byte 8) (,height ,width ,channels))
	(satisfies image-displaced-to-buffer-p)))

(deftype grayscale-image (&optional height width)
  "An IMAGE with one channel."
  `(image ,height ,width 1))

(deftype rgb-image (&optional height width)
  "An IMAGE with three channels."
  `(image ,height ,width 3))

(defun make-image (height width channels)
  "Make a new image of the specified size, with undefined contents."
  (make-array (list height width channels) :element-type '(unsigned-byte 8)
	      :displaced-to (cffi-sys:make-shareable-byte-vector
			     (* height width channels))))

(defun image-height (image) 
  "Return the height of image, i.e., the number of rows."
  (array-dimension image 0))

(defun image-width (image)
  "Return the width of IMAGE, i.e., the number of columns."
  (array-dimension image 1))

(defun image-channels (image) 
  "Return the number of channels in IMAGE."
  (array-dimension image 2))


(define-foreign-library libpng
  (:unix (:or "libpng12.0.dylib" "libpng12.so.0"))
  (t (:default "libpng")))

(use-foreign-library libpng)

(defcfun "png_access_version_number" :uint32)

(defconstant +png-libpng-ver-string+ (symbol-name '|1.2.26|))

(defcfun "png_create_read_struct" :pointer
  (user-png-ver :string)
  (error-ptr :pointer)
  (error-fn :pointer)
  (warn-fn :pointer))

(defcfun "png_destroy_read_struct" :void
  (png-ptr-ptr :pointer)
  (info-ptr-ptr :pointer)
  (end-info-ptr-ptr :pointer))

(defcfun "png_create_write_struct" :pointer
  (user-png-ver :string)
  (error-ptr :pointer)
  (error-fn :pointer)
  (warn-fn :pointer))

(defcfun "png_destroy_write_struct" :void
  (png-ptr-ptr :pointer)
  (info-ptr-ptr :pointer))

(defcfun "png_create_info_struct" :pointer
  (png-ptr :pointer))

(defcfun "png_destroy_info_struct" :void
  (png-ptr :pointer)
  (info-ptr-ptr :pointer))

(defcfun "png_init_io" :void
  (png-ptr :pointer)
  (file :pointer))

(defcfun "png_read_info" :void
  (png-ptr :pointer)
  (info-ptr :pointer))

(defcfun "png_read_png" :void
  (png-ptr :pointer)
  (info-ptr :pointer)
  (png-transforms :int)
  (params :pointer))

(defcfun "png_get_IHDR" :uint32
  (png-ptr :pointer)
  (info-ptr :pointer)
  (width-uint32-ptr :pointer)
  (height-uint32-ptr :pointer)
  (bit-depth-int-ptr :pointer)
  (color-type-int-ptr :pointer)
  (interlace-type-int-ptr :pointer)
  (compression-type-int-ptr :pointer)
  (filter-type-int-ptr :pointer))

(defcfun "png_set_IHDR" :void
  (png-ptr :pointer)
  (info-ptr :pointer)
  (width :uint32)
  (height :uint32)
  (bit-depth :int)
  (color-type :int)
  (interlace-type :int)
  (compression-type :int)
  (filter-type :int))

(defcfun "png_set_palette_to_rgb" :void
  (png-ptr :pointer))

(defcfun "png_set_expand_gray_1_2_4_to_8" :void
  (png-ptr :pointer))

(defcfun "png_get_valid" :uint32
  (png-ptr :pointer)
  (info-ptr :pointer)
  (flag :uint32))

(defcfun "png_set_tRNS_to_alpha" :void
  (png-ptr :pointer))

(defcfun "png_set_strip_16" :void
  (png-ptr :pointer))

(defcfun "png_set_strip_alpha" :void
  (png-ptr :pointer))

(defcfun "png_set_packing" :void
  (png-ptr :pointer))

(defcfun "png_get_rows" :pointer
  (png-ptr :pointer)
  (info-ptr :pointer))

(defcfun "png_set_rows" :void
  (png-ptr :pointer)
  (info-ptr :pointer)
  (row-pointers :pointer))

(defcfun "png_read_image" :void
  (png-ptr :pointer)
  (row-pointers :pointer))

(defcfun "png_write_png" :void
  (png-ptr :pointer)
  (info-ptr :pointer)
  (transforms :int)
  (params :pointer))

(defcallback error-fn :void ((png-structp :pointer) (message :string))
  (declare (ignore png-structp))
  (error message))

(defcallback warn-fn :void ((png-structp :pointer) (message :string))
  (declare (ignore png-structp))
  (error message))

(defmacro with-png-struct ((var &key (direction :input)) &body body)
  (let ((pointer (gensym "POINTER")))
    `(let ((,var (,(ecase direction
			  (:input 'png-create-read-struct)
			  (:output 'png-create-write-struct))
			  +png-libpng-ver-string+ (null-pointer)
			  (callback error-fn) (callback warn-fn))))
       (with-foreign-pointer (,pointer (foreign-type-size :pointer))
	 (setf (mem-ref ,pointer :int) (pointer-address ,var))
	 (unwind-protect (progn ,@body)
	   ,(ecase direction
	      (:input `(png-destroy-read-struct ,pointer (null-pointer) 
						(null-pointer)))
	      (:output `(png-destroy-write-struct ,pointer 
						  (null-pointer)))))))))

(defmacro with-png-info-struct ((var png-struct initform) &body body)
  (let ((pointer (gensym "POINTER")))
    `(let ((,var ,initform))
       (with-foreign-pointer (,pointer (foreign-type-size :pointer))
	 (setf (mem-ref ,pointer :int) (pointer-address ,var))
	 (unwind-protect (progn ,@body)
	   (png-destroy-info-struct ,png-struct ,pointer))))))

(defun get-ihdr (png-ptr info-ptr)
  (with-foreign-pointer (width (foreign-type-size :uint32))
    (with-foreign-pointer (height (foreign-type-size :uint32))
      (with-foreign-pointer (bit-depth (foreign-type-size :int))
	(with-foreign-pointer (color-type (foreign-type-size :int))
	  (png-get-ihdr png-ptr info-ptr width height bit-depth
			color-type (null-pointer) (null-pointer)
			(null-pointer))
	  (values (mem-ref width :uint32) (mem-ref height :uint32)
		  (mem-ref bit-depth :int) (mem-ref color-type :int)))))))

(defmacro with-row-pointers ((rows-ptr image)
			     &body body)
  (let ((row-pointers (gensym "ROW-POINTERS"))
	(raw-data (gensym "RAW-DATA"))
	(i (gensym "I"))
	(buffer (gensym "BUFFER")))
    `(let ((,row-pointers (cffi-sys:make-shareable-byte-vector
			   (* (image-height ,image)
			      (foreign-type-size :pointer))))
	   (,buffer (array-displacement ,image)))
       (with-pointer-to-vector-data (,rows-ptr ,row-pointers)
	 (with-pointer-to-vector-data (,raw-data ,buffer)
	   (dotimes (,i (image-height ,image))
	     (setf (mem-aref ,rows-ptr :pointer ,i) 
		   (inc-pointer ,raw-data (* ,i (image-width ,image)
					     (image-channels ,image)))))
	   ,@body)))))

(defun grayp (color-type)
  (zerop (logand color-type (lognot +png-color-mask-alpha+))))

(defun decode (stream)
  "Read a PNG image from STREAM and return it as an array of type IMAGE."
  (with-png-struct (png-ptr :direction :input)
    (with-png-info-struct (info-ptr png-ptr (png-create-info-struct png-ptr))
      (with-png-info-struct (end-ptr png-ptr (png-create-info-struct png-ptr))
	(with-file (file stream "rb")
	  (png-init-io png-ptr file)
	  (png-read-info png-ptr info-ptr)
	  (multiple-value-bind (width height bit-depth color-type)
	      (get-ihdr png-ptr info-ptr)
	    (when (= color-type +png-color-type-palette+)
	      (png-set-palette-to-rgb png-ptr))
	    (png-set-packing png-ptr)
	    (when (= bit-depth 16)
	      (png-set-strip-16 png-ptr))
	    (unless (zerop (logand color-type +png-color-mask-alpha+))
	      (png-set-strip-alpha png-ptr))
	    (let ((image (make-image height width
				     (if (grayp color-type) 1 3))))
	      (with-row-pointers (row-pointers image)
		(png-set-rows png-ptr info-ptr row-pointers)
		(png-read-image png-ptr row-pointers))
	      image)))))))

(defun decode-file (pathname)
  "Open the specified file and call DECODE on it."
  (with-open-file (input pathname)
    (decode input)))

(defun encode (image output &optional (bit-depth 8))
  "Write an image to a stream in PNG format."
  (check-type image (or grayscale-image rgb-image))
  (with-png-struct (png-ptr :direction :output)
    (with-png-info-struct (info-ptr png-ptr (png-create-info-struct png-ptr))
      (with-file (file output "wb")
 	(png-init-io png-ptr file)
 	(png-set-ihdr png-ptr info-ptr (image-width image) (image-height image)
 		      bit-depth (if (= (image-channels image) 1)
				    +png-color-type-gray+
				    +png-color-type-rgb+)
 		      +png-interlace-none+ +png-compression-type-default+
 		      +png-filter-type-default+)
 	(with-row-pointers (row-pointers image)
 	  (png-set-rows png-ptr info-ptr row-pointers)
 	  (png-write-png png-ptr info-ptr +png-transform-packing+ 
 			 (null-pointer))))))
   t)


(defun encode-file (image pathname &optional (bit-depth 8))
  "Open the specified file and use ENCODE to write the specified image
to it."
  (with-open-file (output pathname :direction :output :if-exists :supersede)
    (encode image output bit-depth)))

;;;; Testing.

(defun write-image-as-pnm (image filename)
  (with-open-file (output filename :direction :output :if-exists :supersede
			  :element-type '(unsigned-byte 8))
    (write-sequence (map 'vector #'char-code 
			 (format nil "~A~%~D ~D~%~D~%" 
				 (if (= 1 (array-dimension image 2)) "P5" "P6")
				 (array-dimension image 1)
				 (array-dimension image 0)
				 (1- (expt 2 8))))
		    output)
    (let ((buffer (make-array (reduce #'* (array-dimensions image))
			      :element-type '(unsigned-byte 8)
			      :displaced-to image)))
      (write-sequence buffer output)))
  t)

(defvar *image*)

(defun test-decode (&optional (input "/Users/ljosa/research/systbio/rp_ld_example.png") (output "/tmp/foo.pnm"))
  (defparameter *image* (with-open-file (stream input)
			  (decode stream)))
  (write-image-as-pnm *image* output))

(defun test-encode (&optional (input-filename "/Users/ljosa/research/systbio/rp_ld_example.png") (output-filename "/tmp/foo.png"))
  (with-open-file (output output-filename :direction :output 
			  :if-exists :supersede)
    (encode (with-open-file (input input-filename)
	      (decode input))
	    output)))

(defun test-read-pngsuite ()
  (dolist (pathname (directory "/Users/ljosa/tmp/PngSuite/*.png"))
    (unless (equal (pathname-name pathname) "pngsuite_logo")
      (format t "~A " (pathname-name pathname))
      (flet ((report (error)
	       (princ (if (eq (and error t)
			      (equal #\x (char (pathname-name pathname) 0)))
			  "OK  "
			  "FAIL - "))
	       (when error
		 (princ error))
	       (terpri)))
	(handler-case 
	    (with-open-file (stream pathname)
	      (let ((im (decode stream)))
		(unless (= (image-channels im)
			   (ecase (digit-char-p (char (pathname-name pathname)
						      4))
			     ((0 1 4) 1)
			     ((2 3 6) 3)))
		  (error "Unexpected number of channels: ~D" 
			 (image-channels im))))
	      (report nil))
	  (error (e)
	    (report e)))))))

(defun rotate (input-pathname output-pathname)
  "Read a PNG image, rotate it 90 degrees, and write it to a new file."
  (let* ((old (with-open-file (input input-pathname)
		(png:decode input)))
	 (new (png:make-image (png:image-width old)
			      (png:image-height old)
			      (png:image-channels old)))
	 (m (png:image-width old)))
      (dotimes (i (png:image-height new))
	(dotimes (j (png:image-width new))
	  (dotimes (k (png:image-channels new))
	    (setf (aref new i j k) (aref old j (- m i 1) k)))))
      (with-open-file (output output-pathname :direction :output
			      :if-exists :supersede)
	(png:encode new output))))

(defun grayscale (image)
  (let* ((h (image-height image))
	 (w (image-width image))
	 (c (image-channels image))
	 (gray (make-image h w 1)))
    (dotimes (j w gray)
      (dotimes (i h)
	(setf (aref gray i j 0) (floor (loop for k below c
					  sum (aref image i j k)) c))))))

;; 4.7 s per 8-bit image encoded; this is the same as time-encode.c.
;; The logior increases this to 5.8 s.
(defun time-encode ()
  (let ((im (decode-file "/Users/ljosa/src/cl-png/trunk/Fall.png")))
      (time
       (dotimes (i 10)
	 (princ i)
	 (terpri)
	 (with-open-file (output "/tmp/foo.png" :direction :output
				 :if-exists :overwrite)
	   (encode im output (bit-depth im)))))))

(defun nstrip (im bits)
  (dotimes (i (array-total-size im))
    (setf (row-major-aref im i) 
	  (ldb (byte (- 8 bits) bits) (row-major-aref im i)))))

(defun bit-depth (im)
  (let ((x (reduce #'logior (array-displacement im)))
	(bits 8))
    (loop 
       while (and (not (zerop bits))
		  (zerop (ldb (byte 1 (1- bits)) x)))
       do (decf bits))
    bits))