
(in-package #:cl-user)

(asdf:oos 'asdf:load-op '#:cffi)

(asdf:oos 'asdf:load-op '#:verrazano-support)

(load "libpng")

(cffi:load-foreign-library "/sw/lib/libpng.3.dylib")

(cffi:defcfun fopen :pointer (filename :string) (mode :string))
(cffi:defcfun fclose :void (file :pointer))

(defmacro with-open-c-file ((file-var filename &optional (mode "r")) 
			    &body body)
  `(let ((,file-var (fopen ,filename ,mode)))
    (when (cffi:null-pointer-p ,file-var)
      (error "Failed to open file ~S." ,filename))
    (unwind-protect
	 (progn
	   ,@body)
      (fclose ,file-var))))

(defmacro with-png-structs ((png-var info-var) &body body) 
  `(let ((,png-var (cffi:null-pointer))
	 (,info-var (cffi:null-pointer)))
    (setf ,png-var (cffi:with-foreign-string (s "1.2.8")
		     (libpng::png-create-read-struct 
		      s (cffi:null-pointer) (cffi:null-pointer) 
		      (cffi:null-pointer))))
    (when (cffi:null-pointer-p ,png-var)
      (error "Unable to allocate PNG read struct x."))
    (unwind-protect
	 (progn
	   (setf ,info-var (libpng::png-create-info-struct ,png-var))
	   (when (cffi:null-pointer-p ,info-var)
	     (error "Unable to allocate PNG info struct."))
	   ,@body)
      "")))

(defun read-png (filename)
  (with-open-c-file (file-pointer filename)
    (with-png-structs (png-struct info-struct)
      (libpng:png-init-io png-struct file-pointer)
      (libpng:png-read-png png-struct info-struct 
			      (logior libpng:png-transform-strip-16
				      libpng:png-transform-strip-alpha
				      libpng:png-transform-packing)
			      (cffi:null-pointer))
      (let ((row-pointers (libpng:png-get-rows png-struct info-struct))
	    (height (libpng:png-get-image-height png-struct info-struct))
	    (width (libpng:png-get-image-width png-struct info-struct)))
	(let ((image (make-array (list height width))))
	  (dotimes (i height)
	    (let ((row (cffi:mem-aref row-pointers 'libpng:png-bytep i)))
	      (dotimes (j width)
		(let ((red (cffi:mem-aref row 'libpng:png-byte 
					  (+ (* j 3) 0)))
		      (green (cffi:mem-aref row 'libpng:png-byte
					    (+ (* j 3) 1)))
		      (blue (cffi:mem-aref row 'libpng:png-byte 
					   (+ (* j 3) 2))))
		  (setf (ldb (byte 8 16) (aref image i j)) red
			(ldb (byte 8 8) (aref image i j)) green
			(ldb (byte 8 0) (aref image i j)) blue)))))
	  image)))))

(defun write-ppm (output-filename image)
  (with-open-file (stream output-filename :direction :output 
			  :if-exists :supersede :element-type 'unsigned-byte)
    (let ((string (with-output-to-string (text-stream)
		    (format text-stream "P6~%~D ~D~%~D~%" 
			    (array-dimension image 1)
			    (array-dimension image 0) 255))))
      (loop for ch across string
	    do (write-byte (char-code ch) stream)))
    (dotimes (i (array-dimension image 0))
      (dotimes (j (array-dimension image 1))
	(let ((num (aref image i j)))
	  (write-byte (ldb (byte 8 16) num) stream)
	  (write-byte (ldb (byte 8 8) num) stream)
	  (write-byte (ldb (byte 8 0) num) stream))))))

(defun equal-file-contents (filename-1 filename-2)
  (with-open-file (stream-1 filename-1 :element-type 'unsigned-byte)
    (with-open-file (stream-2 filename-2 :element-type 'unsigned-byte)
      (loop
       for byte = (read-byte stream-1 nil nil)
       while byte
       unless (= byte (read-byte stream-2 nil nil))
       do (return nil))
      (when (null (read-byte stream-2 nil nil))
	t))))

(defun image-equal (image-1 image-2)
  (let ((match t))
    (unless (equal (array-dimensions image-1) (array-dimensions image-2))
      (setf match nil))
    (dotimes (i (array-dimension image-1 0))
      (dotimes (j (array-dimension image-1 1))
	(unless (= (aref image-1 i j) (aref image-2 i j))
	  (setf match nil))))
    match))
  
(defun test-pngsuite (pngsuite-directory &key verbose)
  (let* ((filenames (directory (make-pathname :name :wild :type "png" 
					      :directory pngsuite-directory)))
	 (decoded-count 0)
	 (equal-count 0))
    (dolist (filename filenames)
      (ignore-errors
	(let ((mine (png:decode-file filename))
	      (theirs (read-png (namestring filename))))
	  (incf decoded-count)
	  (write-ppm (make-pathname :type "tmp" :defaults filename) mine)
	  (cond ((image-equal mine theirs) (incf equal-count))
		(verbose (format t "~&~A was decoded incorrectly.&" 
				 filename))))))
    (format t "~&~3D images decoded.~&" decoded-count)
    (format t "~&~3D images decoded correctly.~&" equal-count)
    equal-count))
