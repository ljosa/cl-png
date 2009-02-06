;;; TODO:
;;; * The displacement messes up the type inference, so making image
;;;   operations efficient is a pain.  Should see if I can just pass
;;;   the 3-D array to the C functions.

(in-package #:png)


(defun image-displaced-to-buffer-p (image)
  (multiple-value-bind (displacement index) (array-displacement image)
    (and displacement
        (zerop index)
        (typep (array-displacement image) 
               (list #+allegro 'array
                     #-allegro 'simple-array
                     (array-element-type image)
                     (list (reduce #'* (array-dimensions image))))))))
  
(deftype 8-bit-image (&optional height width channels)
  "An 8-bit-image is a three-dimensional array of (unsigned-byte 8)
displaced to a one-dimensional array with the same total number of
elements."
  `(and (array (unsigned-byte 8) (,height ,width ,channels))
        (satisfies image-displaced-to-buffer-p)))

(deftype 16-bit-image (&optional height width channels)
  "A 16-bit-image is a three-dimensional array of (unsigned-byte 16)
displaced to a one-dimensional array with the same total number of
elements."
  `(and (array (unsigned-byte 16) (,height ,width ,channels))
	(satisfies image-displaced-to-buffer-p)))

(deftype image (&optional height width channels)
  "An image is a three-dimensional array of (unsigned-byte 8)
or (unsigned-byte 16) displaced to a one-dimensional array with the
same number of elements."
  `(or (8-bit-image ,height ,width ,channels)
       (16-bit-image ,height ,width ,channels)))

(deftype grayscale-image (&optional height width)
  "An IMAGE with one channel."
  `(image ,height ,width 1))

(deftype rgb-image (&optional height width)
  "An IMAGE with three channels."
  `(image ,height ,width 3))

#+lispworks
(defun make-shareable-byte-vector (size &optional (byte-size 8))
  (sys:in-static-area
   (make-array size :element-type (list 'unsigned-byte byte-size))))

#+allegro
(defun make-shareable-byte-vector (size &optional (byte-size 8))
  (make-array size :element-type (list 'unsigned-byte byte-size)
	      :allocation :static-reclaimable))

#-(or lispworks allegro)
(defun make-shareable-byte-vector (size &optional (byte-size 8))
   (make-array size :element-type (list 'unsigned-byte byte-size)))

(defun make-image (height width channels &optional bit-depth)
  "Make a new IMAGE of the specified size, with undefined contents.
Makes an 8-BIT-IMAGE if BIT-DEPTH is 8 or NIL and a 16-BIT-IMAGE if
BIT-DEPTH is 16."
  (make-array (list height width channels) 
	      :element-type (ecase bit-depth
			      ((8 nil) '(unsigned-byte 8))
			      (16 '(unsigned-byte 16)))
	      :displaced-to (make-shareable-byte-vector
			     (* height width channels) (or bit-depth 8))))

(defun image-height (image) 
  "Return the height of image, i.e., the number of rows."
  (array-dimension image 0))

(defun image-width (image)
  "Return the width of IMAGE, i.e., the number of columns."
  (array-dimension image 1))

(defun image-channels (image) 
  "Return the number of channels in IMAGE."
  (array-dimension image 2))

(defun image-bit-depth (image)
  (etypecase image
    (8-bit-image 8)
    (16-bit-image 16)))

(defun copy-image (image)
  (let ((new (make-image (image-height image) (image-width image)
			 (image-channels image) (image-bit-depth image))))
    (dotimes (i (array-total-size image) new)
      (setf (row-major-aref new i) (row-major-aref image i)))))

(defun 8-bit-image (image)
  "Return a copy of image, converted to an 8-BIT-IMAGE.
If IMAGE is a 16-BIT-IMAGE, each sample will be divided by 257 and
rounded to the nearst integer."
  (etypecase image
    (8-bit-image (copy-image image))
    (16-bit-image 
     (let ((new (make-image (image-height image) (image-width image)
			    (image-channels image) 8)))
       (dotimes (i (array-total-size image) new)
	 ;; TODO: Bitfidling may be faster.
	 (setf (row-major-aref new i) (round (row-major-aref image i)
					     257)))))))
    
(defun 16-bit-image (image)
  "Return a copy of image, converted to a 16-BIT-IMAGE.
If IMAGE is an 8-BIT-IMAGE, each sample will be multiplied by 257, so
as to utilize the entire dynamic range of the 16-BIT-IMAGE."
  (etypecase image
    (16-bit-image (copy-image image))
    (8-bit-image 
     (let ((new (make-image (image-height image) (image-width image)
			    (image-channels image) 16)))
       (dotimes (i (array-total-size image) new)
	 ;; TODO: DPB may be faster.
	 (setf (row-major-aref new i) (* 257 (row-major-aref image i))))))))

#||
(defun 16-bit-image-2 (image)
  (etypecase image
    (16-bit-image (copy-image image))
    (8-bit-image 
     (let* ((new (make-image (image-height image) (image-width image)
			     (image-channels image) 16))
	    (image-simple (array-displacement image))
	    (new-simple (array-displacement new)))
       (declare (type (simple-array (unsigned-byte 8) (*)) image-simple)
		(type (simple-array (unsigned-byte 16) (*)) new-simple))
       (dotimes (i (array-total-size image) new)
	 ;; TODO: DPB may be faster.
	 (setf (aref new-simple i) (* 257 (aref image-simple i))))))))

(defun 16-bit-image-3 (image)
  "Return a copy of image, converted to a 16-BIT-IMAGE.
If IMAGE is an 8-BIT-IMAGE, each sample will be multiplied by 257, so
as to utilize the entire dynamic range of the 16-BIT-IMAGE."
  (etypecase image
    (16-bit-image (copy-image image))
    (8-bit-image 
     (let ((new (make-image (image-height image) (image-width image)
			    (image-channels image) 16)))
       (declare (type 8-bit-image image)
		(type 16-bit-image new))
       (dotimes (i (array-total-size image) new)
	 ;; TODO: DPB may be faster.
	 (setf (row-major-aref new i) (* 257 (row-major-aref image i))))))))

(defun new (image)
  (make-image (image-height image) (image-width image)
	      (image-channels image) 16))

(defun 16-bit-image-1 (image new n)
  (declare (type (simple-array (unsigned-byte 8) (*)) image)
	   (type (simple-array (unsigned-byte 16) (*)) new)
	   (type fixnum n))
  (dotimes (i n new)
    (setf (row-major-aref new i) (* 257 (the (unsigned-byte 8) (row-major-aref image i))))))
||#
