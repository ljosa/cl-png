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
  "An IMAGE with element type (UNSIGNED-BYTE 8)."
  `(and (array (unsigned-byte 8) (,height ,width ,channels))
        (satisfies image-displaced-to-buffer-p)))

(deftype 16-bit-image (&optional height width channels)
  "An IMAGE with element type (UNSIGNED-BYTE 16)."
  `(and (array (unsigned-byte 16) (,height ,width ,channels))
	(satisfies image-displaced-to-buffer-p)))

(deftype image (&optional height width channels)
  "A three-dimensional array of (unsigned-byte 8) or (unsigned-byte
16). In the current version, an IMAGE is displaced to a
one-dimensional SIMPLE-ARRAY with the same total number of elements,
but applications should not rely on this implementation detail, as it
is likely to change in future versions. The functions MAKE-IMAGE,
DECODE, COPY-IMAGE, 8-BIT-IMAGE, AND 16-BIT-IMAGE return IMAGEs.

The three dimensions represent row, column, and channel.  In other
words, (aref image i j k) is the intensity in the k-th channel of the
pixel in the i-th row and j-th column of image."
  `(or (8-bit-image ,height ,width ,channels)
       (16-bit-image ,height ,width ,channels)))

(deftype grayscale-image (&optional height width)
  "An IMAGE with one channel."
  `(image ,height ,width 1))

(deftype rgb-image (&optional height width)
  "An IMAGE with three channels."
  `(image ,height ,width 3))

(defun make-image (height width channels &optional bit-depth)
  "Make a new IMAGE of the specified height, width, and number of
channels.  The image will be an 8-bit-image or a 16-bit-image depending
on the value of byte-size.  Makes an 8-BIT-IMAGE if BIT-DEPTH is 8 or
NIL and a 16-BIT-IMAGE if BIT-DEPTH is 16.  The contents of the image
are undefined."
  (make-array (list height width channels) 
	      :element-type (ecase bit-depth
			      ((8 nil) '(unsigned-byte 8))
			      (16 '(unsigned-byte 16)))
	      :displaced-to (make-shareable-byte-vector
			     (* height width channels) (or bit-depth 8))))

(defun image-height (image) 
  "The height of image, i.e., the number of rows."
  (array-dimension image 0))

(defun image-width (image)
  "The width of IMAGE, i.e., the number of columns."
  (array-dimension image 1))

(defun image-channels (image) 
  "The number of channels in IMAGE.  Grayscale images have one
channel, whereas RGB images have three."
  (array-dimension image 2))

(defun image-bit-depth (image)
  "Returns the bit-depth of the image, i.e., the number of bits in the byte representing each sample. The bit depth is 8 or 16, depending on whether the image is an 8-bit-image or a 16-bit-image, respectively."
  (etypecase image
    (8-bit-image 8)
    (16-bit-image 16)))

(defun copy-image (image)
  "Creates a copy of IMAGE. The elements of the new image are the same
as the corresponding elements of IMAGE, and the new image has the same
height, width, number of channels, and bit depth as IMAGE."
  (let ((new (make-image (image-height image) (image-width image)
			 (image-channels image) (image-bit-depth image))))
    (dotimes (i (array-total-size image) new)
      (setf (row-major-aref new i) (row-major-aref image i)))))

(defun 8-bit-image (image)
  "If IMAGE is an 8-BIT-IMAGE, return it or a copy of it.  If IMAGE is
a 16-BIT-IMAGE, return an 8-BIT-IMAGE that has the same width, height,
and number of channels as image, but where each element is the
corresponding element in image divided by 257 and rounded to the
nearest integer.  The effect of this division is to compress the
dynamic range of the image so as to fit within the smaller bit depth."
  (etypecase image
    (8-bit-image image)
    (16-bit-image 
     (let* ((new (make-image (image-height image) (image-width image)
			     (image-channels image) 8))
	    (v16 (array-displacement image))
	    (v8 (array-displacement new)))
       (declare (type (simple-array (unsigned-byte 8) 1) v8)
		(type (simple-array (unsigned-byte 16) 1) v16))
       (dotimes (i (array-total-size v16) new)
	 ;; TODO: Bitfidling may be faster.
	 (setf (aref v8 i) (round (aref v16 i) 257)))))))
    
(defun 16-bit-image (image)
  "If IMAGE is a 16-BIT-IMAGE, return it or a copy of it.  If IMAGE is
an 8-BIT-IMAGE, return a 16-BIT-IMAGE that has the same width, height,
and number of channels as IMAGE, but where each element is the
corresponding element in image multiplied by 257.  The effect of this
multiplication is to stretch the dynamic range of the image to utilize
the increased bit depth."
  (etypecase image
    (16-bit-image image)
    (8-bit-image 
     (let* ((new (make-image (image-height image) (image-width image)
			     (image-channels image) 16))
	    (v8 (array-displacement image))
	    (v16 (array-displacement new)))
       (declare (type (simple-array (unsigned-byte 8) 1) v8)
		(type (simple-array (unsigned-byte 16) 1) v16))
       (dotimes (i (length v8) new)
	 (setf (aref v16 i) (* 257 (aref v8 i))))))))
