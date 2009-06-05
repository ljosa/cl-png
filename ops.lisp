(in-package #:image)

;;; Conditions.

(defun check-image-size-and-depth (image-a image-b)
  (unless (equalp (type-of image-a) (type-of image-b))
    (error "Images must be of the same size and bit depth.")))


;;; Additional IMAGE accessors

(defun image-size (im)
  "The (height x width) of an image as an alist"
  (cons (array-dimension im 0) (array-dimension im 1)))

(defun image-dims (image)
  "The dimensions (height x width x channels) of an image as a list"
  (array-dimensions image))


;;; Convenience functions

(defun make-image-like (image)
  "Makes an image the same size and bitdepth as IM."
  (make-image (image-height image) (image-width image) (image-channels image)
	      (image-bit-depth image)))


;;; Image setting functions

(defun image-fill (image value)
  "Fills entire image with a constant value.  VALUE must be a sequence with
  the same number of elements as IM has channels.   Returns IMAGE."
  (unless (= (length value) (image-channels image))
    (error "VALUE must be a sequence of length ~D." (image-channels image)))
  (dotimes (k (image-channels image) image)
    (let ((v (elt value k)))
      (dotimes (i (image-height image))
	(dotimes (j (image-width image))
	  (setf (aref image i j k) v))))))



;;; Scalar-valued functions of images

(defun image-channel-max (im)
  "Finds the max values over all the pixel channels (separately)
in the image. "
  (let ((mx (make-array (image-channels im) :initial-element 0)))
    (dotimes (i (image-height im) mx)
      (dotimes (j (image-width im))
        (dotimes (k (image-channels im))
          (when (< (aref mx k) (aref im i j k))
            (setf (aref mx k) (aref im i j k))))))))


(defun image-max (im)
  "Finds the max intensities over all the pixels (sum of all the
channels) in the image. "
  (let ((mx 0))
    (dotimes (i (image-height im) mx)
      (dotimes (j (image-width im))
        (let ((acc 0))
          (dotimes (k (image-channels im))
            (incf acc (aref im i j k)))
          (when (< mx acc)
            (setf mx acc)))))))


(defun image-norm2 (im)
  "Calculates the sum of the squared intensities of all the pixels in
  the image. "
  (flet ((square (x)
	   (* x x)))
    (loop
       for i below (array-total-size im)
       sum (square (row-major-aref im i)))))


;;; Image-valued operations on single images

(defun rotate (old)
  "Returns a new image which is rotated counter-clockwise 90-degrees
from the old image"
  (let* ((new (make-image (image-width old) (image-height old)
                          (image-channels old) (image-bit-depth old)))
         (m (image-width old)))
    (dotimes (i (image-height new) new)
      (dotimes (j (image-width new))
        (dotimes (k (image-channels new))
          (setf (aref new i j k) (aref old j (- m i 1) k)))))))


;;; Image-valued operations on multiple images

(defun image-sub (im1 im2)
  "Subtracts image IM2 from image IM1 and returns the resulting image
difference without modifying either IM1 or IM2. Both images must be
the same type and size.  Clips pixel intensity to 0 when necessary. "
  (check-image-size-and-depth im1 im2)
  (let ((new (make-image-like im1)))
        (dotimes (i (image-height new) new)
          (dotimes (j (image-width new))
            (dotimes (k (image-channels new))
              (setf (aref new i j k)
                    (if (> (aref im1 i j k) (aref im2 i j k))
                        (-  (aref im1 i j k) (aref im2 i j k))
                        0)))))))

(defun image-nsub (im1 im2)
  "Destructively subtracts image IM2 from image IM1, leaving the
resulting image difference in im1. Both images must be the same type
and size.  Clips pixel intensity to 0 when necessary. "
  (check-image-size-and-depth im1 im2)
  (dotimes (i (image-height im1) im1)
	(dotimes (j (image-width im1))
	  (dotimes (k (image-channels im1))
        (if (> (aref im1 i j k) (aref im2 i j k))
            (decf (aref im1 i j k) (aref im2 i j k))
            (setf (aref im1 i j k) 0))))))


(defun image-add (im1 im2)
  "Adds image IM2 from image IM1 and returns the resulting image sum
without modifying either IM1 or IM2. Both images must be the same type
and size.  Clips to maximum intensity in each channel if exceeded. "
  (check-image-size-and-depth im1 im2)
  (let ((lim (1- (expt 2 (image-bit-depth im1))))
        (new (make-image-like im1)))
        (dotimes (i (image-height new) new)
          (dotimes (j (image-width new))
            (dotimes (k (image-channels new))
              (let ((sum (+ (coerce (aref im1 i j k) '(unsigned-byte 16))
                            (coerce (aref im2 i j k) '(unsigned-byte 16)))))
                (setf (aref new i j k) (if (> sum lim) lim sum))))))))


(defun image-nadd (im1 im2)
  "Destructively adds image IM2 from image IM1, leaving the resulting
image sum in im1. Both images must be the same type and size.
Clips to maximum intensity in each channel if exceeded"
  (check-image-size-and-depth im1 im2)
  (let ((lim (1- (expt 2 (image-bit-depth im1)))))
    (dotimes (i (image-height im1) im1)
      (dotimes (j (image-width im1))
        (dotimes (k (image-channels im1))
          (if (< (aref im1 i j k) (- lim (aref im2 i j k)))
              (incf (aref im1 i j k) (aref im2 i j k))
              (setf (aref im1 i j k) lim)))))))

