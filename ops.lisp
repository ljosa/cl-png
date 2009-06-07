(in-package #:image)

;;; Error handlers

(define-condition mismatched-image-types (error)
  ((types :initarg :types :reader mismatched-image-types-types))
  (:report (lambda (condition stream)
             (format stream "Mismatched image types:~%~{  ~s~%~}"
                     (mismatched-image-types-types condition)))))

(define-condition mismatched-image-sizes (error)
  ((sizes :initarg :sizes :reader mismatched-image-sizes))
  (:report (lambda (condition stream)
             (format stream "Mismatched image sizes:~%~{  ~s~%~}"
                     (mismatched-image-sizes condition)))))

(define-condition mismatched-image-dims ()
  ((dims :initarg :dims :reader mismatched-image-dims-dims))
  (:report (lambda (condition stream)
             (format stream "Mismatched image dims:~%~{  ~s~%~}"
                     (mismatched-image-dims-dims condition)))))

;;; Convenience functions

(defun make-image-like (im)
  "Makes an image the same size and bitdepth as IM."
  (destructuring-bind (nm tp (h w c)) (type-of im)
    (declare (ignorable nm))
	(make-image h w c (second tp))))


;;; Additional IMAGE accessors

(defun size (im)
  "The (height x width) of an image as an alist"
  (cons (array-dimension im 0) (array-dimension im 1)))

(defun dims (im)
  "The dimensions (height x width x channels) of an image as a list"
  (list (array-dimension im 0) (array-dimension im 1) (array-dimension im 2)))


;;; Image setting functions

(defun fillv (im val)
  "Fills entire image with constant value. Value must be a list with
  the same number of elements as IM has channels.

Returns IM. "
  (unless (= (length val) (image-channels im))
    (error "Image channels doesn't match fill dimension: ~s" (length val)))
  (dotimes (i (image-height im) im)
      (dotimes (j (image-width im))
        (dotimes (k (image-channels im))
          (setf (aref im i j k) (nth k val))))))



;;; Scalar-valued functions of images

(defun channel-max (im)
  "Finds the max values over all the pixel channels (separately)
in the image. "
  (let ((mx (make-array (image-channels im) :initial-element 0)))
    (dotimes (i (image-height im) mx)
      (dotimes (j (image-width im))
        (dotimes (k (image-channels im))
          (when (< (aref mx k) (aref im i j k))
            (setf (aref mx k) (aref im i j k))))))))

(defun channel-min (im)
  "Finds the min values over all the pixel channels (separately)
in the image. "
  (let ((mn (make-array (image-channels im) :initial-element 255)))
    (dotimes (i (image-height im) mn)
      (dotimes (j (image-width im))
        (dotimes (k (image-channels im))
          (when (> (aref mn k) (aref im i j k))
            (setf (aref mn k) (aref im i j k))))))))

(defun intensity-max (im)
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


(defun norm2 (im)
  "Calculates the sum of the squared intensities of all the pixels in
  the image. "
  (let ((acc 0))
    (dotimes (i (image-height im) acc)
      (dotimes (j (image-width im))
        (dotimes (k (image-channels im))
          (incf acc (* (aref im i j k) (aref im i j k))))))))


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


(defun flip (old)
  "Returns a new image which is flipped vertically from the old image"
  (let ((new (make-image-like old))
        (m (image-height old)))
    (dotimes (h (image-height new) new)
      (dotimes (w (image-width new))
        (dotimes (c (image-channels new))
          (setf (aref new h w c) (aref old (- m h 1) w c)))))))


(defun mirror (old)
  "Returns a new image which is mirrored horizontally from the old
image"
  (let ((new (make-image-like old))
        (m (image-width old)))
    (dotimes (h (image-height new) new)
      (dotimes (w (image-width new))
        (dotimes (c (image-channels new))
          (setf (aref new h w c) (aref old h (- m w 1) c)))))))


;;; Image-valued operations on multiple images

(defun sub (im1 im2)
  "Subtracts image IM2 from image IM1 and returns the resulting image
difference without modifying either IM1 or IM2. Both images must be
the same type and size.

Clips pixel intensity to 0 when necessary. "
  (unless (equalp (type-of im1) (type-of im2))
    (error 'mismatched-image-types :types (list (type-of im1) (type-of im2))))
  (let ((new (make-image-like im1)))
        (dotimes (i (image-height new) new)
          (dotimes (j (image-width new))
            (dotimes (k (image-channels new))
              (setf (aref new i j k)
                    (if (> (aref im1 i j k) (aref im2 i j k))
                        (-  (aref im1 i j k) (aref im2 i j k))
                        0)))))))

(defun sub* (im1 im2)
  "Destructively subtracts image IM2 from image IM1, leaving the
resulting image difference in im1. Both images must be the same type
and size.

Clips pixel intensity to 0 when necessary. "
  (unless (equalp (type-of im1) (type-of im2))
    (error 'mismatched-image-types :types (list (type-of im1) (type-of im2))))
  (dotimes (i (image-height im1) im1)
	(dotimes (j (image-width im1))
	  (dotimes (k (image-channels im1))
        (if (> (aref im1 i j k) (aref im2 i j k))
            (decf (aref im1 i j k) (aref im2 i j k))
            (setf (aref im1 i j k) 0))))))


(defun add (im1 im2)
  "Adds image IM2 from image IM1 and returns the resulting image sum
without modifying either IM1 or IM2. Both images must be the same type
and size.

Clips to maximum intensity in each channel if exceeded. "
  (unless (equalp (type-of im1) (type-of im2))
    (error 'mismatched-image-types :types (list (type-of im1) (type-of im2))))
  (let ((lim (1- (expt 2 (image-bit-depth im1))))
        (new (make-image-like im1)))
        (dotimes (i (image-height new) new)
          (dotimes (j (image-width new))
            (dotimes (k (image-channels new))
              (let ((sum (+ (coerce (aref im1 i j k) '(unsigned-byte 16))
                            (coerce (aref im2 i j k) '(unsigned-byte 16)))))
                (setf (aref new i j k) (if (> sum lim) lim sum))))))))


(defun add* (im1 im2)
  "Destructively adds image IM2 from image IM1, leaving the resulting
image sum in im1. Both images must be the same type and size.
Clips to maximum intensity in each channel if exceeded"
  (unless (equalp (type-of im1) (type-of im2))
    (error 'mismatched-image-types :types (list (type-of im1) (type-of im2))))
  (let ((lim (1- (expt 2 (image-bit-depth im1)))))
    (dotimes (i (image-height im1) im1)
      (dotimes (j (image-width im1))
        (dotimes (k (image-channels im1))
          (if (< (aref im1 i j k) (- lim (aref im2 i j k)))
              (incf (aref im1 i j k) (aref im2 i j k))
              (setf (aref im1 i j k) lim)))))))

