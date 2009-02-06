(defpackage #:png-test
  (:use #:common-lisp #:lisp-unit #:png)
  (:export #:run-tests))

(in-package #:png-test)

(defparameter *pngsuite-pathname* (merge-pathnames "PngSuite/"
						   *load-truename*))

(defun decode-pngsuite (basename)
  (let ((pathname (merge-pathnames (make-pathname :name basename :type "png")
				   *pngsuite-pathname*)))
    (with-open-file (input pathname :element-type '(unsigned-byte 8))
      (decode input))))

(defun encode-decode (im)
  (ignore-errors (delete-file "tmp.png"))
  (png::encode-file im "tmp.png")
  (prog1
      (png::decode-file "tmp.png")
    (delete-file "tmp.png")))

(define-test make-image
  (let ((i8 (make-image 2 4 3))
	(i16 (make-image 2 4 3 16)))
    (assert-true (typep i8 '8-bit-image)) 
    (assert-false (typep i8 '16-bit-image))
    (assert-true (typep i16 '16-bit-image)) 
    (assert-false (typep i16 '8-bit-image))
    (assert-true (typep i8 'image)) 
    (assert-true (typep i8 'rgb-image))
    (assert-true (typep i16 'image))
    (assert-true (typep i16 'rgb-image))
    (assert-equal '(2 4 3) (array-dimensions i8))))

(define-test decode-8-bit
  (let ((im (decode-pngsuite "basn0g08")))
    (assert-true (typep im '8-bit-image))
    (assert-equal 0 (aref im 0 0 0))
    (assert-equal 255 (aref im 7 31 0))
    (assert-equal #x1f (aref im 0 31 0))
    (assert-equal #x1c (aref im 31 0 0))))
 
(define-test decode-16-bit
  (let ((im (decode-pngsuite "basn0g16")))
    (assert-true (typep im '16-bit-image))
    (assert-equal 0 (aref im 0 0 0))
    (assert-equal 65535 (aref im 2 28 0))
    (assert-equal #xf700 (aref im 2 27 0))
    (assert-equal #x3e00 (aref im 31 0 0))))

(define-test encode-8-bit
  (let* ((a (decode-pngsuite "basn0g08"))
	 (b (encode-decode a)))
    (assert-equalp a b)))

(define-test encode-16-bit
  (let* ((a (decode-pngsuite "basn0g16"))
	 (b (encode-decode a)))
    (assert-equalp a b)))

(define-test copy-image
  (let* ((a (decode-pngsuite "basn0g08"))
	 (b (copy-image a)))
    (assert-equalp a b)))

(define-test 16-bit-image
  (let* ((a (decode-pngsuite "basn0g08"))
	 (b (16-bit-image a))
	 (c (8-bit-image b)))
    (assert-equalp a c)))
