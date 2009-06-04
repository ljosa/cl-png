;;;# Unit Tests for BMP file interface 
;;;
;;; RUN-TESTS to run tests, either individually, multiply, or all at
;;; once:
;;;
;; (run-tests test1)              ; runs one test
;; (run-tests test1 test2)        ; runs two tests
;; (run-tests)                    ; runs all defined tests
;;;
;;;## Requirements
;;;
;;; Depends on the lisp-unit, obtained from
;;; http://www.cliki.net/lisp-unit. The package defined in
;;; bmp-test.asd will load and run them.
;;;
;;; Test images are provided which must located as defined in
;;; *BMPIMAGES-PATHNAME* below.
;;;
(defpackage #:bmp-test
  (:use #:common-lisp #:lisp-unit #:png)
  (:export #:*images-pathname*))
(in-package #:bmp-test)
(use-package :png)

(defparameter *images-pathname*
  #+asdf (merge-pathnames "images/" 
                          (truename (asdf:system-definition-pathname 
                                     '#:bmp-test))))

;;;## Utility functions
;;;
(defun make-name (bn tp)
  (merge-pathnames (make-pathname :name bn :type tp) *images-pathname*))

(defun decode-pngimage (basename)
  (let ((pathname (make-name basename "png")))
    (with-open-file (input pathname :element-type '(unsigned-byte 8))
      (decode input))))

(defun decode-bmpimage (basename &key strip-alpha)
  (let ((pathname (make-name basename "bmp")))
    (with-open-file (input pathname :element-type '(unsigned-byte 8))
      (decode-bmp input :strip-alpha strip-alpha))))

(defun encode-decode (im &key strip-alpha keep-tmp)
  (let ((pathname (make-name "tmp" "bmp")))
    (ignore-errors (delete-file pathname))
    (encode-bmp-file im pathname :strip-alpha strip-alpha)
    (prog1
        (decode-bmp-file pathname)
      (unless keep-tmp
        (delete-file pathname)))))

(defun max-diff (im1 im2)
  (image-max (image-sub im1 im2)))


;;;# Tests
;;;
;;; Test that the row padding is being handled properly.  There are
;;; four cases: images whose widths modulo 4 result in 0,1,2,3. 
;;; 
;;; The methodology is to decode a file into an image, re-encode the
;;; image back to a file, then decode it again and compare the two
;;; decoded images. 

(define-test encode-modulo-0
  (let ((a (decode-bmpimage "scene")))
    (assert-equal 0 (max-diff a (encode-decode a)))))

(define-test encode-modulo-1
  (let ((a (decode-bmpimage "scene-w765"))
        (b (decode-bmpimage "scene-bw765")))
    (assert-true (typep a 'rgb-image))
    (assert-equal 0 (max-diff a (encode-decode a)))
    (assert-true (typep b 'grayscale-image))
    (assert-error 'png::unhandled-bitcount (encode-decode b))))
;;; Note: haven't implemented encoder for grayscale-image yet

(define-test encode-modulo-2
  (let ((a (decode-bmpimage "scene-w766")))
    (assert-equal 0  (image-max (image-sub a (encode-decode a))))))

(define-test encode-modulo-3
  (let ((a (decode-bmpimage "scene-w767"))
        (b (decode-bmpimage "scene-bw767")))
    (assert-true (typep a 'rgb-image))
    (assert-equal 0 (max-diff a (encode-decode a)))
    (assert-true (typep b 'grayscale-image))
    (assert-error 'png::unhandled-bitcount (encode-decode b))))
;;; Note: haven't implemented encoder for grayscale-image yet


(run-tests encode-modulo-0 encode-modulo-2)
(run-tests encode-modulo-1 encode-modulo-3)



(define-test decode-strip-alpha
    (let* ((a (decode-bmpimage "intrepid-rgb"))
           (b (decode-bmpimage "intrepid-argb"))
           (c (decode-bmpimage "intrepid-argb" :strip-alpha T)))
    (assert-true (typep a 'rgb-image))
    (assert-true (typep b 'rgba-image))
    (assert-true (typep c 'rgb-image))
    (assert-equal 0 (max-diff a c))))

(define-test encode-strip-alpha
    (let* ((a (decode-bmpimage "intrepid-argb"))
           (b (encode-decode a :strip-alpha T :keep-tmp T))
           (c (decode-bmpimage "tmp")))
      (assert-true (typep a 'rgba-image))
      (assert-true (typep b 'rgb-image))
      (assert-true (typep c 'rgb-image))
      (assert-equal 0 (max-diff b c))))

(run-tests decode-strip-alpha)
(run-tests encode-strip-alpha)



;;;# Tests for other compression modes
;;;
;;; Currently, all of these files should signal errors in the decoder,
;;; as they are not supported.  

(define-test encode-xrgb
  (assert-error 'png::unhandled-compression (decode-bmpimage "intrepid-xrgb")))

;;; This one uses Compression=3, bitcount=16
(define-test encode-r5b6g5
  (assert-error 'png::unhandled-compression (decode-bmpimage "intrepid-r5b6g5")))

;;; This one uses Compression=3, bitcount=16
(define-test encode-a1r5b5g5
  (assert-error 'png::unhandled-compression (decode-bmpimage "intrepid-a1r5b5g5")))

;;; This one uses Compression=0, bitcount=16
(define-test encode-x1r5b5g5
  (assert-error 'png::unhandled-bitcount (decode-bmpimage "intrepid-x1r5b5g5")))

(run-tests encode-xrgb)
(run-tests encode-r5b6g5)
(run-tests encode-a1r5b5g5)
(run-tests encode-x1r5b5g5)


