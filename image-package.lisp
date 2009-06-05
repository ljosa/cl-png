(in-package #:cl-user)

(defpackage #:image
  (:documentation "Image representation and manipulation.")
  (:use #:common-lisp)
  (:export
   #:image
   #:8-bit-image
   #:16-bit-image
   #:grayscale-image
   #:rgb-image
   #:rgba-image
   #:make-image
   #:copy-image
   #:image-height
   #:image-width
   #:image-channels
   #:image-bit-depth
   ))
