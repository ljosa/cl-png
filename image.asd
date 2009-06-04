;;;; -*- Mode: Lisp; -*-

(in-package #:cl-user)

(asdf:defsystem #:image
  :description "Image represetation and manipulation."
  :perform (asdf:load-op :after (op #:image)
			 (pushnew :image *features*))
  :components ((:file "compat")
	       (:file "image" :depends-on ("compat"))
	       (:file "pnm" :depends-on ("image"))))

(defpackage #:image
  (:documentation "Image representation and manipulation.")
  (:use #:common-lisp)
  (:export
   #:image
   #:8-bit-image
   #:16-bit-image
   #:grayscale-image
   #:rgb-image
   #:argb-image
   #:make-image
   #:copy-image
   #:image-height
   #:image-width
   #:image-channels
   #:image-bit-depth
   ))


