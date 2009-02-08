;;;; -*- Mode: Lisp;  -*-

(defpackage #:png
  (:use #:common-lisp #:cffi)
  (:shadow #:make-shareable-byte-vector
	   #+(or allegro clisp) #:with-pointer-to-vector-data)
  (:export #:image #:grayscale-image #:rgb-image #:8-bit-image #:16-bit-image
	   #:make-image #:copy-image
	   #:image-height #:image-width #:image-channels
	   #:decode #:encode))


