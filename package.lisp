;;;; -*- Mode: Lisp;  -*-

(defpackage #:com.ljosa.compat
  (:use #:common-lisp #:cffi)
  (:export #:stream-fd #:with-file #:array-storage-vector))

(defpackage #:png
  (:use #:common-lisp #:cffi #:com.ljosa.compat)
  (:export #:image #:grayscale-image #:rgb-image #:8-bit-image #:16-bit-image
	   #:make-image #:copy-image
	   #:image-height #:image-width #:image-channels
	   #:decode #:encode))


