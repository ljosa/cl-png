;;;; -*- Mode: Lisp;  -*-

(defpackage #:com.ljosa.compat
  (:use #:common-lisp #:cffi)
  (:export #:stream-fd #:with-file))

(defpackage #:png
  (:use #:common-lisp #:cffi #:com.ljosa.compat)
  (:export #:image #:make-image #:image-height #:image-width #:image-channels
	   #:decode-file
	   #:encode-file
	   #:decode-stream
	   #:encode-stream
	   #:image-size))


