;;;; -*- Mode: Lisp;  -*-

(defpackage #:png
  (:documentation "Read and write PNG (Portable Network Graphics) files.")
  (:use #:common-lisp #:cffi)
  (:shadow #:make-shareable-byte-vector
	   #+(or allegro clisp) #:with-pointer-to-vector-data)
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
   #:decode
   #:decode-file
   #:encode
   #:encode-file
   #:decode-bmp
   #:decode-bmp-file
   ))


