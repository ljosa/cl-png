;;;; -*- Mode: Lisp; Package: User; -*-

;;; A PNG library in Common Lisp
;;; See the file README for more info.
;;;
;;; Author: Harald Musum
;;;
;;; Copyright (c) 2001 Harald Musum. All rights reserved.


(defpackage #:png
  (:use #:cl)
  (:export #:decode-file
	   #:encode-file
	   #:decode-stream
	   #:encode-stream
	   #:image-size))


