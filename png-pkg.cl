;;;; -*- Mode: Lisp; Package: User; -*-

;;; A PNG library in Common Lisp
;;; See the file README for more info.
;;;
;;; Author: Harald Musum
;;;
;;; Copyright (c) 2001 Harald Musum. All rights reserved.


(defpackage "PNG"
  (:use "COMMON-LISP")
  (:import-from "ZLIB" "ENCODE-BUFFER" "DECODE-BUFFER")
  (:export "DECODE-FILE"
	   "ENCODE-FILE"
	   "DECODE-STREAM"
	   "ENCODE-STREAM"
	   "IMAGE-SIZE"))


