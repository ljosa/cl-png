;;; cl-pdf copyright 2002-2003 Marc Battyani see license.txt for the details
;;; You can reach me at marc.battyani@fractalconcept.com or marc@battyani.net
;;; The homepage of cl-pdf is here: http://www.fractalconcept.com/asp/html/cl-pdf.html

(defpackage #:zlib-from-cl-pdf
  (:use #:common-lisp)
  (:export #:*compress-streams* #:*zlib-search-paths* #:compress-string
   #:uncompress-string))

(in-package #:zlib-from-cl-pdf)

;Adapted from an UFFI example

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar *compress-streams* t)

;; CMUCL desperately wants this *here*, not in config.lisp, or it
;; won't load the compiled file.  Putting it in an eval-when in
;; config.lisp doesn't work either.

(defvar *zlib-search-paths* '("/usr/local/lib/" "/usr/lib/")
  "The paths where to search the zlib shared library")

(defparameter *zlib-path*
  #-(or macosx darwin)
  (uffi:find-foreign-library
   "libz"
   *zlib-search-paths*
   :drive-letters '("C" "D" "E")
   :types '("so" "a" "dll"))
  #+(or macosx darwin)
  (uffi:find-foreign-library
   "z"
   `(,(pathname-directory *load-pathname*))))

(cond
  (*zlib-path*
   (format t "~&;;; Loading ~s" *zlib-path*)
   (uffi:load-foreign-library *zlib-path*
			      :module "zlib" 
			      :supporting-libraries '("c"))
   (push :zlib cl:*features*))
  (t
   (warn "Unable to load zlib. Disabling compression.")
   (setf *compress-streams* nil))))

#+(and zlib cmucl)
(uffi:def-struct dummy ; Why is this necessary???
    (dummy :int))      ; Without it, CMUCL doesn't load the library..

#+zlib
(uffi:def-function ("compress" c-compress)
    ((dest (* :unsigned-char))
     (destlen (* :long))
     (source :cstring)
     (source-len :long))
    :returning :int
    :module "zlib")

#+zlib
(defun compress-string (source)
  "Returns two values: array of bytes containing the compressed data
 and the numbe of compressed bytes"
  (let* ((sourcelen (length source))
	 (destsize (+ 12 (ceiling (* sourcelen 1.01))))
	 (dest (uffi:allocate-foreign-string destsize :unsigned t))
	 (destlen (uffi:allocate-foreign-object :long)))
    (setf (uffi:deref-pointer destlen :long) destsize)
    (uffi:with-cstring (source-native source)
      (let ((result (c-compress dest destlen source-native sourcelen))
	    (newdestlen (uffi:deref-pointer destlen :long)))
	(unwind-protect
	     (if (zerop result)
		 (values (uffi:convert-from-foreign-string 
			  dest
;			  :external-format '(:latin-1 :eol-style :lf)
			  :length newdestlen
			  :null-terminated-p nil)
			 newdestlen)
		 (error "zlib error, code ~D" result))
	  (progn
	    (uffi:free-foreign-object destlen)
	    (uffi:free-foreign-object dest)))))))

#+zlib
(uffi:def-function ("uncompress" c-uncompress)
    ((dest (* :unsigned-char))
     (destlen (* :long))
     (source :cstring)
     (source-len :long))
    :returning :int
    :module "zlib")

#+zlib
(defun uncompress-string (source &key uncompressed-size)
  "Returns two values: array of bytes containing the uncompressed data
 and the number of uncompressed bytes"
  (let* ((sourcelen (length source))
	 (destsize (or uncompressed-size (* 2 sourcelen)))
	 (dest (uffi:allocate-foreign-string destsize :unsigned t))
	 (destlen (uffi:allocate-foreign-object :long)))
    (setf (uffi:deref-pointer destlen :long) destsize)
    (uffi:with-cstring (source-native source)
      (unwind-protect
          (loop
           (let ((result (c-uncompress dest destlen source-native sourcelen))
	         (newdestlen (uffi:deref-pointer destlen :long)))
             (case result
               (0 (return (values (uffi:convert-from-foreign-string 
			           dest
;			  :external-format '(:latin-1 :eol-style :lf)
			           :length newdestlen
			           :null-terminated-p nil)
			          newdestlen)))
              (-5 (uffi:free-foreign-object dest)
                  (setf destsize (* 2 destsize)
                        dest (uffi:allocate-foreign-string destsize :unsigned t)
                        (uffi:deref-pointer destlen :long) destsize))
              (t (error "zlib error, code ~D" result)))))
        (progn
          (uffi:free-foreign-object destlen)
          (uffi:free-foreign-object dest))))))

#|
Unfinished Work!
Using compression by block to avoid the huge cstring allocation of compress.
If somebody has some time to finish it...

(uffi:def-struct zstream
  (next-in (* :unsigned-char))
  (avail-in :unsigned-int)
  (total-in :unsigned-long)
  (next-out (* :unsigned-char))
  (avail-out :unsigned-int)
  (total-out :unsigned-long)
  (msg (* :unsigned-char))
  (state :long)
  (zalloc :long)
  (zfree :long)
  (opaque :long)
  (data-type :int)
  (alder :unsigned-long)
  (reserved :unsigned-long))

(defconstant +z-no-compression+ 0)
(defconstant +z-best-speed+ 1)
(defconstant +z-best-compression+ 9)
(defconstant +z-default-compression+ -1)

#+zlib
(uffi:def-function ("deflateInit" deflate-init)
    ((stream (* (:struct zstream)))
     (level :int))
  :returning :int
  :module "zlib")

(defconstant +z-no-flush+ 0)
(defconstant +z-sync-flush+ 2)
(defconstant +z-full-flush+ 3)
(defconstant +z-finish+ 4)

#+zlib
(uffi:def-function ("deflate" deflate)
    ((stream (* (:struct zstream)))
     (flush :int))
  :returning :int
  :module "zlib")

#+zlib
(uffi:def-function ("deflateEnd" deflate-end)
    ((stream (* (:struct zstream))))
  :returning :int
  :module "zlib")

(defvar *z-block-size* 4096)

|#
