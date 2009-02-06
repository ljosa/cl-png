;;;; -*- Mode: Lisp; -*-

(in-package #:cl-user)

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op '#:cffi-grovel))

#+(and cffi-features:darwin ccl)
(push #p"/usr/X11/lib/" cffi:*foreign-library-directories*)

#+(and cffi-features:darwin ccl)
(push #p"/opt/local/lib/" cffi:*foreign-library-directories*)

(asdf:defsystem :png
  :perform (asdf:load-op :after (op png)
			 (pushnew :png *features*))
  :components ((:file "package")
	       (:file "compat" :depends-on ("package"))
	       (:file "image" :depends-on ("package"))
	       (:file "libpng" :depends-on ("libpngint" "image" "compat"))
	       (cffi-grovel:grovel-file "libpngint" :depends-on ("package"))
	       )
  :depends-on (#:cffi))

