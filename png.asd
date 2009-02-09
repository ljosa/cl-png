;;;; -*- Mode: Lisp; -*-

(in-package #:cl-user)

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op '#:cffi-grovel))

#+cffi-features:darwin
(push #p"/usr/X11/lib/" cffi:*foreign-library-directories*)

#+cffi-features:darwin
(push #p"/opt/local/lib/" cffi:*foreign-library-directories*)

(asdf:defsystem #:png
  :description "Read and write PNG (Portable Network Graphics) files."
  :perform (asdf:load-op :after (op png)
			 (pushnew :png *features*))
  :components ((:file "package")
	       (:file "compat" :depends-on ("package"))
	       (:file "image" :depends-on ("package" "compat"))
	       (:file "libpng" :depends-on ("grovel" "image" "compat"))
	       (cffi-grovel:grovel-file "grovel" :depends-on ("package")))
  :depends-on (#:cffi))

