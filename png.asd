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
  :components ((:file "png-package")
	       (:file "compat" :depends-on ("png-package"))
	       (:file "libpng" :depends-on ("grovel" "compat" "png-package"))
	       (cffi-grovel:grovel-file "grovel"))
  :depends-on (#:cffi #:image))



