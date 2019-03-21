;;;; -*- Mode: Lisp; -*-

(in-package #:cl-user)

(asdf:defsystem #:png
  :description "Read and write PNG (Portable Network Graphics) files."
  :perform (asdf:load-op :after (op png)
                         (pushnew :png *features*))
  :components ((:file "png-package")
	       (:file "compat" :depends-on ("png-package"))
	       (:file "libpng" :depends-on ("grovel" "compat" "png-package" "wrappers"))
	       (:cffi-grovel-file "grovel")
               (:cffi-wrapper-file "wrappers"))
  :depends-on (#:cffi #:image)
  :defsystem-depends-on ("cffi-grovel"))
