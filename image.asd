;;;; -*- Mode: Lisp; -*-

(in-package #:cl-user)

(asdf:defsystem #:image
  :description "Image represetation and manipulation."
  :perform (asdf:load-op :after (op #:image)
			 (pushnew :image *features*))
  :components ((:file "image-package")
	       (:file "image" :depends-on ("image-package"))
	       (:file "ops" :depends-on ("image"))
	       (:file "bmp" :depends-on ("image"))
	       (:file "pnm" :depends-on ("image"))))


