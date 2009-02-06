;;;; -*- Mode: Lisp; -*-

(asdf:defsystem :png-test
  :components ((:file "test" :depends-on ("lisp-unit"))
	       (:file "lisp-unit")
	       )
  :depends-on (#:png))

