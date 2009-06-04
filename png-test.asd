;;;; -*- Mode: Lisp; -*-

(asdf:defsystem :png-test
  :components ((:file "test-png" :depends-on ("lisp-unit"))
	       (:file "lisp-unit")
	       )
  :depends-on (#:png))