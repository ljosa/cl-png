;;;; -*- Mode: Lisp; -*-

(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op '#:cffi-grovel))

(asdf:defsystem :png
  :perform (load-op :after (op png)
		    (pushnew :png cl:*features*))
  :components ((:file "package")
	       (:file "compat" :depends-on ("package"))
	       (:file "libpng" :depends-on ("libpngint" "compat"))
	       (cffi-grovel:grovel-file "libpngint" :depends-on ("package"))
	       )
  :depends-on (#:cffi))

