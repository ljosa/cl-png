;;;; -*- Mode: Lisp; Package: ASDF; -*-

(in-package :asdf)

(defsystem :png
  :perform (load-op :after (op png)
		    (pushnew :png cl:*features*))
  :components ((:file "zlib-from-cl-pdf")
	       (:file "png-pkg"
		      :depends-on ("zlib-from-cl-pdf"))
	       (:file "png"
		      :depends-on ("png-pkg" "zlib-from-cl-pdf")))
  :depends-on (#:uffi))

(defmethod source-file-type ((c cl-source-file) (s (eql (find-system :png))))
  "cl")
