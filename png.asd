;;;; -*- Mode: Lisp; Package: ASDF; -*-

(in-package :asdf)

(defsystem :png
  :perform (load-op :after (op png)
		    (pushnew :png cl:*features*))
  :components ((:file "zlib")
	       (:file "png-pkg"
		      :depends-on ("zlib"))
	       (:file "png"
		      :depends-on ("png-pkg" "zlib"))))

(defmethod source-file-type ((c cl-source-file) (s (eql (find-system :png))))
  "cl")
