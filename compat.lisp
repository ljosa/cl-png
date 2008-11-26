(in-package #:com.ljosa.compat)

;; STREAM-FD from CL+SSL

(defgeneric stream-fd (stream))

(defmethod stream-fd (stream) stream)

#+sbcl
(defmethod stream-fd ((stream sb-sys:fd-stream))
  (sb-sys:fd-stream-fd stream))

#+cmu
(defmethod stream-fd ((stream system:fd-stream))
  (system:fd-stream-fd stream))

#+openmcl
(defmethod stream-fd ((stream ccl::basic-stream))
  (ccl::ioblock-device (ccl::stream-ioblock stream t)))

#+clisp
(defmethod stream-fd ((stream stream))
  ;; sockets appear to be direct instances of STREAM
  (ignore-errors (socket:stream-handles stream)))

(defcfun "fdopen" :pointer
  (fd :int)
  (mode :string))

(defcfun "fclose" :void
  (file :pointer))

(defmacro with-file ((var stream &optional (mode "rb")) &body body)
  `(let ((,var (fdopen (stream-fd ,stream) ,mode)))
     (unwind-protect (progn ,@body)
       (fclose ,var))))
