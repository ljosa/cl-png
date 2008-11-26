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
  (ignore-errors (multiple-value-bind (in out) (socket:stream-handles stream)
		   (or in out))))

#+lispworks
(defmethod stream-fd ((stream stream))
  (stream::os-file-handle-stream-file-handle stream))

(defcfun "fdopen" :pointer
  (fd :int)
  (mode :string))

(defcfun "fclose" :void
  (file :pointer))

(defmacro with-file ((var stream &optional (mode "rb")) &body body)
  (let ((stream (gensym "STREAM")))
    ; Keep the stream from being GCed before we close it.
    `(let ((,stream-var ,stream)) 
       (let ((,var (fdopen (stream-fd ,stream) ,mode)))
	 (unwind-protect (progn ,@body)
	   (fclose ,var))))))


#|
;;; From http://www.mail-archive.com/cffi-devel@common-lisp.net/msg00867.html

(in-package #:cffi-sys)

#+allegro
(defun make-shareable-byte-vector (size)
  (make-array size
              :element-type '(unsigned-byte 8)
              :allocation :static-reclaimable))

#+allegro
(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  `(let ((,ptr-var ,vector))
     ,@body))

(export '(make-shareable-byte-vector with-pointer-to-vector-data))
|#
