(in-package #:com.ljosa.compat)

#+sbcl ; Present in SBCL 1.0.24.
(declaim (ftype (function (array) (values (simple-array * (*)) &optional))
                array-storage-vector))
(defun array-storage-vector (array)
  "Returns the underlying storage vector of ARRAY, which must be a non-displaced array.

In SBCL, if ARRAY is a of type \(SIMPLE-ARRAY * \(*)), it is its own storage
vector. Multidimensional arrays, arrays with fill pointers, and adjustable
arrays have an underlying storage vector with the same ARRAY-ELEMENT-TYPE as
ARRAY, which this function returns.

Important note: the underlying vector is an implementation detail. Even though
this function exposes it, changes in the implementation may cause this
function to be removed without further warning."
  ;; KLUDGE: Without TRULY-THE the system is not smart enough to figure out that
  ;; the return value is always of the known type.
  (sb-ext:truly-the (simple-array * (*))
             (if (sb-kernel:array-header-p array)
                 (if (sb-kernel:%array-displaced-p array)
                     (error "~S cannot be used with displaced arrays. Use ~S instead."
                            'array-storage-vector 'array-displacement)
                     (sb-kernel:%array-data-vector array))
                 array)))

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
