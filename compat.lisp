(in-package #:png)

#+lispworks
(defun make-shareable-byte-vector (size &optional (byte-size 8))
  (sys:in-static-area
   (make-array size :element-type (list 'unsigned-byte byte-size))))

#+allegro
(defun make-shareable-byte-vector (size &optional (byte-size 8))
  (make-array size :element-type (list 'unsigned-byte byte-size)
	      :allocation :static-reclaimable))

#-(or lispworks allegro)
(defun make-shareable-byte-vector (size &optional (byte-size 8))
   (make-array size :element-type (list 'unsigned-byte byte-size)))

#+sbcl ; Present in SBCL 1.0.24.
(declaim (ftype (function (array) (values (simple-array * (*)) &optional))
                array-storage-vector))
#+sbcl
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

#+allegro
(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR. Not safe
except with vectors allocated by MAKE-SHAREABLE-BYTE-VECTOR and
possibly arrays of type simple-array (unsigned-byte 8) (*)."
;;; An array allocated in static-reclamable is a non-simple array in
;;; the normal Lisp allocation area, pointing to a simple array in the
;;; static-reclaimable allocation area. Therefore we have to get out
;;; the simple-array to find the pointer to the actual contents.
  (let ((simple-vec (gensym "SIMPLE-VEC")))
    `(excl:with-underlying-simple-vector (,vector ,simple-vec)
       (let ((,ptr-var (ff:fslot-address-typed :unsigned-char 
					       :lisp ,simple-vec)))
	 ,@body))))

#+clisp
(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  (let ((vector-var (gensym))
	(type (gensym))
	(nbytes (gensym))
	(bytes-per-word (gensym)))
    `(let* ((,vector-var ,vector)
	    ,type ,bytes-per-word)
       (etypecase ,vector-var
	 ((simple-array (unsigned-byte 8) (*)) (setq ,type :unsigned-char
						     ,bytes-per-word 1))
	 ((simple-array (unsigned-byte 16) (*)) (setq ,type :unsigned-short
						      ,bytes-per-word 2)))
       (with-foreign-pointer (,ptr-var (* (length ,vector-var) ,bytes-per-word)
				       ,nbytes)
         ;; copy-in
         (loop
	    for word from 0 
	    and byte below ,nbytes by ,bytes-per-word 
	    do (cffi-sys:%mem-set (aref ,vector-var word) ,ptr-var ,type byte))
         (unwind-protect (progn ,@body)
           ;; copy-out
           (loop 
	      for word from 0
	      and byte below ,nbytes by ,bytes-per-word
	      do (setf (aref ,vector-var word)
		       (cffi-sys:%mem-ref ,ptr-var ,type byte))))))))

#+clisp
(defmacro with-pointer-to-vector-data ((ptr-var vector) &body body)
  "Bind PTR-VAR to a foreign pointer to the data in VECTOR."
  (let ((vector-var (gensym))
	(type (gensym))
	(nbytes (gensym))
	(bytes-per-word (gensym)))
    `(let* ((,vector-var ,vector)
	    ,type ,bytes-per-word)
       (etypecase ,vector-var
	 ((simple-array (unsigned-byte 8) (*)) (setq ,type :unsigned-char
						     ,bytes-per-word 1))
	 ((simple-array (unsigned-byte 16) (*)) (setq ,type :unsigned-short
						      ,bytes-per-word 2)))
       (with-foreign-pointer (,ptr-var (* (length ,vector-var) ,bytes-per-word)
				       ,nbytes)
         ;; copy-in
         (loop
	    for word from 0 
	    and byte below ,nbytes by ,bytes-per-word 
	    do (cffi-sys:%mem-set (aref ,vector-var word) ,ptr-var ,type byte))
         (unwind-protect (progn ,@body)
           ;; copy-out
           (loop 
	      for word from 0
	      and byte below ,nbytes by ,bytes-per-word
	      do (setf (aref ,vector-var word)
		       (cffi-sys:%mem-ref ,ptr-var ,type byte))))))))
