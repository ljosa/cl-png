;;; -*- Mode: Lisp; Package: PNG; -*-
;;;
;;; See the file README for documentation and copyright information.

(in-package #:png)

(eval-when (compile)
  (declaim (optimize (speed 0) (safety 3) (space 1) (debug 3) #+cmu (ext:inhibit-warnings 3))))

(defparameter *debug-level* 0)
(defparameter *debug-stream* t)

(defmacro debug-format-1 (&body body)
  `(case *debug-level*
     (0 nil)
     (1 (format *debug-stream* ,@body))
     (2 (format *debug-stream* ,@body))))

(defmacro debug-format-2 (&body body)
  `(case *debug-level*
     (0 nil)
     (1 nil)
     (2 (format *debug-stream*,@body))))

(defconstant +png-major-version+ 0)
(defconstant +png-minor-version+ 2)

(defvar +png-signature+ #(137 80 78 71 13 10 26 10))

(defvar +crc-table+
  (loop with array = (make-array 256 :element-type '(unsigned-byte 32))
	for i from 0 below 256
	for c = i
	do
	(loop for k from 0 below 8
	      do
	      (if (= (logand c 1) 1)
		  (setq c (logxor (ash c -1) #xedb88320))
		(setq c (ash c -1))))
	(setf (aref array i) c)
	finally (return array)))

(deftype bit-depth () '(member 1 2 4 8 16))

(deftype color-type () '(member 0 2 3 4 6))

(defvar +bit-depth-and-color-type-combination+ '((0 1) (0 2) (0 4) (0 8) (0 16)
						      (2 8) (2 16)
						      (3 1) (3 2) (3 4) (3 8)
						      (4 8) (4 16)
						      (6 8) (6 16)))

(defstruct png-image
  ihdr
  idat
  plte
  text)

(defstruct ihdr
  width
  height
  bit-depth
  color-type
  compression-method
  filter-method
  interlace-method)

(defstruct text
  keywords
  strings)

(defun read-32-bits (stream)
  "Read a 32-bit word from STREAM, MSB first"
  (loop with length = 0
        for i from 0 below 4 do
        (incf length (ash (or (read-byte stream nil nil)
			      (error "incomplete word on input stream"))
			  (* 8 (- 3 i))))
        finally (return length)))

(defun read-32-bits-from-array (array)
  "Read a 32-bit word from ARRAY, MSB first"
  (loop with length = 0
        for i from 0 below 4 do
        (incf length (ash (aref array i) (* 8 (- 3 i))))
        finally (return length)))

(defun read-n-bytes-from-array (array n)
  "Read N octets from ARRAY, MSB first"
  (loop with length = 0
        for i from 0 below n do
        (incf length (ash (aref array i) (* 8 (- 1 i))))
        finally (return length)))

(defun string-to-byte-array (s)
  (declare (type simple-base-string s)
           (optimize (speed 3) (safety 0)))
  (let* ((len (length s))
	 (a (make-array len :element-type '(unsigned-byte 8))))
    (dotimes (i len)
      (setf (aref a i) (char-code (char s i))))
    a))

;;; CRC-32 functions

(defun update-crc (crc buffer)
  (declare (type (unsigned-byte 32) crc)
           (type (simple-array (signed-byte 8) (*)) buffer)
           (optimize (speed 3) (safety 0)))
  (setq crc (logxor crc #xffffffff))
  (loop for n from 0 below (length buffer)
	for i = (logand #xff (logxor crc (aref buffer n)))
	do	
	(setq crc (logxor (aref +crc-table+ i) (ash crc -8)))
	finally (return (logxor crc #xffffffff))))

(defun crc (buffer)
  (update-crc 0 buffer))

;;; Functions that operate on PNG chunks

(defun read-chunk-length (stream)
  "Read 4 bytes from STREAM and return length, else return nil"
  (loop with length = 0
        for i from 0 below 4
        for byte = (read-byte stream nil nil)
        do
        (if byte
            (incf length (ash byte (* 8 (- 3 i))))
          (return nil))
        finally (return length)))

(defun read-chunk-type (stream)
  (let ((type (make-array 4 :element-type '(unsigned-byte 8))))
    (read-sequence type stream)
    (print type)
    (map 'string #'code-char type)))

(defun read-chunk (stream length)
  "Read a chunk from STREAM, calculate CRC and compare with supplied CRC.  Return
data if the CRCs are equal, else return an error"
  ;; We have to calculate the CRC of both chunk data and chunk type
  ;; (which is 4 bytes long, so we add 4 bytes to the data array)
  (let ((data (make-array (+ length 4) :element-type '(unsigned-byte 8)))
        data-crc crc chunk-type)
;    (format t "~&chunk-length is ~D~%" length)
    (read-sequence data stream)
    (setq chunk-type (map 'string #'code-char (subseq data 0 4)))
    (debug-format-2 "~&chunk-type is ~a.~%" chunk-type)
    (setq data-crc (crc data))
    (setq crc (read-32-bits stream))
    (if (equal crc data-crc)
        (values chunk-type (subseq data 4))
      (error "CRC error"))))

;;; Chunk decoding functions

(defun decode-idat (idat size)
  "Decode IDAT chunk and return a vector with the result"
  (debug-format-2 "~&IDAT processing. Length of data: ~D.~%" (length idat))
  (map '(array (unsigned-byte 8) (*)) #'char-code
       (zlib-from-cl-pdf:uncompress-string (map 'string #'code-char idat)
                                           :uncompressed-size size)))

(defun decode-plte (plte)
  "Decode PLTE chunk and return a vector palette"
  (let* ((length (length plte))
	 (palette (make-array length)))
    (assert (zerop (mod length 3)))
    (debug-format-2 "~&PLTE processing. Length of plte: ~D.~%" length)
    (loop for i from 0 below (floor length 3)
	  do
	  (setf (aref palette i) (vector (aref plte (* i 3))
					 (aref plte (+ (* i 3) 1))
					 (aref plte (+ (* i 3) 2))))
	  finally (return palette))))

(defun decode-ihdr (ihdr)
  "Decode IHDR chunk and return an ihdr struct"
  (debug-format-2 "~&IHDR processing.~%")
  (make-ihdr :width (read-32-bits-from-array (subseq ihdr 0 4))
             :height (read-32-bits-from-array (subseq ihdr 4 8))
             :bit-depth (aref ihdr 8)
             :color-type (aref ihdr 9)
             :compression-method (aref ihdr 10)
             :filter-method (aref ihdr 11)
             :interlace-method (aref ihdr 12)))

(defun decode-gama (gama)
  "Decode gAMA chunk.  Not implemented"
  (debug-format-1 "~&gAMA processing not implemented. Gamma: ~D.~%"
		  (read-32-bits-from-array gama)))

(defun decode-text (text)
  "Decode TEXT chunk and return keyword and string"
  (debug-format-2 "~&tEXt processing")
  (let* ((position (position (code-char 0)
			     (map 'string #'code-char text) :test #'equal))
	 (keyword (map 'string #'code-char (subseq text 0 position)))
	 (string (map 'string #'code-char (subseq text (1+ position)))))
    (values keyword string)))

;;; Filtering functions
    
(defun paeth-predictor (a b c)
  (declare (type (unsigned-byte 8) a b c)
	   (optimize speed))
  (let* ((p (+ a (- b c )))
         (pa (abs (- p a)))
         (pb (abs (- p b)))
         (pc (abs (- p c))))
    (cond ((and (<= pa pb) (<= pa pc)) a)
	  ((<= pb pc) b)
	  (t c))))

;; Remove filter line for line, since filter method can be changed for
;; each scanline and we may want to display line by line anyway
(defun remove-filter (data filter-type index previous-line-index
		 length bytes-per-pixel)
  "Remove filtering of FILTER-TYPE for a scanline"
  (declare (type fixnum index length filter-type bytes-per-pixel)
	   (type (vector (unsigned-byte 8)) data)
;	   (type (unsigned-byte 8) filter-type bytes-per-pixel)
	   (optimize speed))
  (loop for i fixnum from 0 below length
	for raw fixnum = (aref data (+ index i))
	for above fixnum = (if previous-line-index
			       (aref data (+ previous-line-index i))
			     0)
	for left fixnum = (if (>= (- i bytes-per-pixel) 1)
			      (aref data (+ index (- i bytes-per-pixel)))
			    0)
	for upper-left fixnum = (if (and previous-line-index
					 (>= (- i bytes-per-pixel) 1))
				    (aref data (+ previous-line-index
						  (- i bytes-per-pixel)))
				  0)
	do
;      (format t "~&index: ~D~%" index)
;      (format t "~&previous-line-index: ~D~%" previous-line-index)
;      (format t "~&i: ~D~%" i)
;      (format t "~&raw: ~D~%" raw)
;      (format t "~&above: ~D~%" above)
;      (format t "~&left: ~D~%" left)
;      (format t "~&upper-left: ~D~%" upper-left)
	(setf (aref data (+ index i))
	      (ecase filter-type
		(0 raw)
		(1 (logand 255 (+ raw left)))
		(2 (logand 255 (+ raw above)))
		(3 (logand 255 (+ raw (floor (+ left above) 2))))
		(4 (logand 255 (+ raw (paeth-predictor left above upper-left))))))))

(defun apply-filter (data filtered-line filter-type index previous-line-index
		     length bytes-per-pixel)
  "Apply FILTER-TYPE filter to a scanline"
  (declare (type (vector (unsigned-byte 8)) data filtered-line)
	   (type fixnum index length)
	   (type (unsigned-byte 8) filter-type bytes-per-pixel)
	   (optimize speed))
  (setf (aref filtered-line 0) filter-type)
  (loop for i fixnum from 1 below length
	;; left-index is the index of the pixel to the left of the
	;; current one
	for left-index fixnum = (- i bytes-per-pixel)
	for raw of-type (unsigned-byte 8) = (aref data (+ index i))
	for left of-type (unsigned-byte 8) = (if (>= left-index 1)
						 (aref data (+ index left-index))
					       0)
	for above of-type (unsigned-byte 8) = (if previous-line-index
						  (aref data (+ previous-line-index i))
						0)
	for upper-left of-type (unsigned-byte 8) = (if (and previous-line-index
							    (>= left-index 1))
						       (aref data
							     (+ previous-line-index left-index))
						     0)
	do
;      (format t "~&index: ~D~%" index)
;      (format t "~&previous-line-index: ~D~%" previous-line-index)
;      (format t "~&i: ~D~%" i)
;      (format t "~&raw: ~D~%" raw)
;      (format t "~&above: ~D~%" above)
;      (format t "~&left: ~D~%" left)
;      (format t "~&upper-left: ~D~%" upper-left)
	(setf (aref filtered-line i)
	      (ecase filter-type
		(0 raw)
		(1 (mod (- raw left) 256))
		(2 (mod (- raw above) 256))
		(3 (mod (- raw (floor (+ left above) 2)) 256))
		(4 (mod (- raw (paeth-predictor left above upper-left)) 256))))))

;;; Output to file (PGM and PNM pictures)

(defun write-pgm (image filename width height comment &optional (16-bit t))
  (debug-format-1 "~&Writing PNG image to PGM file: ~A.~%" filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "P2~%#~A~%~D ~D~%" comment width height)
    (if 16-bit
	(progn (write-string "65536" stream)
	       (dotimes (i height)
		 (dotimes (j width)
		   (when (zerop (mod j 4))
		     (terpri stream))
		   (write (ldb (byte 16 0) (aref image i j)) :stream stream)
		   (write-string " " stream))))
      (progn (write-string "255" stream)
	     (dotimes (i height)
	       (dotimes (j width)
		 (when (zerop (mod j 4))
		   (terpri stream))
		 (write (ldb (byte 8 0) (aref image i j)) :stream stream)
		 (write-string " " stream)))))))

(defun write-raw-pgm (image filename width height comment)
  (debug-format-1 "~&Writing PNG image to raw PGM file: ~A.~%" filename)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :element-type '(unsigned-byte 8))
    (format stream "P5~%#~A~%~D ~D~%~d~%" comment width height 255)
    (dotimes (i height)
      (dotimes (j width)
	(write-byte (ldb (byte 8 0) (aref image i j)) stream)))))

(defun write-ppm (image filename width height comment &optional (16-bit t))
  (debug-format-1 "~&Writing PNG image to PPM file: ~A.~%" filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "P3~%#~A~%~D ~D~%" comment width height)
    (if 16-bit
	(progn (write-string "65536" stream)
      	       (dotimes (i height)
		 (dotimes (j width)
		   (when (zerop (mod j 4))
		     (terpri stream))
		   (write (ldb (byte 16 0) (aref image i j)) :stream stream)
		   (write-string " " stream)
		   (write (ldb (byte 16 16) (aref image i j)) :stream stream)
		   (write-string " " stream)
		   (write (ldb (byte 16 32) (aref image i j)) :stream stream)
		   (write-string " " stream))))
      (progn (write-string "255" stream)
	     (dotimes (i height)
	       (dotimes (j width)
		 (when (zerop (mod j 4))
		   (terpri stream))
		 (write (ldb (byte 8 0) (aref image i j)) :stream stream)
		 (write-string " " stream)
		 (write (ldb (byte 8 8) (aref image i j)) :stream stream)
		 (write-string " " stream)
		 (write (ldb (byte 8 16) (aref image i j)) :stream stream)
		 (write-string " " stream)))))))

(defun write-raw-ppm (image filename width height comment)
  (debug-format-1 "~&Writing PNG image to raw PPM file: ~A.~%" filename)
  (with-open-file (stream filename
			  :direction :output
			  :if-exists :supersede
			  :element-type '(unsigned-byte 8))
    (format stream "P6~%#~A~%~D ~D~%~d~%" comment width height 255)
    (dotimes (i height)
      (dotimes (j width)
	(write-byte (ldb (byte 8 0) (aref image i j)) stream)
	(write-byte (ldb (byte 8 8) (aref image i j)) stream)
	(write-byte (ldb (byte 8 16) (aref image i j)) stream)))))

(defun write-pnm (image output-file width height comment color-type bit-depth)
  (let ((comment (concatenate 'string "Source file: " (file-namestring comment)))
	(16-bit (= bit-depth 16)))
    (if (or (= color-type 0) (= color-type 4))
        (write-pgm image output-file width height comment 16-bit)
      (write-ppm image output-file width height comment 16-bit))))

(defun read-signature (stream)
  "Read PNG signature and return t if correct, else return nil"
  (dotimes (i (length +png-signature+) t)
    (unless (eql (read-byte stream nil nil) (aref +png-signature+ i))
      (return nil))))

(defun result-array-size (ihdr)
  "Compute the size of the array to store the IDAT chunks in."
  (if (= (ihdr-interlace-method ihdr) 0)
      (* (ihdr-height ihdr) (line-length ihdr))
      ;; FIXME.  Is this correct for 16 bit images?  And is it correct
      ;; at all (gives 1116 instead of 1084 for the test images)
    (+ (* (ihdr-height ihdr) (line-length ihdr)) (* (ceiling (ihdr-height ihdr) 8) 15))))

(defun line-length (ihdr)
  "Return the length of a scanline"
  ;; Add one because the first byte in each line represents the filter type
  (+ 1
     (ceiling
      (* (ihdr-width ihdr)
         (bits-per-pixel ihdr))
      8)))

(defun bits-per-pixel (ihdr)
  (ecase (ihdr-color-type ihdr)
    (0 (ihdr-bit-depth ihdr))
    (2 (* (ihdr-bit-depth ihdr) 3))
    (3 (ihdr-bit-depth ihdr))
    (4 (* (ihdr-bit-depth ihdr) 2))
    (6 (* (ihdr-bit-depth ihdr) 4))))

(defun bytes-per-pixel (ihdr)
  (ceiling (bits-per-pixel ihdr) 8))

(defun write-pixel-16-bits (target x y red green blue alpha)
  (setf (aref target y x)
	(dpb red (byte 16 0)
	     (dpb green (byte 16 16)
		  (dpb blue (byte 16 32)
		       (dpb (- 255 alpha) (byte 16 48) 0))))))

;; FIXME: Should be able to write pixels that have greater width and
;; height than 1 (when the image is interlaced)
(defun write-pixel (target x y red green blue alpha)
  "Write each pixel to target array as a 32-bit value"
  (declare (type fixnum x y red green blue alpha)
	   (optimize speed))
  (setf (aref target y x)
	(dpb red (byte 8 0)
	     (dpb green (byte 8 8)
		  (dpb blue (byte 8 16)
		       (dpb (- 255 alpha) (byte 8 24) 0))))))

(defun raw-component-value (source byte-index bit-index bit-depth)
  (declare (type (unsigned-byte 32) byte-index bit-index)
	   (type (unsigned-byte 8) bit-depth)
	   (type (vector (unsigned-byte 8)) source)
	   (optimize speed))
  (ecase bit-depth
    (1 (ldb (byte 1 (- 7 (mod bit-index 8)))
	    (aref source (+ byte-index (ash bit-index -3)))))
    (2 (ldb (byte 2 (* 2 (- 3 (mod bit-index 4))))
	    (aref source (+ byte-index (ash bit-index -2)))))
    (4 (ldb (byte 4 (* 4 (- 1 (mod bit-index 2))))
	    (aref source (+ byte-index (ash bit-index -1)))))
    (8 (aref source (+ byte-index bit-index)))
    (16 (logior (ash (aref source byte-index) 8)
		(aref source (+ byte-index 1))))))

(defun component-value (source byte-index bit-index bit-depth)
  (ecase bit-depth
    (1 (* 255 (raw-component-value source byte-index bit-index bit-depth)))
    (2 (* 85 (raw-component-value source byte-index bit-index bit-depth)))
    (4 (* 17 (raw-component-value source byte-index bit-index bit-depth)))
    (8 (raw-component-value source byte-index bit-index bit-depth))
    (16 (raw-component-value source byte-index bit-index bit-depth))))

(defun write-scanline (image image-data target x0 dx y index
			     width pixel-width pixel-height bit-depth color-type)
  ;; pixel-width and pixel-height might be used later if we implement
  ;; rendering of interlaced picture. Ignored for now.
  (declare (ignore pixel-width pixel-height)
	   (type (unsigned-byte 32) x0 dx y index width)
	   (type (unsigned-byte 8) bit-depth color-type)
	   (optimize speed))
  (loop for x of-type (unsigned-byte 32) from x0 by dx
	;; i is the index sample, which means that if bit-depth is 1
	;; it counts bits, if bit-depth is 8 it counts octets
	for i of-type (unsigned-byte 32) from 0
	until (>= x width)
	do
	(ecase color-type
	  (0
	   (let ((value (component-value image-data
					 (if (= bit-depth 16)
					     (+ index (* 2 i))
					   index)
					 i
					 bit-depth)))
	     (if (= bit-depth 16)
		 (write-pixel-16-bits target x y value value value 255)
	       (write-pixel target x y value value value 255))))
	  (2
	   (let ((red (component-value image-data
				       (+ index (* 3 i (ash bit-depth -3)))
				       0
				       bit-depth))
		 (green (component-value image-data
					 (+ index (* 3 i (ash bit-depth -3))
					    (ash bit-depth -3))
					 0
					 bit-depth))
		 (blue (component-value image-data
					(+ index (* 3 i (ash bit-depth -3))
					   (* 2 (ash bit-depth -3)))
					0
					bit-depth)))
	     (if (= bit-depth 16)
		 (write-pixel-16-bits target x y red green blue 255)
               (write-pixel target x y red green blue 255))))
	  (3
	   (let* ((palette-index (raw-component-value image-data index i bit-depth))
		  (plte (png-image-plte image))
		  (red (aref (aref plte palette-index) 0))
		  (green (aref (aref plte palette-index) 1))
		  (blue (aref (aref plte palette-index) 2)))
	     (write-pixel target x y red green blue 255)))
	  (4
	   (let ((value (component-value image-data
					 (+ index (* 2 i (ash bit-depth -3)))
					 0
					 bit-depth))
		 (alpha (component-value image-data
					 (+ index (* 2 i (ash bit-depth -3))
					    (ash bit-depth -3))
					 0
					 bit-depth)))
	     (if (= bit-depth 16)
		 (write-pixel-16-bits target x y value value value alpha)
	       (write-pixel target x y value value value alpha))))
	  (6
	   (let ((red (component-value image-data
				       (+ index (* 4 i (ash bit-depth -3)))
				       0
				       bit-depth))
		 (green (component-value image-data
					 (+ index (* 4 i (ash bit-depth -3))
					    (ash bit-depth -3))
					 0
					 bit-depth))
		 (blue (component-value image-data
					(+ index (* 4 i (ash bit-depth -3))
					   (* (ash bit-depth -3)) 2)
					0
					bit-depth))
		 (alpha (component-value image-data
					 (+ index (* 4 i (ash bit-depth -3))
					    (* (ash bit-depth -3) 3))
					 0
					 bit-depth)))
	     (if (= bit-depth 16)
		 (write-pixel-16-bits target x y red green blue alpha)
               (write-pixel target x y red green blue alpha)))))))

(defun decode-image (stream)
  (declare (optimize speed))
  (if (read-signature stream)
      (debug-format-2 "~&PNG signature OK.~%")
    (error "~A is not a PNG file." stream))
  (loop for chunk-length  = (read-chunk-length stream)
	with idat = (make-array 0 :element-type '(unsigned-byte 8))
	with ihdr
	with plte
	with text
	with width
	with height
	when chunk-length
	do
	(multiple-value-bind (chunk-type data)
	    (read-chunk stream chunk-length)
	  (cond ((string= chunk-type "IHDR")
		 (debug-format-1 "~&Processing IHDR chunk.~%")
		 (setq ihdr (decode-ihdr data))
		 ;; FIXME. Should also check that the combinations of
		 ;; the different parameters are valid.
		 (check-type (ihdr-bit-depth ihdr) bit-depth)
		 (check-type (ihdr-color-type ihdr) color-type)
		 (check-type (ihdr-compression-method ihdr) (integer 0))
		 (check-type (ihdr-compression-method ihdr) (integer 0))
		 (check-type (ihdr-filter-method ihdr) (integer 0))
		 (check-type (ihdr-interlace-method ihdr) (integer 0 1))
		 (setq width (ihdr-width ihdr))
		 (setq height (ihdr-height ihdr))
		 (assert (> width 0))
		 (assert (> height 0))
		 (when (>= *debug-level* 1) (print ihdr)))
		((string= chunk-type "gAMA")
		 (debug-format-1 "~&Processing gAMA chunk.~%")
		 (decode-gama data))
		((string= chunk-type "PLTE")
		 (debug-format-1 "~&Processing PLTE chunk.~%")
		 (setq plte (decode-plte data)))
		 ;;(print plte))
		((string= chunk-type "IDAT")
		 (debug-format-1 "~&Processing IDAT chunk.~%")
		 (setq idat (concatenate '(vector (unsigned-byte 8) *) idat data)))
		((string= chunk-type "tEXt")
                 (debug-format-1 "~&Processing tEXt chunk.~%")
                 (multiple-value-bind (keyword string) (decode-text data)
                   (if text
                       (progn
                         (push keyword (text-keywords text))
                         (push string (text-strings text)))
                     (setq text (make-text :keywords (list keyword) :strings (list string)))))
		 (when (>= *debug-level* 1) (print text)))
		((string= chunk-type "IEND")
		 (debug-format-1 "~&IEND chunk found.  End of processing.~%")
		 (return (make-png-image
			  :ihdr ihdr
			  :idat (decode-idat idat
					     (result-array-size ihdr))
			  :plte plte
			  :text text)))
		(t (debug-format-1 "~&Unsupported chunk type found.  Skipping chunk.~%"))))))

;; FIXME.  Much common code with decode-image
(defun image-size (file)
  (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
    (if (read-signature stream)
	(debug-format-2 "~&PNG signature OK.~%")
      (error "~A is not a PNG file." stream))
    (let ((chunk-length (read-chunk-length stream))
	  ihdr width height)
      (when chunk-length
	(multiple-value-bind (chunk-type data)
	    (read-chunk stream chunk-length)
	  (cond ((string= chunk-type "IHDR")
		 (debug-format-1 "~&Processing IHDR chunk.~%")
		 (setq ihdr (decode-ihdr data))
		 ;; FIXME. Should also check that the combinations of
		 ;; the different parameters are valid.
		 (check-type (ihdr-bit-depth ihdr) bit-depth)
		 (check-type (ihdr-color-type ihdr) color-type)
		 (check-type (ihdr-compression-method ihdr) (integer 0))
		 (check-type (ihdr-compression-method ihdr) (integer 0))
		 (check-type (ihdr-filter-method ihdr) (integer 0))
		 (check-type (ihdr-interlace-method ihdr) (integer 0 1))
		 (setq width (ihdr-width ihdr))
		 (setq height (ihdr-height ihdr))
		 (assert (> width 0))
		 (assert (> height 0))
		 (when (>= *debug-level* 1) (print ihdr)))))
	 (return-from image-size (values width height))))))

(defun decode-stream (stream &key output-file)
  (let* ((image (decode-image stream))
         (image-data (png-image-idat image))
         (line-length (line-length (png-image-ihdr image)))
         (previous-line-index nil)
         (width (ihdr-width (png-image-ihdr image)))
         (height (ihdr-height (png-image-ihdr image)))
         (bit-depth (ihdr-bit-depth (png-image-ihdr image)))
         (color-type (ihdr-color-type (png-image-ihdr image)))
         (target (make-array (list height width))))
      (declare (type (vector (unsigned-byte 8)) image-data)
	       (optimize speed))
      (debug-format-1 "~&Decoding stream: ~A.~%" stream)
      (case (ihdr-interlace-method (png-image-ihdr image))
	(0
	 (debug-format-2 "~&Interlace method 0.~%")
	 (loop for y from 0 below height
	       with index = 0
	       with dx = 1
	       do
;               (format t "~&Line ~D~%" y)
;               (format t "~&index ~D~%" index)
;               (format t "~&line-length ~D~%" line-length)
;               (format t "~&Filter type ~D~%" (aref image-data index))
	       (remove-filter image-data (aref image-data index) index previous-line-index
			      line-length (bytes-per-pixel (png-image-ihdr image)))
	       (setq previous-line-index index)
;               (format t "~&Line filtered: ~A~%"
;                       (subseq image-data index (+ index line-length)))
	       (write-scanline image image-data target 0 dx y (1+ index)
			       width 1 1 bit-depth color-type)
	       (incf index line-length)))
	(1
	 (debug-format-2 "~&Interlace method 1.~%")
	 (loop for j fixnum from 1 to 7
	       for x0 fixnum in '(0 4 0 2 0 1 0)
	       for dx fixnum in '(8 8 4 4 2 2 1)
	       for y0 fixnum in '(0 0 4 0 2 0 1)
	       for dy fixnum in '(8 8 8 4 4 2 2)
	       for pixel-width fixnum in '(8 4 4 2 2 1 1)
	       for pixel-height fixnum in '(8 4 4 2 2 1 1)
	       ;; Add one because the first byte represents filter method
	       for line-length = (+ 1 (ceiling
				       (* (bits-per-pixel (png-image-ihdr image))
					  (ceiling (- width x0) dx)) 8))
	       for previous-line-index = nil
	       with index fixnum = y0
	       do
	       (when (= j 1)
		 (setq index y0))
;               (format t "~&Pass no. ~D~%" j)
;               (format t "~&x0: ~D~%" x0)
;               (format t "~&dx: ~D~%" dx)
;               (format t "~&y0: ~D~%" y0)
;               (format t "~&dy: ~D~%" dy)
;               (format t "~&pixel-width: ~D~%" pixel-width)
;               (format t "~&pixel-height: ~D~%" pixel-height)
	       (loop for y from y0 by dy
		     until (>= y height)
		     when (> line-length 1)
		     do
;                     (format t "~&Line ~D~%" y)
;                     (format t "~&index ~D~%" index)
;                     (format t "~&line-length ~D~%" line-length)
;                     (format t "~&Filter type ~D~%" (aref image-data index))
		     (remove-filter image-data (aref image-data index) index
				    previous-line-index line-length
				    (bytes-per-pixel (png-image-ihdr image)))
		     (setq previous-line-index index)
;                     (format t "~&Line filtered: ~A~%" (subseq image-data index
;                                                             (+ index line-length)))
		     (write-scanline image image-data target x0 dx
				     y (1+ index) width pixel-width
				     pixel-height bit-depth color-type)
		     (incf index line-length))))
	(t (error "Unknown interlace method")))
;      (format t "~&idat array length ~D~%" (length image-data))
;      (format t "~&idat array written to target arrray")
      (when output-file
	(write-pnm target output-file width height (pathname stream) color-type bit-depth))
      target))

(defun decode-file (file &key (output-file nil))
  (with-open-file (stream file :direction :input :element-type '(unsigned-byte 8))
    (debug-format-1 "~&Decoding file: ~A.~%" file)
    (decode-stream stream :output-file output-file)))

;;; Writing PNG files

(defun encode-stream (source stream &key (filter-type 4)
		      (bit-depth 8) (color-type 2) (source-bit-depth 8)
		      (interlace-method 0))
  (let* ((compression-method 0)
	 (filter-method 0)
	 (height (array-dimension source 0))
	 (width (array-dimension source 1))
	 (ihdr (make-ihdr :width width
			  :height height
			  :bit-depth bit-depth
			  :color-type color-type
			  :compression-method compression-method
			  :filter-method filter-method
			  :interlace-method interlace-method))
	 (text (list "Software"
		     (format nil "CL PNG library, version ~D.~D."
			     +png-major-version+
			     +png-minor-version+)))
	 (plte-hash-table nil)
	 (line-length (line-length ihdr))
	 ;; Add one byte per line to for the filter type
	 (idat (if (= interlace-method 0)
		   (make-array (* height line-length)
			       :initial-element 0
			       :element-type '(unsigned-byte 8))
                 (make-array 0
			     :fill-pointer 0
			     :adjustable t
			     :element-type '(unsigned-byte 8))))
	 (line (make-array line-length :element-type '(unsigned-byte 8))))
    (check-type (ihdr-bit-depth ihdr) bit-depth)
    (check-type (ihdr-color-type ihdr) color-type)
    (check-type (ihdr-compression-method ihdr) (integer 0))
    (check-type (ihdr-compression-method ihdr) (integer 0))
    (check-type (ihdr-filter-method ihdr) (integer 0))
    (check-type (ihdr-interlace-method ihdr) (integer 0 1))

    ;; FIXME.  Convert to 8 bit for now, since we don't support
    ;; encoding 16 bit images
    (when (= source-bit-depth 16)
      (loop for i from 0 below (* width height)
	    for red = (ldb (byte 16 0) (row-major-aref source i))
	    for green = (ldb (byte 16 16) (row-major-aref source i))
	    for blue = (ldb (byte 16 32) (row-major-aref source i))
	    for alpha = (ldb (byte 16 48) (row-major-aref source i))
	    ;; FIXME.  Use with instead of for?
	    for result = 0
	    do
	    (setq result (dpb (ash red -8) (byte 8 0)
			      (dpb (ash green -8) (byte 8 8)
				   (dpb (ash blue -8) (byte 8 16)
					(dpb (ash alpha -8) (byte 8 24) result)))))
	    (setf (row-major-aref source i) result)))
      
    (unless (member (list color-type bit-depth)
		    +bit-depth-and-color-type-combination+ :test #'equal)
      (error "~&This combination of color-type (~A) and bit-depth (~A) is illegal."
	     color-type bit-depth))
    (debug-format-1 "~&Encoding image.~%")
    (when (>= *debug-level* 1) (print ihdr))
    (when (= color-type 3)
      (setq plte-hash-table (make-plte-hash-table source)))
    (if (= interlace-method 0)
	(progn
	  (loop for i from 0 below height
		do
		(setf (subseq idat (* i line-length) (* (1+ i) line-length))
		      (if plte-hash-table
			  (read-pixel source line i width
				      color-type bit-depth plte-hash-table)
			(read-pixel source line i width
				    color-type bit-depth))))
	  (loop for i from 0 below height
		with previous-line-index = nil
		with index = 0
		with filtered-line1 = (make-array line-length
						  :element-type '(unsigned-byte 8))
		with filtered-line2 = (make-array line-length
						  :element-type '(unsigned-byte 8))
		do
		(apply-filter idat filtered-line1 filter-type index previous-line-index
			      line-length (bytes-per-pixel ihdr))
		(when (> i 0)
		  (replace idat filtered-line2 :start1 (- index line-length)
			   :end1 index)
		  ;; Special case for the last line
		  (when (= i (1- height))
		    (replace idat filtered-line1 :start1 index
			     :end1 (+ line-length index))))
		(replace filtered-line2 filtered-line1)
		(setq previous-line-index index)
		(incf index line-length)))
          
	;; Interlace method 1
      (loop for j fixnum from 1 to 7
	    for x0 fixnum in '(0 4 0 2 0 1 0)
	    for dx fixnum in '(8 8 4 4 2 2 1)
	    for y0 fixnum in '(0 0 4 0 2 0 1)
	    for dy fixnum in '(8 8 8 4 4 2 2)
	    with bytes-per-pixel = (bytes-per-pixel ihdr)
	    ;; Add one because the first byte represents filter method
	    for line-length = (1+ (* (ceiling width dx) bytes-per-pixel))
	    with index fixnum = 0
	    do
;            (format t "~&----------------------~%")
;            (format t "~&Pass ~D~%" j)
;            (format t "~&line-length: ~D~%" line-length)
	    (loop for y from y0 by dy
		  until (>= y height)
		  do
		  ;; Don't use this method anyway? I think I have
		  ;; calculated the size of idat for interlaced
		  ;; images somewhere
		  (vector-push-extend filter-type idat)
		  (incf index)
		  (loop for x from x0 by dx
			until (>= x width) 
			do
			(loop for value in (multiple-value-list
					    (if plte-hash-table
						(read-pixel-interlaced source y x
								       color-type bit-depth
								       plte-hash-table)
					      (read-pixel-interlaced source y x
								     color-type bit-depth)))
			      do
			      (vector-push-extend value idat)
			      (incf index))))
;	      (format t "~&Ready to apply filter, index: ~D~%" index)
	    (loop for i from 0 below (ceiling height dy)
		  with filter-index = (- index (* line-length (ceiling height dy)))
		  with previous-line-index = nil
		  with filtered-line1 = (make-array line-length
						    :element-type '(unsigned-byte 8))
		  with filtered-line2 = (make-array line-length
						    :element-type '(unsigned-byte 8))
		  do
;                  (format t "~&i: ~D~%" i)
;                  (format t "~&filter-index: ~D~%" filter-index)
;                  (format t "~&Line ~D before filtering: ~A~%"
;                          i (subseq idat filter-index (+ filter-index line-length)))
		  (apply-filter idat filtered-line1 filter-type filter-index
				previous-line-index line-length
				bytes-per-pixel)
;		  (format t "~&Line ~D after filtering: ~A~%" i filtered-line1)
		  (when (> i 0)
;                    (format t "~&We are here:~%")
;                    (format t "~&filter-index: ~D~%" filter-index)
;                    (format t "~&line-length: ~D~%" line-length)
;                    (print idat)
		    (replace idat filtered-line2 :start1 (- filter-index line-length)
			     :end1 filter-index)
;		      (print idat)
		    ;; Special case for the last line
		    (when (= i (1- (ceiling height dy)))
		      (replace idat filtered-line1 :start1 filter-index
			       :end1 (+ line-length filter-index))))
		  (replace filtered-line2 filtered-line1)
		  (setq previous-line-index filter-index)
		  (incf filter-index line-length))))

    (debug-format-1 "~&Writing PNG signature.~%")
    (write-sequence +png-signature+ stream)
    (debug-format-1 "~&Writing IHDR chunk.~%")
    (write-ihdr stream ihdr)
    (when (= color-type 3)
      (debug-format-1 "~&Writing PLTE chunk.~%")
      (write-plte stream plte-hash-table))
    (debug-format-1 "~&Writing IDAT chunk.~%")
    (encode-idat idat #'write-idat stream)
    (debug-format-1 "~&Writing tEXt chunk.~%")
    (write-text stream text)
    (debug-format-1 "~&Writing IEND chunk.~%")
    (write-iend stream)
    (debug-format-1 "~&Image written to file ~A.~%" (pathname stream))))

(defun encode-file (source output-file &key (filter-type 4)
		    (bit-depth 8) (color-type 2) (source-bit-depth 8)
		    (interlace-method 0))
  (with-open-file (stream output-file
			  :direction :output
			  :if-exists :supersede
			  :element-type '(unsigned-byte 8))
    (encode-stream source stream :filter-type filter-type
		   :bit-depth bit-depth :color-type color-type
		   :source-bit-depth source-bit-depth
		   :interlace-method interlace-method)))

(defun write-32-bits-value (stream value)
  (write-byte (ldb (byte 8 24) value) stream)
  (write-byte (ldb (byte 8 16) value) stream)
  (write-byte (ldb (byte 8 8) value) stream)
  (write-byte (ldb (byte 8 0) value) stream))

(defun write-32-bits-to-array (array value &optional (start 0))
  (loop for i from 0 to 3
	do
	(setf (aref array (+ start i)) (ldb (byte 8 (* 8 (- 3 i))) value))))

(defun write-chunk (stream data &optional tag)
  (let ((length (if tag (length data) (- (length data) 4)))
        (checksum 0))
    (write-32-bits-value stream length)
    (when tag
      (write-sequence (map 'vector #'char-code tag) stream)
      (setf checksum (update-crc checksum tag)))
    (write-sequence data stream)
    (write-32-bits-value stream (update-crc checksum data))))

(defun write-idat (stream idat)
  (write-chunk stream idat "IDAT"))

(defun write-ihdr (stream ihdr)
  ;; Make an array consisting of the values in ihdr plus (13 bytes)
  ;; the chunk type (4 bytes)a
  (let ((ihdr-array (make-array (+ 13 4) :element-type '(unsigned-byte 8))))
    (setf (subseq ihdr-array 0 4) (map 'vector #'char-code "IHDR"))
    (write-32-bits-to-array ihdr-array (ihdr-width ihdr) 4)
    (write-32-bits-to-array ihdr-array (ihdr-height ihdr) 8)
    (setf (aref ihdr-array 12) (ihdr-bit-depth ihdr))
    (setf (aref ihdr-array 13) (ihdr-color-type ihdr))
    (setf (aref ihdr-array 14) (ihdr-compression-method ihdr))
    (setf (aref ihdr-array 15) (ihdr-filter-method ihdr))
    (setf (aref ihdr-array 16) (ihdr-interlace-method ihdr))
    (write-chunk stream ihdr-array)))

(defun write-iend (stream)
  (write-chunk stream #() "IEND"))

(defun write-plte (stream plte-hash-table)
  (let* ((entries (hash-table-count plte-hash-table))
	 (plte (make-array (+ (* 3 entries) 4) :element-type '(unsigned-byte 8))))
    (setf (subseq plte 0 4) (map 'vector #'char-code "PLTE"))
    (loop for value being the hash-values of plte-hash-table
	  for index = (first value)
	  for pixel-value = (second value)
	  do
;          (format t "~&index, pixel-value: ~D, ~D~%" index pixel-value)
;          (format t "~&R,G,B: ~D, ~D, ~D~%" (ldb (byte 8 0) pixel-value)
;                  (ldb (byte 8 8) pixel-value) (ldb (byte 8 16) pixel-value))
	  (setf (aref plte (+ 4 (* index 3))) (ldb (byte 8 0) pixel-value))
    	  (setf (aref plte (+ 5 (* index 3))) (ldb (byte 8 8) pixel-value))
       	  (setf (aref plte (+ 6 (* index 3))) (ldb (byte 8 16) pixel-value)))
    (write-chunk stream plte)))

(defun write-text (stream text)
  (let* ((keyword (first text))
	 (string (second text))
	 (length (+ (length keyword) 1 (length string)))
	 (text (make-array (+ length 4) :element-type '(unsigned-byte 8)
			   :initial-contents
    			   (concatenate '(vector (unsigned-byte 8) *)
					(map 'vector #'char-code "tEXt")
					(map 'vector #'char-code keyword)
                                        (make-array 1 :initial-element 0)
					(map 'vector #'char-code string)))))
    ;; FIXME. Check that keyword and string does not contain the null
    ;; character and that the length of them are not too large.
    (write-chunk stream text)))

(defun encode-idat (idat writer-function stream)
  (funcall writer-function
           stream
	   (string-to-byte-array
              	(zlib-from-cl-pdf:compress-string
		  (map 'string #'code-char idat)))))

(defun read-pixel-interlaced (source x y color-type bit-depth &optional plte-hash-table)
  "Read a pixel from source array and return it"
  (ecase color-type
    (0 (pixel-value source x y bit-depth 0))
    (2 (values (pixel-value source x y bit-depth 0)
	       (pixel-value source x y bit-depth 8)
	       (pixel-value source x y bit-depth 16)))
    (3 (first (gethash (ldb (byte 24 0) (aref source x y)) plte-hash-table)))
    (4 (values (pixel-value source x y bit-depth 0)
	       (pixel-value source x y bit-depth 24)))
    (6 (values (pixel-value source x y bit-depth 0)
	       (pixel-value source x y bit-depth 8)
	       (pixel-value source x y bit-depth 16)
	       (pixel-value source x y bit-depth 24)))))

(defun read-pixel (source line i width color-type
		   bit-depth &optional plte-hash-table)
  "Read each pixel from source array and write it to a scanline"
  (ecase color-type
    (0 (loop for j from 0 below (floor width (floor 8 bit-depth))
	     do
	     (setf (aref line (1+ j)) (pixel-value source i j bit-depth 0))))
    (2 (loop for j from 0 below (floor width (floor 8 bit-depth))
	     do
    	     (setf (aref line (1+ (* j 3))) (pixel-value source i j bit-depth 0))
	     (setf (aref line (+ (* j 3) 2)) (pixel-value source i j bit-depth 8))
	     (setf (aref line (+ (* j 3) 3)) (pixel-value source i j bit-depth 16))))
    (3 (loop for j from 0 below (floor width (floor 8 bit-depth))
	     do
	     (setf (aref line (1+ j))
		   (first (gethash (ldb (byte 24 0) (aref source i j)) plte-hash-table)))))
    (4 (loop for j from 0 below (floor width (floor 8 bit-depth))
	     do
    	     (setf (aref line (1+ (* j 2))) (pixel-value source i j bit-depth 0))
    	     (setf (aref line (+ (* j 2) 2)) (pixel-value source i j bit-depth 24))))
    (6 (loop for j from 0 below (floor width (floor 8 bit-depth))
	     do
  	     (setf (aref line (1+ (* j 4))) (pixel-value source i j bit-depth 0))
	     (setf (aref line (+ (* j 4) 2)) (pixel-value source i j bit-depth 8))
	     (setf (aref line (+ (* j 4) 3)) (pixel-value source i j bit-depth 16))
	     (setf (aref line (+ (* j 4) 4)) (pixel-value source i j bit-depth 24)))))
  line)

(defun pixel-value (source i j bit-depth weight)
;  (format t "~&items-per-byte: ~D~%" (floor 8 bit-depth))
;  (format t "~&i,j: ~D,~D~%" i j)
  (declare (type fixnum i j)
           (type (unsigned-byte 8) bit-depth weight)
           (optimize speed))
  (loop with result of-type (unsigned-byte 8) = 0
        with items-per-byte of-type (unsigned-byte 8) = (floor 8 bit-depth)
	for l of-type (unsigned-byte 32) from (* j items-per-byte) below (* (1+ j) items-per-byte)
	do
;        (format t "~&l: ~D~%" l)
;        (format t "~&byte-weight: ~D~%" (* bit-depth (mod l items-per-byte)))
;        (format t "~&source-value: ~D~%" (ldb (byte 8 weight) (aref source i l)))
	(setq result (dpb (ldb (byte 8 weight) (aref source i l))
			  (byte bit-depth
				(- 8 (* (mod l items-per-byte) bit-depth) bit-depth))
			  result))
	finally (return result)))

(defun make-plte-hash-table (source)
;  (debug-format-2 "~&height: ~D~%" (array-dimension source 0))
;  (debug-format-2 "~&widht: ~D~%" (array-dimension source 1))
  (loop with hash-table = (make-hash-table :test #'equal)
	with index = 0
	for i from 0 below (* (array-dimension source 0) (array-dimension source 1))
	for key = (ldb (byte 24 0) (row-major-aref source i))
	do
	(unless (gethash key hash-table)
	  (setf (gethash key hash-table) (list index key))
	  (incf index))
	finally	(progn
		  (debug-format-1 "~&Number of entries in PLTE: ~D~%" index)
		  (when (> index 255)
		    (error "Sorry. Too many entries in PLTE palette. You can't use PLTE for this image."))
		  (return
		    hash-table))))

(defun decode-test-png (input-file &optional (output-file t))
  (format t "~&")
  (format t "~&---------------------------------------------------------")
  (format t "~&Testing PNG file decoding with file: ~A.~%" input-file)
  (decode-file input-file :output-file output-file))

(defun decode-test-all ()
  (let ((output-file-number1 97)
	(output-file-number2 97))
    (dolist (k (directory "pictures/*.png"))
      (unless (char= (char (pathname-name k) 0) #\0)
	(format t "~&Now processing ~A.~%" k)
	(decode-test-png k
		  (concatenate
		   'string "test"
		   (make-string 1 :initial-element (code-char output-file-number1))
		   (make-string 1 :initial-element (code-char output-file-number2))
		   ".pnm"))
;	(sleep 10)
	(if (= output-file-number2 122)	    
	    (progn
	      (incf output-file-number1)
	      (setq output-file-number2 97))
	  (incf output-file-number2))))))

(defun encode-test-png (input-file &optional (output-file t))
  (format t "~&")
  (format t "~&---------------------------------------------------------")
  (format t "~&Testing PNG file encoding with input file: ~A.~%" input-file)
  (format t "~&Output to file: ~A.~%" output-file)
  (encode-file (decode-file input-file) output-file))

(defun encode-test-all ()
  (let ((output-file-number1 97)
	(output-file-number2 97))
    ;; Lispworks and Clisp lists the file in opposite order and when
    ;; testing I have files with errors in them as the last files ...
    (dolist (k #-(or lispworks clisp) (directory "pictures/*.png")
	       #+(or lispworks clisp) (reverse(directory "pictures/*.png")))
      (unless (char= (char (pathname-name k) 0) #\0)
	(format t "~&Now processing ~A.~%" k)
	(encode-test-png k
			 (concatenate
			  'string "test"
			  (make-string 1 :initial-element (code-char output-file-number1))
			  (make-string 1 :initial-element (code-char output-file-number2))
			  ".png"))
	(if (= output-file-number2 122)
	    (progn
	      (incf output-file-number1)
	      (setq output-file-number2 97))
	  (incf output-file-number2))))))
