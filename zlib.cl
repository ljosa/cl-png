;;; -*- Mode: Lisp; Package: User; -*-
;;;
;;; See the file README for copyright information.
;;;
;;; ZLIB implementation in Common Lisp.
;;; ZLIB is described in RFC 1950, DEFLATE in RFC 1951 

;;; BUGS:

; (encode-file (decode-test-png "pictures/basi4a08.png" nil) "temp"
; :color-type 4 :bit-depth 8 :filter-type 0) Works when we increase
; size of bit-stream arry by 200.  If we have no filtering we need
; extra space for headers and the filter byte in each line.  See below
; too.

;;;; encode-test-all fails on some of the small pictures. See below.


;; The bug below is because of the small size of the picture and the
;; fact that we need some bytes for DEFLATE header etc.

;Testing PNG file encoding with input file: /home/musum/lisp/png/pictures/s01i3p01.png.
;Output to file: testdx.png.
;Processing IHDR chunk.
;
;#S(IHDR
;     :WIDTH 1
;     :HEIGHT 1
;     :BIT-DEPTH 1
;     :COLOR-TYPE 3
;     :COMPRESSION-METHOD 0
;     :FILTER-METHOD 0
;     :INTERLACE-METHOD 1) 
;Processing gAMA chunk.
;gAMA processing not implemented. Gamma: 100000.
;Unsupported chunk type found.  Skipping chunk.
;Processing PLTE chunk.
;Processing IDAT chunk.
;IEND chunk found.  End of processing.
;Compressed with fixed Huffman codes.
;Decoding file: /home/musum/lisp/png/pictures/s01i3p01.png.
;Encoding image.
;
;#S(IHDR
;     :WIDTH 1
;     :HEIGHT 1
;     :BIT-DEPTH 8
;     :COLOR-TYPE 2
;     :COMPRESSION-METHOD 0
;     :FILTER-METHOD 0
;     :INTERLACE-METHOD 0) 
;Writing PNG signature.
;Writing IHDR chunk.
;Writing IDAT chunk.
;
;
;Error in function KERNEL::INVALID-ARRAY-INDEX-ERROR-HANDLER:
;   Invalid array index, 4 for #(120 156 99 97).  Should have been less than 4
;
;Restarts:
;  0: [ABORT] Return to Top-Level.
;
;Debug  (type H for help)
;
;(BIT-STREAM-WRITE-BIT #S(BIT-STREAM :BYTES #(120 156 99 97) :POSITION 32) 0)
;Source: 
;; File has been modified since compilation:
;;   /home/musum/lisp/png/zlib.cl
;; Using form offset instead of character position.
;(AREF (BIT-STREAM-BYTES BIT-STREAM) (ASH (BIT-STREAM-POSITION BIT-STREAM) -3))
;



;;;; TODO
;;;;
;;;; Use a hash-table in bit-list-from-value to speed it up?
;;;;
;;;;
;;;; Find the size of the result array in decode-buffer and create an
;;;; array of that size so we don't have to extend the array all the
;;;; time.  (but what if we want to use these functions for gzip. Can
;;;; the size of the result array be known a priori?).  Make the size
;;;; an optional parameter and do it with adjustable arrays if they
;;;; are not supplied?
;;;; 
;;;;
;;;; New algorithm and data structure in add-hash-value?
;;;;
;;;;
;;;; Should there be a user-defined variable that can change the block
;;;; size in the deflate algorithm?
;;;;
;;;;
;;;; Make a better algorithm for choosing the size of the array in
;;;; bit-stream in encode-buffer.
;;;;
;;;;
;;;; Implement dynamic huffman compression encoding.
;;;;
;;;; Memoization of fixed-huffman-code, distance-code-bits and similar
;;;; functions to speed things up?
;;;;
;;;;
;;;; size of hash-table in encode-fixed-huffman-block? (has impact on
;;;; performance. tune it!)
;;;;
;;;; The number of blocks used when encoding the buffer influences the
;;;; speed (probably because hash-table look-up and insertion takes
;;;; longer time with bigger blocks).
;;;;
;;;;
;;;;
;;;; Should I use a bit-stream for writing deflate blocks too?  What
;;;; should the initial bytes be set to?  adjustable array? Use a
;;;; bit-vector instead?
;;;;
;;;;
;;;; Write directly to output-stream instead of first to array and
;;;; then to output-stream?
;;;;
;;;; use a bitvector instead of a bit-list when encoding?


(defpackage #:zlib
  (:use #:cl)
  (:export #:encode-buffer
           #:decode-buffer))


(in-package #:zlib)


(eval-when (compile)
;  (declaim (optimize (speed 0) (safety 3) (space 1) (debug 3) #+cmu (ext:inhibit-warnings 3))))
  (declaim (optimize (speed 3) (safety 0) (space 1) (debug 1) #+cmu (ext:inhibit-warnings 3))))



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



(defconstant +zlib-major-version+ 0)
(defconstant +zlib-minor-version+ 1)


(defvar +fixed-huffman-code-lengths+
  (let ((array (make-array 288)))
    (loop for i from 0 to 143 do (setf (aref array i) 8))
    (loop for i from 144 to 255 do (setf (aref array i) 9))
    (loop for i from 256 to 279 do (setf (aref array i) 7))
    (loop for i from 280 to 287 do (setf (aref array i) 8))
    array)
  "The number of bits used to represent a code length")

(defvar +fixed-huffman-codes+
  (let ((array (make-array 288))
	(start1 #b00110000)
	(start2 #b110010000)
	(start3 #b0000000)
	(start4 #b11000000))
    (loop for i from 0 to 143 do (setf (aref array i) (+ start1 i)))
    (loop for i from 144 to 255 do (setf (aref array i) (+ start2 (- i 144))))
    (loop for i from 256 to 279 do (setf (aref array i) (+ start3 (- i 256))))
    (loop for i from 280 to 287 do (setf (aref array i) (+ start4 (- i 280))))
    array))


(defvar +fixed-huffman-code-bitlist+
  (loop with array = (make-array (length +fixed-huffman-codes+))
	for i from 0 below (length +fixed-huffman-codes+)
	do
	(loop with length fixnum = (aref +fixed-huffman-code-lengths+ i)
	      with result = '()
	      with code = (aref +fixed-huffman-codes+ i)
	      for j fixnum from 0 below length
	      do
	      (push (ldb (byte 1 j) code) result)
	      finally (setf (aref array i) result))
	finally (return array)))
	

(defconstant +huffman-end-of-block-symbol+ 256)


(defvar +dynamic-huffman-code-lengths-order+
  '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))

(defconstant +max-non-compressed-block-size+ 65535)

;; According to RFC 1951, minimum distance is 1 and minimum length is
;; 3, so a distance less than 3 does not make sense.  What am I
;; missing?
(defconstant +min-distance+ 3)
(defconstant +max-distance+ 32768)

(defconstant +min-length+ 3)
(defconstant +max-length+ 258)

(defconstant +deflate-max-hash-table-contents-length+ 5)


(defconstant +adler-base+ 65521)



(defvar +length-encoding+
  #((0 3) (0 4) (0 5) (0 6) (0 7) (0 8)
    (0 9) (0 10) (1 11) (1 13) (1 15) (1 17)
    (2 19) (2 23) (2 27) (2 31) (3 35) (3 43)
    (3 51) (3 59) (4 67) (4 83) (4 99) (4 115)
    (5 131) (5 163) (5 195) (5 227) (0 258))
  "Gives the relationship between a code, and extra bits and length")


(defvar +distance-encoding+
  #((0 1) (0 2) (0 3) (0 4) (1 5) (1 7)
    (2 9) (2 13) (3 17) (3 25) (4 33) (4 49)  
    (5 65) (5 97) (6 129) (6 193) (7 257) (7 385)
    (8 513) (8 769) (9 1025) (9 1537) (10 2049) (10 3073)
    (11 4097) (11 6145) (12 8193) (12 12289) (13 16385) (13 24577))
  "Gives the relationship between a code, and extra bits and distance")


(defstruct bit-stream
  (bytes)
  (position 0 :type fixnum))



;;; Functions that operate on a bit-stream struct

(defun bit-stream-read-reversed-bit (bit-stream)
  (prog1
      (ldb (byte 1 (- 7 (mod (bit-stream-position bit-stream) 8)))
           (aref (bit-stream-bytes bit-stream)
                 (ash (bit-stream-position bit-stream) -3)))
    (incf (bit-stream-position bit-stream))))



(defun bit-stream-read-reversed-bits (bit-stream n)
  (loop with result = 0
	for i from 0 below n
	do
	(setq result (logior result (ash (bit-stream-read-bit bit-stream) (1- (- n i)))))
	finally (return result)))



(defun bit-stream-read-bit (bit-stream)
  "Return the next bit from BIT-STREAM"
  (declare (optimize speed))
  (let ((position (bit-stream-position bit-stream))
	(array (bit-stream-bytes bit-stream)))
    ;; FIXME. Is position always a fixnum?
    (declare (type fixnum position)
	     (type (vector (unsigned-byte 8)) array))
    (prog1
	(ldb (byte 1 (mod position 8))
	     (aref array (ash position -3)))
      (incf (bit-stream-position bit-stream)))))


(defun bit-stream-read-bits (bit-stream n)
  "Read N bits from BIT-STREAM"
  (declare (optimize speed))
  (loop with result fixnum = 0
	for i of-type (unsigned-byte 8) from 0 below n
	for bit of-type (unsigned-byte 1) = (bit-stream-read-bit bit-stream)
	do
	(setq result (+ result (ash bit i)))
	finally (return result)))


(defun bit-stream-read-byte (bit-stream)
  "Read the next byte (8 bits) from BIT-STREAM"
  (prog1 (aref (bit-stream-bytes bit-stream)
	       (ash (bit-stream-position bit-stream) -3))
    (incf (bit-stream-position bit-stream) 8)))


(defun bit-stream-read-n-bytes (bit-stream n)
  "Read N bytes from BIT-STREAM"
  (loop with length = 0
        for i from 0 below n do
        (incf length (ash (bit-stream-read-byte bit-stream) (* 8 (- (1- n) i))))
        finally (return length)))


(defun bit-stream-read-length-and-distance (bit-stream
					    code
					    &optional (distance-huffman-tree nil))
  "Find the length and distance for CODE from BIT-STREAM and return them"
  (check-type code (integer 0 285))
  (values (+ (first (rest (aref +length-encoding+ (- code 257))))
             (bit-stream-read-bits bit-stream
                                   (first (aref +length-encoding+ (- code 257)))))
          (let ((distance-code (if distance-huffman-tree
				   (bit-stream-read-symbol
				    bit-stream
				    distance-huffman-tree)
				 (bit-stream-read-reversed-bits bit-stream 5))))
            (+ (first (rest (aref +distance-encoding+ distance-code)))
               (bit-stream-read-bits
		bit-stream
		(first (aref +distance-encoding+ distance-code)))))))



(defun bit-stream-read-symbol (bit-stream huffman-tree)
  "Read bits from BIT-STREAM and find the corresponding symbol in HUFFMAN-TREE"
  (loop
   until (atom huffman-tree)
   do
   (setf huffman-tree (if (zerop (bit-stream-read-bit bit-stream))
                          (car huffman-tree)
                        (cdr huffman-tree)))
   finally (return huffman-tree)))



(defun bit-stream-write-bit (bit-stream bit)
  "Write BIT to BIT-STREAM"
  (declare (type (unsigned-byte 1) bit)
           (optimize speed))
  (setf (aref (bit-stream-bytes bit-stream) (ash (bit-stream-position bit-stream) -3))
        (dpb bit (byte 1 (mod (bit-stream-position bit-stream) 8))
             (aref (bit-stream-bytes bit-stream)
                   (ash (bit-stream-position bit-stream) -3))))
  (incf (bit-stream-position bit-stream)))


(defun bit-stream-write-byte (bit-stream byte)
  "Write BYTE to BIT-STREAM"
  (setf (aref (bit-stream-bytes bit-stream)
	      (ash (bit-stream-position bit-stream) -3))
	byte)
  (incf (bit-stream-position bit-stream) 8))


(defun bit-stream-write-bits (bit-stream bit-list)
  "Write bits from BIT-LIST to BIT-STREAM"
  (loop for bit in bit-list
	do
	(bit-stream-write-bit bit-stream bit)))


(defun bit-stream-write-bits2 (bit-stream bit-vector)
  (loop for bit across bit-vector
	do
	(bit-stream-write-bit bit-stream bit)))


(defun bit-stream-pad-to-byte-boundary (bit-stream)
  "If necessary, pads the current byte in BIT-STREAM with zeroes"
  (unless (zerop (mod (bit-stream-position bit-stream) 8))
    (loop for i from (mod (bit-stream-position bit-stream) 8) below 8
	  do
	  (bit-stream-write-bit bit-stream 0))))


;;; Huffman coding utility functions

;; See section 3.2.2 in RFC 1951 for a description of the algorithm.
;; Some of the variable names are also from this section
(defun make-huffman-tree (huffman-code-lengths)
  "Create a Huffman tree from HUFFMAN-CODE-LENGTHS"
  (let* ((max-bits (reduce #'max huffman-code-lengths))
	 (huffman-tree '())
	 (bl-count (make-array (1+ max-bits) :initial-element 0))
	 (next-code (make-array (1+ max-bits) :initial-element 0))
	 (max-code (1- (length huffman-code-lengths))))
    (loop for i from 0 below (length huffman-code-lengths)
	  for code-length = (aref huffman-code-lengths i)
	  do
	  (unless (zerop code-length)
	    (incf (aref bl-count code-length))))
    (loop for bits fixnum from 1 to max-bits
	  with code = 0
	  do
	  (setq code (ash (+ code (aref bl-count (1- bits))) 1))
	  (setf (aref next-code bits) code))
    (loop for i fixnum from 0 to max-code
	  for len = (aref huffman-code-lengths i)
	  do
	  (unless (zerop len)
	    (setq huffman-tree (huffman-insert-element huffman-tree
						       len
						       (aref next-code len)
						       i))
	    (incf (aref next-code len))))
    huffman-tree))




(defun huffman-insert-element (tree length code symbol)
  "Insert SYMBOL into TREE"
  (cond ((= 0 length)
	 (assert (null tree))
	 symbol)
	((logbitp (1- length) code)
	 (unless (consp tree)
	   (setq tree (cons nil nil)))
	 (setf (cdr tree)
	       (huffman-insert-element (cdr tree) (1- length) code symbol))
	 tree)
	(t
	 (unless (consp tree)
	   (setq tree (cons nil nil)))
	 (setf (car tree)
	       (huffman-insert-element (car tree) (1- length) code symbol))
	 tree)))



(defun read-huffman-code-lengths (bit-stream huffman-tree items)
  (loop with result = (make-array items :initial-element 0)
	with i fixnum = 0
	with symbol
	until (= i items)
	do
	(setq symbol (bit-stream-read-symbol bit-stream huffman-tree))
	(case symbol
	  (16 (let ((count (+ 3 (bit-stream-read-bits bit-stream 2))))
		(loop for j from 0 below count
		      do
		      (setf (aref result (+ i j)) (aref result (- i 1))))
		(incf i count)))
	  (17 (let ((count (+ 3 (bit-stream-read-bits bit-stream 3))))
		(loop for j from 0 below count
		      do
		      (setf (aref result (+ i j)) 0))
		(incf i count)))
	  (18 (let ((count (+ 11 (bit-stream-read-bits bit-stream 7))))
		(loop for j from 0 below count
		      do
		      (setf (aref result (+ i j)) 0))
		(incf i count)))
	  (otherwise
	   (setf (aref result i) symbol)
	   (incf i)))
	  finally (return result)))



(defun fixed-huffman-code (symbol)
  "Return the code for the given SYMBOL."
  (declare (type fixnum symbol)
	   (optimize speed))
  (loop with length fixnum = (aref +fixed-huffman-code-lengths+ symbol)
	with result = '()
	with code = (aref +fixed-huffman-codes+ symbol)
	for i fixnum from 0 below length
	do
	(push (ldb (byte 1 i) code) result)
	finally (return result)))



(defun fixed-huffman-code2 (symbol)
  (declare (type fixnum symbol)
	   (optimize speed))
  (loop with length fixnum = (aref +fixed-huffman-code-lengths+ symbol)
	with result = (make-array length :element-type '(unsigned-byte 1))
	with code = (aref +fixed-huffman-codes+ symbol)
	for i fixnum from 0 below length
	do
	(setf (aref result i) (ldb (byte 1 i) code))
	finally (return result)))



;; FIXME. It should be possible to do this smarter
(defun distance-code (distance)
  "Return the distance-code for a given DISTANCE"
  (cond ((< distance 5) (1- distance))
	((<= 5 distance 6) 4)
	((<= 7 distance 8) 5)
	((<= 9 distance 12) 6)
	((<= 13 distance 16) 7)
	((<= 17 distance 24) 8)
	((<= 25 distance 32) 9)
	((<= 33 distance 48) 10)
	((<= 49 distance 64) 11)
	((<= 65 distance 96) 12)
	((<= 97 distance 128) 13)
	((<= 129 distance 192) 14)
	((<= 193 distance 256) 15)
	((<= 257 distance 384) 16)
	((<= 385 distance 512) 17)
	((<= 513 distance 768) 18)
	((<= 769 distance 1024) 19)
	((<= 1025 distance 1536) 20)
	((<= 1537 distance 2048) 21)
	((<= 2049 distance 3072) 22)
	((<= 3073 distance 4096) 23)
	((<= 4097 distance 6144) 24)
	((<= 6145 distance 8192) 25)
	((<= 8193 distance 12288) 26)
	((<= 12289 distance 16384) 27)
	((<= 16385 distance 24576) 28)
	((<= 24577 distance 32768) 29)
	(t (error "A distance larger than 32768 is illegal"))))


;; FIXME. It should be possible to do this smarter
(defun length-code (length)
  "Return the length-code for a given LENGTH"
  (cond ((<= length 10) (+ 254 length))
	((<= 11 length 12) 265)
	((<= 13 length 14) 266)
	((<= 15 length 16) 267)
	((<= 17 length 18) 268)
	((<= 19 length 22) 269)
	((<= 23 length 26) 270)
	((<= 27 length 30) 271)
	((<= 31 length 34) 272)
	((<= 35 length 42) 273)
	((<= 43 length 50) 274)
	((<= 51 length 58) 275)
	((<= 59 length 66) 276)
	((<= 67 length 82) 277)
	((<= 83 length 98) 278)
	((<= 99 length 114) 279)
	((<= 115 length 130) 280)
	((<= 131 length 162) 281)
	((<= 163 length 194) 282)
	((<= 195 length 226) 283)
	((<= 227 length 257) 284)
	((= length 258) 285)
	(t (error "A length larger than 258 is illegal"))))



(defun distance-code-bits (code)
  "Return a list with 5 elements that are the binary representation of CODE."
  (loop with length = 5
	with result = '()
	for i from 0 below length
	do
	(push (ldb (byte 1 i) code) result)
	finally (return result)))


(defun distance-code-bits2 (code)
  (loop with result = (make-array 5 :element-type '(unsigned-byte 1))
	for i from 0 below 5
	do
	(setf (aref result i) (ldb (byte 1 i) code))
	finally (return result)))



(defun extra-distance-bits (distance)
  "The number of extra distance bits that are needed for a given DISTANCE."
  (max (- (integer-length (- distance 1)) 2) 0))


(defun extra-length-bits (length)
  "The number of extra length bits that are needed for a given DISTANCE."
  (max (- (integer-length (- length 3)) 3) 0))


(defun bit-list-from-value (value length)
  "Return a list with a bit representation of VALUE. The list has
LENGTH elements"
  (loop with result = '()
	for i from 0 below length
	do
	(push (ldb (byte 1 i) value) result)
	finally (return (nreverse result))))



;; FIXME. Shouldn't the result be reversed here too?
(defun bit-vector-from-value (value length)
  (loop with bit-vector = (make-array length :element-type '(unsigned-byte 1))
	for i from 0 below length
	do
	(setf (aref bit-vector i) (ldb (byte 1 i) value))
	finally (return bit-vector)))





(defun read-32-bits-from-array (array)
  "Read a 32-bit word from ARRAY, MSB first"
  (loop with length = 0
        for i from 0 below 4 do
        (incf length (ash (aref array i) (* 8 (- 3 i))))
        finally (return length)))


;;; Checksum functions

(defun update-adler-32 (adler buffer)
  (declare (type (vector (unsigned-byte 8)) buffer)
           (type (unsigned-byte 32) adler)
           (optimize speed))
  (loop for i fixnum from 0 below (length buffer)
	with s1 of-type (unsigned-byte 32) = (logand adler #xffff)
	with s2 of-type (unsigned-byte 32) = (logand (ash adler -16) #xffff)
	do
	(setq s1 (mod (+ s1 (aref buffer i)) +adler-base+))
	(setq s2 (mod (+ s2 s1) +adler-base+))
	finally (return (+ (ash s2 16) s1))))



(defun adler-32 (buffer)
  "Compute Adler-32 checksum of BUFFER.  Based on the sample code in
appendix C of RFC 1950. update-adler-32 does all the work"
  (update-adler-32 1 buffer))





(defun decode-buffer (buffer size)
  "Decode BUFFER and return a vector with the result"
  (declare (type (vector (unsigned-byte 8)) buffer)
           (optimize speed))
  (loop with bit-stream = (make-bit-stream :bytes buffer :position 0)
	with cmf fixnum = (bit-stream-read-byte bit-stream)
	with flg fixnum = (bit-stream-read-byte bit-stream)
	for bfinal fixnum = (bit-stream-read-bits bit-stream 1)
	for btype fixnum = (bit-stream-read-bits bit-stream 2)
	with adler-32 = (read-32-bits-from-array (subseq buffer (- (length buffer) 4)))
	with result = (make-array size
;				  :adjustable t
				  :fill-pointer 0
				  :element-type '(unsigned-byte 8))
	do
;        (format t "~&size: ~D~%" size)
;        (format t "~&cmf: ~D~%" cmf)
;        (format t "~&flg: ~D~%" flg)
	(check-type (ldb (byte 4 0) cmf) (integer 8))
	(check-type (ldb (byte 1 5) flg) (integer 0))
;          (debug-format-1 "~&Compression method: ~D~%" (ldb (byte 4 0) cmf))
;          (debug-format-1 "~&Compression info: ~D~%" (ldb (byte 4 4) cmf))
;          (debug-format-1 "~&FCHECK: ~D~%" (ldb (byte 4 0) flg))
;          (debug-format-1 "~&FDICT: ~D~%" (ldb (byte 1 5) flg))
;          (debug-format-1 "~&FLEVEL: ~D~%" (ldb (byte 2 6) flg))
;          (debug-format-1 "~&BFINAL: ~D~%" bfinal)
;          (debug-format-1 "~&BTYPE: ~D~%" btype)
	(assert (zerop (mod (+ (* 256 cmf) flg) 31)))
	(cond
	 ((= btype 0)
	  (debug-format-1 "~&Data in non-compressed format.~%")
	  (decode-non-compressed-block bit-stream result))
	 ((= btype 1)
	  (debug-format-1 "~&Compressed with fixed Huffman codes.~%")
	  (decode-fixed-huffman-block bit-stream result))
	 ((= btype 2)
	  (debug-format-1 "~&Compressed with dynamic Huffman codes.~%")
	  (decode-dynamic-huffman-block bit-stream result))
	 (t (error "Data compression method not recognised.")))
	(debug-format-2 "~&End of block symbol found. Deflate block processed.~%")
	(when (= bfinal 1)
	  (unless (= (adler-32 result) adler-32)
	    (error "Adler-32 checksum error"))
	  (return result))))


(defun decode-non-compressed-block (bit-stream result)
  "Decode one non-compressed block in BIT-STREAM and store the result
in RESULT"
  (let ((junk (bit-stream-read-bits bit-stream 5))
	(len (+ (bit-stream-read-byte bit-stream)
		(ash (bit-stream-read-byte bit-stream) 8)))
	(nlen (+ (bit-stream-read-byte bit-stream)
		 (ash (bit-stream-read-byte bit-stream) 8))))
    (declare (ignore junk)
	     (type (vector (unsigned-byte 8)) result)
	     (optimize speed))
    (assert (= (logand #xffff (lognot nlen)) len))
    (dotimes (i len)
      (vector-push (bit-stream-read-byte bit-stream) result))))
;      (setf (aref result i) (bit-stream-read-byte bit-stream)))
;      (vector-push-extend (bit-stream-read-byte bit-stream) result))))




(defun decode-fixed-huffman-block (bit-stream result)
  "Decode one block in BIT-STREAM with fixed Huffman coding and store the result in
RESULT."
  (declare (type (vector (unsigned-byte 8)) result)
           (optimize speed))
  (loop with huffman-tree = (make-huffman-tree +fixed-huffman-code-lengths+)
	with i fixnum = (fill-pointer result)
	for symbol fixnum = (bit-stream-read-symbol
			     bit-stream
			     huffman-tree)
	until (= symbol +huffman-end-of-block-symbol+)
	do
;	(format t "~&Symbol: ~A" symbol)
;        (format t "~&Current byte: ~D"
;                (aref (bit-stream-bytes bit-stream)
;                      (ash (bit-stream-position bit-stream) -3)))
;	(format t "~&result array index: ~A" i)
	(if (<= symbol 255)
	    (progn 
;		(setf (aref result i) symbol)
;	      (vector-push-extend symbol result)
	      (vector-push symbol result)
	      (incf i))
	  (multiple-value-bind (length distance)
	      (bit-stream-read-length-and-distance bit-stream symbol)
	    (declare (type fixnum length distance))
;              (format t "~&length: ~A" length)
;              (format t "~&distance: ~A" distance)
	    (loop for j fixnum from 0 below length
		  with source-index fixnum = (- i distance)
		  do
;		    (format t "~&index: ~D" (+ (mod j distance) source-index))
;                    (setf (aref result i)
;                          (logand (aref result (+ (mod j distance) source-index)) #xff))
;                  (vector-push-extend
;                   (logand (aref result (+ (mod j distance) source-index)) #xff)
;                   result)
		  (vector-push
                   (logand (aref result (+ (mod j distance) source-index)) #xff)
                   result)
		  (incf i))))))




(defun decode-dynamic-huffman-block (bit-stream result)
  "Decode one block in BIT-STREAM with dynamic Huffman coding and
store the result in RESULT"
  (declare (optimize speed))
  (let ((hlit (+ (bit-stream-read-bits bit-stream 5) 257))
	(hdist (+ (bit-stream-read-bits bit-stream 5) 1))
	(hclen (+ (bit-stream-read-bits bit-stream 4) 4))
        (code-lengths (make-array 19 :initial-element 0))
	literal-huffman-tree distance-huffman-tree
	code-length-huffman-tree)
;    (format t "~&HLIT: ~D" hlit)
;    (format t "~&HDIST: ~D" hdist)
;    (format t "~&HCLEN: ~D" hclen)
    (loop for i fixnum from 1 to hclen
	  for j in +dynamic-huffman-code-lengths-order+
	  do
	  (setf (aref code-lengths j) (bit-stream-read-bits bit-stream 3)))
;    (format t "~&code-lengths ~A" code-lengths)
    (setq code-length-huffman-tree (make-huffman-tree code-lengths))
;    (format t "~&Code length Huffman tree ~A" code-length-huffman-tree)
    (setq literal-huffman-tree (make-huffman-tree
				(read-huffman-code-lengths
				 bit-stream
				 code-length-huffman-tree
				 hlit)))
;    (format t "~&Literal/length Huffman tree ~A" literal-huffman-tree)
;    (format t "~&Position in bit-stream tree ~D" (bit-stream-position bit-stream))
    (setq distance-huffman-tree (make-huffman-tree
				 (read-huffman-code-lengths
				  bit-stream
				  code-length-huffman-tree
				  hdist)))
;    (format t "~&Position in bit-stream tree ~D" (bit-stream-position bit-stream))
;    (format t "~&Distance Huffman tree ~A" distance-huffman-tree)
    (loop for symbol fixnum = (bit-stream-read-symbol bit-stream literal-huffman-tree)
;	  with i fixnum = 0
	  with i fixnum = (fill-pointer result)
	  until (= symbol +huffman-end-of-block-symbol+)
	  do
;          (format t "~&Symbol: ~A" symbol)
;          (format t "~&Current byte: ~D"
;                  (aref (bit-stream-bytes bit-stream)
;                        (ash (bit-stream-position bit-stream) -3)))
;          (format t "~&bit-position: ~D" (bit-stream-position bit-stream))
;          (format t "~&result array index: ~A" i)
	  (if (<= symbol 255)
	      (progn
;		(format t "~&fill-pointer: ~D" (fill-pointer result))
;		(vector-push-extend symbol result)
		(vector-push symbol result)
;		(setf (aref result i) symbol)
		(incf i))
	    (multiple-value-bind (length distance) (bit-stream-read-length-and-distance
						    bit-stream
						    symbol
						    distance-huffman-tree)
	      (declare (type fixnum length distance))
;              (format t "~&length: ~A" length)
;              (format t "~&distance: ~A" distance)
;	      (loop for j fixnum from (fill-pointer result) below (+ length (fill-pointer result))
	      (loop for j fixnum from 0 below length
		    with source-index fixnum = (- i distance)
		    do
;                    (format t "~&j: ~D" j)
;                    (format t "~&index: ~D" (+ (mod j distance) source-index))
;                    (vector-push-extend
;                     (logand (aref result (+ (mod j distance) source-index)) #xff)
;                     result)
                    (vector-push
                     (logand (aref result (+ (mod j distance) source-index)) #xff)
                     result)
;                    (setf (aref result i)
;                          (logand (aref result (+ (mod j distance) source-index)) #xff))
		    (incf i)))))))



(defun encode-buffer (buffer btype writer-function stream)
  "Encode BUFFER with deflate algorithm of type BTYPE.
WRITER-FUNCTION should be a function that can write a block at a time
to STREAM"
  (loop with cmf fixnum = 120
	with flg fixnum = 156
	with adler-32 = (adler-32 buffer)
	with blocks = (number-of-blocks buffer btype)
	with buffer-length = (length buffer)
	with block-size fixnum = (ceiling buffer-length blocks)
	;; FIXME. Now chooses the size of the array in bit-stream
	;; based on block-size (assumes worst case of no compression).
	;; For small pictures this is not always enough, because you
	;; need some bytes for DEFLATE header etc.
	with bit-stream = (make-bit-stream
			   :bytes (make-array
				   (if (= btype 0)
				       (+ +max-non-compressed-block-size+ 11)
				     (+ block-size 200))
				   :element-type '(unsigned-byte 8))
			   :position 0)
	with hash-table = (unless (= btype 0) (make-hash-table :test 'equalp))
	with bfinal = 0
	for i fixnum from 0 below blocks
	for start = (* i block-size)
        for end = (* (1+ i) block-size)
	with last-bits = nil
	do
;        (format t "~&buffer-length: ~D~%" buffer-length)
;        (format t "~&block-size: ~D~%" (+ block-size 200))
;        (format t "~&start: ~D~%" start)
;        (format t "~&end: ~D~%" end)
;        (format t "~&number of blocks: ~D~%" blocks)
;        (format t "~&block number: ~D~%" i)
	(when (= i (1- blocks))
	  (setq bfinal 1))
;        (format t "~&cmf: ~D~%" cmf)
;        (format t "~&flg: ~D~%" flg)
	(check-type (ldb (byte 4 0) cmf) (integer 8))
	(check-type (ldb (byte 1 5) flg) (integer 0))
;        (format t "~&Compression method: ~D~%" (ldb (byte 4 0) cmf))
;        (format t "~&Compression info: ~D~%" (ldb (byte 4 4) cmf))
;        (format t "~&FCHECK: ~D~%" (ldb (byte 4 0) flg))
;        (format t "~&FDICT: ~D~%" (ldb (byte 1 5) flg))
;        (format t "~&FLEVEL: ~D~%" (ldb (byte 2 6) flg))
;        (format t "~&BFINAL: ~D~%" bfinal)
;        (format t "~&BTYPE: ~D~%" btype)
	(assert (zerop (mod (+ (* 256 cmf) flg) 31)))	
	(when (= i 0)
	  (bit-stream-write-byte bit-stream cmf)
	  (bit-stream-write-byte bit-stream flg))
	;; If there are any bits left from the previous block that are
	;; not written yet, write them out now
	(when last-bits
	    (bit-stream-write-bits bit-stream last-bits)
	    (setq last-bits nil))
	(bit-stream-write-bits bit-stream (list bfinal))
	(cond
	 ((= btype 0)
;	  (format t "~&Use non-compressed format~%")
	  (bit-stream-write-bits bit-stream (list 0 0))
	  (bit-stream-pad-to-byte-boundary bit-stream)
	  (encode-non-compressed-block bit-stream
				       buffer
				       start
				       (min end buffer-length)))
	 ((= btype 1)
;	  (format t "~&Compressing with fixed Huffman codes~%")
	  (bit-stream-write-bits bit-stream (list 1 0))
          (encode-fixed-huffman-block bit-stream
				      buffer
				      hash-table
				      start (min end buffer-length)))
	 ((= btype 2)
;	  (format t "~&Compressing with dynamic Huffman codes~%")
	  (bit-stream-write-bits bit-stream (list 0 1))
	  (encode-dynamic-huffman-block bit-stream buffer))
	 (t (error "Data compression method not recognised")))
	;; If this is the last block, write buffer and return
	(when (= bfinal 1)
          (bit-stream-pad-to-byte-boundary bit-stream)
          (loop for j fixnum from 0 to 3
                do
                (bit-stream-write-byte bit-stream
                                       (ldb (byte 8 (* 8 (- 3 j))) adler-32)))
	  (funcall writer-function
		   stream
		   (subseq (bit-stream-bytes bit-stream) 0
			   (ash (bit-stream-position bit-stream) -3)))
	  (return t))	
	;; If this is not the last block, write buffer and reset array
	;; but do not write the bits in the last byte, because it may
	;; not be completely filled.
	(funcall writer-function
		 stream
		 (subseq (bit-stream-bytes bit-stream) 0
			 (ash (bit-stream-position bit-stream) -3)))
	;; Start using the array from the beginning again but first
	;; save the bits in the last byte in last-bits
	(setq last-bits (bit-list-from-value
			 (aref (bit-stream-bytes bit-stream)
			       (ash (bit-stream-position bit-stream) -3))
			 (mod (bit-stream-position bit-stream) 8)))
	(setf (bit-stream-position bit-stream) 0)))


(defun number-of-blocks (buffer btype)
  "Return the number of blocks that should be used to encode the BUFFER."
  (case btype
    ;; FIXME. This is a requirement, might adjust it later (the blocks
    ;; can be smaller)
    (0 (ceiling (length buffer) +max-non-compressed-block-size+))
    ;; FIXME. This is just a value to test with
    (1 (ceiling (length buffer) (floor +max-non-compressed-block-size+ 4)))
    (2 1)))



(defun find-best-match (buffer match index)
  "Searches all elements in MATCH to find the one with the lowest
position. INDEX is the index to the current position in BUFFER"
  (declare (type (vector (unsigned-byte 8)) buffer)
           (type fixnum index)
           (optimize speed))
  (unless match
    (values nil nil))
  (let ((length 0)
        (temp-length nil)
        (temp-position nil)
        (final-position nil))
    (dolist (position match)
      (setq temp-position (mismatch buffer buffer
                                    :start1 position
                                    :end1 (min index (+ position +max-length+))
                                    :start2 index
                                    :test 'equal))
      ;; If the length is less than 3 I can discard this match anyway,
      ;; regardless if it's a wrong match or the length is too short.      
;      (print match)
;      (format t "~&index: ~D" index)
;      (format t "~&position: ~D" position)
;      (format t "~&temp-position: ~A" temp-position)
      (if temp-position
          (when (>= (- temp-position position) 3)
            (setq temp-length (- temp-position position))
            (when (> temp-length length)
              (setq length temp-length)
              (setq final-position position)))
        ;; Full match.  Calculate length.
        (progn
          (setq temp-length (min (- (length buffer) index)
                                 (- (min index (+ position +max-length+)) position)))
         (format t "~&Full match")
          (when (and (> temp-length length) (>= temp-length 3))
            (setq length temp-length)
            (setq final-position position))))
      (when (>= length +max-length+)
        (return-from find-best-match (values final-position length))))
    (values final-position length)))

  


(defun add-hash-value (hash-table hash-value index)
  "Remove oldest hash-value index from HASH-TABLE (if necessary) and
push INDEX into HASH-TABLE"
  (declare (type fixnum index)
	   (optimize speed))
  (let ((values (gethash hash-value hash-table)))
    (if (>= (length values) +deflate-max-hash-table-contents-length+)
	(progn
;	  (replace values values :start1 1 :start2 0)
	  (setq values (subseq values 0 (1- +deflate-max-hash-table-contents-length+)))
;	  (setq values (nbutlast values))
	  (push index values)
	  (setf (gethash hash-value hash-table) values))
      (push index (gethash hash-value hash-table)))))


(defun encode-non-compressed-block (bit-stream buffer start end)
  "Encode a DEFLATE block using the non-compressing method"
  (declare (type (vector (unsigned-byte 8)) buffer)
	   (type fixnum start end)
	   (optimize speed))
  (let* ((len (- end start))
	 (nlen (logand #xffff (lognot len))))
    (declare (type (unsigned-byte 32) len nlen)
             (optimize speed))
    (assert (= (logand #xffff (lognot nlen)) len))
    (bit-stream-write-byte bit-stream (ldb (byte 8 0) len))
    (bit-stream-write-byte bit-stream (ldb (byte 8 8) len))
    (bit-stream-write-byte bit-stream (ldb (byte 8 0) nlen))
    (bit-stream-write-byte bit-stream (ldb (byte 8 8) nlen))
    (dotimes (i len)
      (bit-stream-write-byte bit-stream (aref buffer (+ i start))))))



(defun encode-fixed-huffman-block (bit-stream buffer hash-table start end)
  "Encode a DEFLATE block using the fixed Huffman code method"
  (declare (type (vector (unsigned-byte 8)) buffer)
           (optimize speed))
;  (format t "~&start: ~D~%" start)
;  (format t "~&end: ~D" end)
  (loop with i = start
;	with buffer-length = (- end start)
	with values = (make-array 3 :element-type '(unsigned-byte 8))
	with symbol 
	with bit-list
;	with position
;       with length
        with distance
	with length-code
	with distance-code
	with hash-value
	with match
	until (>= i (- end 2))
;	for symbol = (aref buffer i)
;	for bit-list = (fixed-huffman-code symbol)
;	for bit-list = (aref +fixed-huffman-code-bitlist+ symbol)
;       for hash-value = (replace values buffer :start2 i :end2 (+ i 3))
;       for match = (gethash hash-value hash-table)
;	for position = (first match)
;       for position = (find-best-match buffer match i)
;       for distance = (when position (- i position))
	do
	(setq symbol (aref buffer i))
	(setq bit-list (aref +fixed-huffman-code-bitlist+ symbol))
	(setq hash-value (replace values buffer :start2 i :end2 (+ i 3)))
	(setq match (gethash hash-value hash-table))		
        (multiple-value-setq (position length) (find-best-match buffer match i))
        (when position
          (setq distance (- i position)))
	(when (and match (not position))
	  ;; We didn't have a match after all!
	  (setq match nil))
;          (format t "~&Match is T, but position is nil!~%"))
;          (print match)
;          (format t "~&position: ~D~%" position))	
;        (format t "~&i: ~D~%" i)
;        (when (or (= i 4) (= i 96))
;          (format t "~&match: ~A~%" match)
;          (format t "~&position: ~D~%" position)
;          (format t "~&length: ~D" length)
;          (format t "~&symbol: ~A~%" symbol))
;        (print hash-value)
;        (push i (gethash hash-value hash-table))
	(add-hash-value hash-table hash-value i)
;	   (print (gethash hash-value hash-table)))
	(if (and match
		 (>= distance +min-distance+)
		 (<= distance +max-distance+))
	    (progn (when (or (null length) (> length +max-length+))
		     (setq length +max-length+))
		   (when (> length (- end i))
		     (setq length (- end i)))
;              (format t "~&Values starting at position ~D: ~A" position values)
;              (format t "~&Distance: ~D" distance)
;              (format t "~&Length: ~D" length)
;              (format t "~&Position: ~D" position)

		   ;; Find the code representing the length
		   (setq length-code (length-code length))
;              (format t "~&length-code: ~A" length-code)
;              (format t " ~&length-code bits: ~A"
;                        (fixed-huffman-code length-code))
		   ;; Write the length
;	           (bit-stream-write-bits bit-stream (fixed-huffman-code length-code))
		   (bit-stream-write-bits bit-stream
					  (aref +fixed-huffman-code-bitlist+ length-code))
		   ;; Additional length bits
;              (format t "~&Extra length bits: ~D"
;                      (extra-length-bits length))
;              (format t "~&Extra length bits list: ~A"
;                      (bit-list-from-value
;                       (- length
;                          (first (rest (aref +length-encoding+ (- length-code 257)))))
;                       (extra-length-bits length)))
		   ;; Write out extra length bits if there are any
		   (when (<= 265 length-code 284)
		     (bit-stream-write-bits
		      bit-stream
		      (bit-list-from-value
		       (- length (first (rest (aref +length-encoding+ (- length-code 257)))))
		       (extra-length-bits length)))
		     )
		   ;; Write 5 bits that represent the distance code
		   (setq distance-code (distance-code distance))
;              (format t "~&distance-code: ~A" distance-code)
;              (format t "~&distance-code bits: ~A"
;                      (distance-code-bits distance-code))
		   (bit-stream-write-bits bit-stream (distance-code-bits distance-code))
		   ;; Additional distance bits
;              (format t "~&Extra distance bits: ~D"
;                      (extra-distance-bits distance))
;              (format t "~&Extra distance bits list: ~A"
;                      (bit-list-from-value
;                       (- distance
;                          (first (rest (aref +distance-encoding+ distance-code))))
;                       (extra-distance-bits distance)))
		   ;; Write extra distance bits if there are any
		   (when (>= distance-code 4)
		     (bit-stream-write-bits
		      bit-stream
		      (bit-list-from-value
		       (- distance (first (rest (aref +distance-encoding+ distance-code))))
		       (extra-distance-bits distance)))
;                (bit-stream-write-bits2
;                 bit-stream
;                 (bit-vector-from-value
;                  (- distance (first (rest (aref +distance-encoding+ distance-code))))
;                  (extra-distance-bits distance)))
		     )
		   (incf i length))
	(progn
	  (bit-stream-write-bits bit-stream bit-list)	  
	  (incf i)))
	;; Check if we wrote out all bytes from buffer, else write out
	;; the remaining ones.
	finally (when (< i end)
;                  (format t "~&Write out more.~%")
;                  (format t "~&i: ~D" i)
		  (loop for j fixnum from 0 below (- end i)
			for bit-list = (fixed-huffman-code (aref buffer (+ i j)))
			do
;		  (format t "~&Writing out one symbol.~%")
			(bit-stream-write-bits bit-stream bit-list))))
  (bit-stream-write-bits bit-stream (fixed-huffman-code +huffman-end-of-block-symbol+)))




(defun encode-dynamic-huffman-block (bit-stream buffer)
  (error "Not implemented yet."))

