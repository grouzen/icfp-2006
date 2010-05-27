;;;; UM-32 "Universal Machine" (http://boundvariable.org/um-spec.txt)
;;;; from icfp-2006.
;;;; Author: Nedokushev Michael <grouzen.hexy@gmail.com>

(defpackage #:um-32
    (:use :cl))

(in-package :um-32)

;;; Physical organization of the machine.
(defclass um-32 ()
  ((registers
    :initarg :registers
    :accessor registers
    :initform (make-array
               8
               :element-type '(unsigned-byte 32)
               :initial-element 0))
   (arrays
    :initarg :arrays
    :accessor arrays
    :initform (make-array
               64
               :fill-pointer 0
               :adjustable t))
   (power
    :initarg :power
    :initform t
    :accessor power)
   (finger
    :initarg finger
    :accessor finger
    :initform 0
    :type (unsigned-byte 32))))

(defmethod get-register ((um um-32) reg)
  (aref (registers um) reg))

(defmethod set-register ((um um-32) reg val)
  (setf (aref (registers um) reg) val))

;;; Operators.
(defmethod op0 ((um um-32) a b c)
  "The register A receives the value in register B,
unless the register C contains 0."
  (unless (= (get-register um c) 0)
    (set-register um a (get-register um b))))

(defmethod op1 ((um um-32) a b c)
  "The register A receives the value stored at offset
in register C in the array identified by B."
  (set-register
   um
   a
   (aref (aref (arrays um) (get-register um b)) (get-register um c))))

(defmethod op2 ((um um-32) a b c)
  "The array identified by A is amended at the offset
in register B to store the value in register C."
  (setf
   (aref (aref (arrays um) (get-register um a)) (get-register um b))
   (get-register um c)))

(defmethod op3 ((um um-32) a b c)
  "The register A receives the value in register B plus
the value in register C, modulo 2^32."
  (set-register um a (logand #xffffffff (+ (get-register um b)
                                           (get-register um c)))))

(defmethod op4 ((um um-32) a b c)
  "The register A receives the value in register B times
the value in register C, modulo 2^32."
  (set-register um a (logand #xffffffff(* (get-register um b)
                                          (get-register um c)))))

(defmethod op5 ((um um-32) a b c)
  "The register A receives the value in register b
divided by the value in register C, if any, where
each quantity is treated as an unsigned 32 bit number."
  (set-register um a (logand #xffffffff
                             (truncate (/ (get-register um b)
                                          (get-register um c))))))

(defmethod op6 ((um um-32) a b c)
  "Each bit in the register A receives the 1 bit if
either register B or register C has a 0 bit in that
position. Otherwise the bit in register A receives
the 0 bit."
  (set-register um a (logand
                      #xffffffff
                      (lognot (logand (get-register um b)
                                      (get-register um c))))))

(defmethod op7 ((um um-32) a b c)
  "The universal machine stops computation."
  (declare (ignore a b c))
  (setf (power um) nil))

(defmethod op8 ((um um-32) a b c)
  "A new array is created with a capacity of platters
commensurate to the value in the register C. This
new array is initialized entirely with platters
holding the value 0. A bit pattern not consisting of
exclusively the 0 bit, and that identifies no other
active allocated array, is placed in the B register."
  (declare (ignore a))
  (set-register um b
                (let ((arrs-length (length (arrays um)))
                      (new-arr (make-array (get-register um c)
                                           :initial-element 0)))
                  (do* ((i 0 (1+ i))
                        (arr (aref (arrays um) i) (aref (arrays um) i)))
                      (nil)
                    (cond ((> i arrs-length)
                           (vector-push-extend new-arr (arrays um)))
                          ((eq arr nil) (setf arr new-arr)))))))

(defmethod op9 ((um um-32) a b c)
  "The array identified by the register C is abandoned.
Future allocations may then reuse that identifier."
  (declare (ignore a b))
  (setf (aref (arrays um) (get-register um c)) nil))

(defmethod op10 ((um um-32) a b c)
  "The value in the register C is displayed on the console
immediatly. Only values between and including 0 and 255
are allowed."
  (declare (ignore a b))
  (let ((ch (get-register um c)))
    (when (> ch 255)
      (error "Character is too big ~d~%" c))
    (format t "~c" (code-char ch))
    (finish-output)))

(defmethod op11 ((um um-32) a b c)
  "The universal machine waits for input on the console.
When input arrives, the register C is loaded with the
input, which must be between and including 0 and 255.
If the end of input has been signaled, then the
register C is endowed with a uniform value pattern
where every place is pregnant with the 1 bit."
  (declare (ignore a b))
  (let ((in (char-code (read-char *standard-input* nil 'eof))))
    (if (eq in 'eof)
        (set-register um c #xffffffff)
        (if (> in 255)
            (error "Input has too big value ~d~%" in)
            (set-register um c in)))))

(defmethod op12 ((um um-32) a b c)
  "The array identified by the B register is duplicated
and the duplicate shall replace the '0' array,
regardless of size. The execution finger is placed
to indicate the platter of this array that is
described by the offset given in C, where the value
0 denotes the first platter, 1 the second, et
cettera.

The '0' array shall be the most sublime choise for
loading, and shall be handled with the utmost
velocity."
  (declare (ignore a))
  (let ((copy (copy-seq (aref (arrays um) (get-register um b)))))
    (setf (aref (arrays um) 0) copy)
    (setf (finger um) (get-register um c))))

(defmethod op13 ((um um-32) a val)
  "The value indicated is loaded into the register A
forthwith."
  (set-register um a val))

(defun um-run (codex-file)
  (let ((um (make-instance 'um))
        scroll end)
    (with-open-file (codex (merge-pathnames codex-file)
                           :if-does-not-exist :error
                           :element-type '(unsigned-byte 32))
      (setf scroll (make-array (file-length codex)
                               :element-type '(unsigned-byte 32))
            end (read-sequence scroll codex))
      (loop for i from 0 below end
            do (rotatef (ldb (byte 8 16) (aref scroll i))
                        (ldb (byte 8 8) (aref scroll i)))
            do (rotatef (ldb (byte 8 24) (aref scroll i))
                        (ldb (byte 8 0) (aref scroll i)))))
    (vector-push-extend scroll (arrays um))
    (let ((ops (list #'op0 #'op1 #'op2 #'op3 #'op4
                     #'op5 #'op6 #'op7 #'op8 #'op9
                     #'op10 #'op11 #'op12 #'op13))
          (platter 0)
          (a 0)
          (b 0)
          (c 0)
          (op 0))
      (loop
        (incf (finger um))
        (setf platter (aref (aref (arrays um) 0) (finger um))
              op (ldb (byte 4 28) platter))
        (cond ((= op 13)
               (funcall (nth op ops)
                        (ldb (byte 3 25) platter)
                        (ldb (byte 25 0) platter)))
              (t (setf a (ldb (byte 3 6) platter)
                       b (ldb (byte 3 3) platter)
                       c (ldb (byte 3 0) platter))
               (funcall (nth op ops) a b c)))))))
