(in-package #:clovetree-implementation)

;;; This type is defined primarily for code readibility and I'm not
;;; certain if it is conforming to use `satisfies' the way it is used
;;; in the commented out code block, hence definition is equivalent to
;;; an ordinary sequence.
(deftype sequence* (element-type)
  (declare (ignore element-type))
  `sequence)

;; (deftype sequence* (element-type)
;;   (let ((name (symbolicate element-type '-seq-p)))
;;     (eval-when (:compile-toplevel :load-toplevel :execute)
;;       (compile name
;;                `(lambda (seq)
;;                   (every (rcurry #'typep ',element-type) seq))))
;;     `(and sequence (satisfies ,name))))

(deftype note ()
  ;; sharp/flat pairs are equivalent to each other.
  `(member :c
           :c-sharp :d-flat
           :d
           :d-sharp :e-flat
           :e
           :f
           :f-sharp :g-flat
           :g
           :g-sharp :a-flat
           :a
           :a-sharp :b-flat
           :b))

;;; Number of semitones above the note C4.
(defun note-offset (note &optional (octave 4))
  (+ (ecase note
       ((:c)               0)
       ((:c-sharp :d-flat) 1)
       ((:d)               2)
       ((:d-sharp :e-flat) 3)
       ((:e)               4)
       ((:f)               5)
       ((:f-sharp :g-flat) 6)
       ((:g)               7)
       ((:g-sharp :a-flat) 8)
       ((:a)               9)
       ((:a-sharp :b-flat) 10)
       ((:b)               11))
     (* (- octave 4) 12)))

;;; Returns a frequency in hertz and MIDI note number.
;;; https://en.wikipedia.org/wiki/Scientific_pitch_notation
(defun frequency (note &optional (octave 4))
  (let* ((offset (note-offset note octave))
         (midi (+ offset 60))
         (frequency (* 440 (expt 2 (/ (- offset 9) 12)))))
    (values frequency midi)))

(defun frequency* (note-on)
  (* 440 (expt 2 (/ (- note-on 69) 12))))
