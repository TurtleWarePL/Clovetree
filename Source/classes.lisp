(in-package #:clovetree-implementation)

;;; Type annotations are provided mainly for readibiliy.

(defclass song-information ()
  ((title
    :type string
    :initarg :title
    :accessor title)
   (composer
    :type string
    :initarg :composer
    :accessor composer)
   (lyrics-author
    :type (or null string)
    :initarg :lyrics-author
    :accessor lyrics-author)
   (publishing-date
    :type (or null local-time:timestamp)
    :initarg :publishing-date
    :accessor publishing-date))
  (:default-initargs
   :title "(Untitled)"
   :composer "(Unknown)"
   :lyrics-author nil
   :publishing-date nil))

(defclass song (song-information)
  ((instruments
    :type (sequence* instrument)
    :initarg :instruments
    :accessor instruments)
   (parts
    :type (sequence* part)
    :initarg :parts
    :accessor parts)
   (views
    :type (sequence* parts-view)
    :initarg :views
    :accessor views))
  (:default-initargs :instruments nil
                     :parts nil
                     :views nil))

(defclass part ()
  ((name
    :type string
    :initarg :name
    :reader name)
   (instrument
    :type instrument
    :initarg :instrument
    :reader instrument)
   (staves
    ;:type hash-table
    :type (sequence* staff)
    :initarg :staves)
   ;; STUB internal representation of the music.
   (buffer     :type (sequence* speck))))

(defclass parts-view ()
  ((parts
    :type (sequence* part)
    :initarg :parts
    :reader parts)))

(defclass instrument ()
  ((name :type string
         :initarg :name
         :reader name)
   (key  :type note
         :initarg :key
         :reader key)))

(defclass staff ()
  ((type
    :type (member :five-line :percussion #|:guitar-tab :drum|#)
    :initarg :staff-type
    :reader staff-type)
   (name
    :type string
    :initarg :name
    :accessor name))
  ;; Clef and key signature usually appear at the beginning of the
  ;; piece but they may be appear later as well (i.e to cancel the
  ;; previous key signature), so they are not part of the staff.
  (:default-initargs
   :staff-type :five-line
   :name "default"))

;;; That's how I imagine the clef representation. It is commented out
;;; because it is not needed right now.
#+ (or)
(defclass clef ()
  ;; G is treble, F is bass (both have the "octave clef" variants)
  ((type
    :type (member :c
                  :g :g-1 :g-2 :g+1 :g+2
                  :f :g-1 :g-2 :g+1 :g+2
                  :neutral
                  #|:tabulature|#)
    :initarg :clef-type
    :reader clef-type)
   ;; Gsharp specifies line to be in range 2 to 6; I don't know why.
   (line
    :type integer
    :initarg :line
    :reader clef-line)))
