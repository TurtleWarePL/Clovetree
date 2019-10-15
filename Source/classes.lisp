(in-package #:clovetree-implementation)

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

(defclass song ()
  ((information :type song-information)
   (instruments :type (sequence* instrument))
   (parts       :type (sequence* part))
   (views       :type (sequence* parts-view))))

(defclass part ()
  ((name       :type string)
   (instrument :type instrument)))

(defclass parts-view ()
  ((parts :type (sequence part))))

(defclass instrument ()
  ((name :type string)
   (key  :type note)))
