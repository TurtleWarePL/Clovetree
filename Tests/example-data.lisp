(in-package #:clovetree-implementation)

(defparameter *staves*
  `((:piano-bass   . ,(make-instance 'staff :name "Bass"   :staff-type :five-line))
    (:piano-treble . ,(make-instance 'staff :name "Treble" :staff-type :five-line))
    (:guitar       . ,(make-instance 'staff :name "Treble" :staff-type :five-line))))

(defparameter *instruments*
  `((:piano  . ,(make-instance 'instrument :name "Piano"))
    (:violin . ,(make-instance 'instrument :name "Violin"))))

(defparameter *parts*
  `((:first-violin . ,(make-instance 'part :name "First violin"
                                           :instrument (assoc-value *instruments* :violin)))
    (:piano        . ,(make-instance 'part :name "Piano"
                                           :instrument (assoc-value *instruments* :piano)
                                           :staves (list (assoc-value *staves* :piano-bass)
                                                         (assoc-value *staves* :piano-treble))))))

(defparameter *views*
  `((:default . ,(make-instance 'parts-view :name "default"
                                            :parts (mapcar #'cdr *parts*)))
    (:violin  . ,(make-instance 'parts-view :name "violin"
                                            :parts (list (cdr (first *parts*)))))
    (:piano   . ,(make-instance 'parts-view :name "piano"
                                            :parts (list (cdr (first *parts*)))))))

(defparameter *songs*
  (list (make-instance 'song
                       :publishing-date (local-time:now)
                       :composer "Jerry Dawn"
                       :lyrics-author "John Donnovan"
                       :title "Nothing lasts forever"
                       :instruments (mapcar #'cdr *instruments*)
                       :parts       (mapcar #'cdr *parts*)
                       :views       (mapcar #'cdr *views*))
        (make-instance 'song
                       :composer "Juliet Dufus"
                       :title "Carpe diem"
                       :instruments (mapcar #'cdr *instruments*)
                       :parts       (mapcar #'cdr *parts*)
                       :views       (mapcar #'cdr *views*))
        (make-instance 'song
                       :publishing-date (local-time:now)
                       :composer "Juliet Dufus"
                       :title "Carpe diem"
                       :instruments (mapcar #'cdr *instruments*)
                       :parts       (mapcar #'cdr *parts*)
                       :views       (mapcar #'cdr *views*)))) 

(run :songs *songs* :new-process t)
