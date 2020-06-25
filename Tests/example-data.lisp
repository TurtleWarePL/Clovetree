(in-package #:clovetree-implementation)

(defvar *staff-1* (make-instance 'staff :name "Bass"   :staff-type :five-line))
(defvar *staff-2* (make-instance 'staff :name "Treble" :staff-type :five-line))
(defvar *staff-3* (make-instance 'staff :name "Treble" :staff-type :five-line))

(defvar *instrument-1* (make-instance 'instrument :name "Piano"))
(defvar *instrument-2* (make-instance 'instrument :name "Violin"))

(defvar *part-1* (make-instance 'part :name "First violin"
                                      :instrument *instrument-2*))
(defvar *part-2* (make-instance 'part :name "Piano"
                                      :instrument *instrument-1*
                                      :staves (list *staff-1* *staff-2*)))

(defvar *view-1* (make-instance 'parts-view :name "default"
                                            :parts (list *part-1* *part-2*)))
(defvar *view-2* (make-instance 'parts-view :name "violin"
                                            :parts (list *part-1*)))
(defvar *view-3* (make-instance 'parts-view :name "piano"
                                            :parts (list *part-2*)))

(defparameter *songs*
  (let ((all-instruments (list *instrument-1* *instrument-2*))
        (all-parts (list *part-1* *part-2*))
        (all-views (list *view-1* *view-2* *view-3*)))
    (list (make-instance 'song
                         :publishing-date (local-time:now)
                         :composer "Jerry Dawn"
                         :lyrics-author "John Donnovan"
                         :title "Nothing lasts forever"
                         :instruments all-instruments
                         :parts       all-parts
                         :views       all-views)
          (make-instance 'song
                         :composer "Juliet Dufus"
                         :title "Carpe diem"
                         :instruments all-instruments
                         :parts       all-parts
                         :views       all-views)
          (make-instance 'song
                         :publishing-date (local-time:now)
                         :composer "Juliet Dufus"
                         :title "Carpe diem"
                         :instruments all-instruments
                         :parts       all-parts
                         :views       all-views)))) 

(run :songs *songs* :new-process t)
