(in-package #:clovetree-implementation)

(defparameter *staff-1* (make-instance 'staff :name "Bass"   :staff-type :five-line))
(defparameter *staff-2* (make-instance 'staff :name "Treble" :staff-type :five-line))
(defparameter *staff-3* (make-instance 'staff :name "Treble" :staff-type :five-line))

(defparameter *instrument-1* (make-instance 'instrument :name "Piano"))
(defparameter *instrument-2* (make-instance 'instrument :name "Violin"))
(defparameter *instrument-3* (make-instance 'instrument :name "Tongue"))

(defparameter *part-1* (make-instance 'part :name "First violin"
                                            :instrument *instrument-2*
                                            :staves (list *staff-3*)))
(defparameter *part-2* (make-instance 'part :name "Piano"
                                      :instrument *instrument-1*
                                            :staves (list *staff-1* *staff-2*)))
(defparameter *part-3* (make-instance 'part :name "Click"
                                      :instrument *instrument-3*))

(defparameter *view-1* (make-instance 'parts-view :name "default"
                                                  :parts (list *part-1* *part-2*)))
(defparameter *view-2* (make-instance 'parts-view :name "violin"
                                            :parts (list *part-1*)))
(defparameter *view-3* (make-instance 'parts-view :name "piano"
                                                  :parts (list *part-2*)))
(defparameter *view-4* (make-instance 'parts-view :name "everything"
                                                  :parts (list *part-1*
                                                               *part-2*
                                                               *part-3*)))

(defparameter *songs*
  (let ((all-instruments (list *instrument-1* *instrument-2* *instrument-3*))
        (all-parts (list *part-1* *part-2* *part-3*))
        (all-views (list *view-1* *view-2* *view-3* *view-4*))
        (date (local-time:format-timestring
               nil (local-time:now)
               :format local-time:+iso-8601-date-format+)))
    (list (make-instance 'song
                         :publishing-date date
                         :composer "Jerry Dawn"
                         :lyrics-author "John Donnovan"
                         :title "Nothing lasts forever"
                         :instruments (butlast all-instruments)
                         :parts       (butlast all-parts)
                         :views       (butlast all-views))
          (make-instance 'song
                         :composer "Juliet Dufus"
                         :title "Carpe diem"
                         :instruments (butlast all-instruments)
                         :parts       (butlast all-parts)
                         :views       (butlast all-views))
          (make-instance 'song
                         :publishing-date date
                         :composer "Merry Moore"
                         :title "Panta rei"
                         :instruments all-instruments
                         :parts       all-parts
                         :views       all-views)))) 

(run :songs *songs* :new-process t)
