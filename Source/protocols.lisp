(in-package #:clovetree-implementation)

;;; Function is meant to be specialized on both parameters. It is
;;; specified as a `:display-function' for all panes in clovetree.
(defgeneric display (frame pane)
  (:method (frame pane)
    (declare (ignore frame))
    (format pane "Hello World!")
    (terpri pane)))


;;; Views
(defclass song-information-view (clim:gadget-view) ())
(defclass song-parts-view       (clim:gadget-view) ())
(defclass song-parts-view-view  (clim:gadget-view) ())

(defconstant +song-information-view+ (make-instance 'song-information-view))
(defconstant +song-parts-view+       (make-instance 'song-parts-view))
(defconstant +song-parts-view-view+  (make-instance 'song-parts-view-view))


;;; Presentation methods
(clim:define-presentation-method clim:present
    ((object song) (type song) stream (view song-information-view) &key)
  (format stream "Title: ~a~%" (title object))
  (format stream "Composer: ~a~%" (composer object))
  (when-let ((author (lyrics-author object)))
    (format stream "Lyrics: ~a~%" author))
  (when-let ((published (publishing-date object)))
    (let* ((format local-time:+iso-8601-date-format+)
           (date (local-time:format-timestring nil published :format format)))
      (format stream "Published: ~a~%" date))))

(clim:define-presentation-method clim:present
    ((object song) (type song) stream (view song-parts-view) &key)
  (clim:format-items (parts object) :stream stream :presentation-type 'part))
