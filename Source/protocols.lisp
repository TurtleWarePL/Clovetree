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
(defclass song-selection-view   (clim:gadget-view) ())
(defclass song-parts-view       (clim:gadget-view) ())
(defclass song-parts-view-view  (clim:gadget-view) ())


(defconstant +song-information-view+ (make-instance 'song-information-view))
(defconstant +song-selection-view+   (make-instance 'song-selection-view))
(defconstant +song-parts-view+       (make-instance 'song-parts-view))
(defconstant +song-parts-view-view+  (make-instance 'song-parts-view-view))


;;; Presentation methods
(clim:define-presentation-method clim:present
    ((object song) (type song) stream (view song-information-view) &key)
  (flet ((show-field (name value)
           (when value
             (clim:with-text-face (stream :bold)
              (format stream "~a: " name))
             (clim:with-text-face (stream :italic)
               (princ value stream))
             (terpri stream)))
         (list-group (name objects presentation-type)
           (clim:with-text-face (stream :bold)
             (format stream "~a~%" name))
           (clim:format-textual-list objects
                                     (lambda (object stream)
                                       (clim:present object
                                                     presentation-type
                                                     :stream stream
                                                     :view view))
                                     :stream stream)
           (princ "." stream)
           (terpri stream)
           (terpri stream)))
    (show-field "Title" (title object))
    (show-field "Composer" (composer object))
    (show-field "Lyrics" (lyrics-author object))
    (when-let ((date (publishing-date object)))
      (show-field "Published" (local-time:format-timestring
                               nil date
                               :format local-time:+iso-8601-date-format+)))
    (terpri stream)
    (list-group "Instruments" (instruments object) 'instrument)
    (list-group "Parts" (parts object) 'part)
    (list-group "Views" (views object) 'parts-view)))

(clim:define-presentation-method clim:present
    ((object song) (type song) stream (view song-selection-view) &key)
  (format stream "Title: ~a~%" (title object))
  (format stream "Composer: ~a~%" (composer object)))

(clim:define-presentation-method clim:present
    ((object song) (type song) stream (view song-parts-view) &key)
  (clim:format-items (parts object) :stream stream :presentation-type 'part))

(clim:define-presentation-method clim:present
    ((object instrument) (type instrument) stream (view song-information-view) &key)
  (format stream "~a ~a" (name object) (key object)))

(clim:define-presentation-method clim:present
    ((object part) (type part) stream (view song-information-view) &key)
  (format stream "~a (" (name object))
  (clim:present (instrument object) 'instrument :stream stream :view view)
  (format stream ")"))

(clim:define-presentation-method clim:present
    ((object parts-view) (type parts-view) stream (view song-information-view) &key)
  (format stream "~a (~d)" (name object) (length (parts object))))
