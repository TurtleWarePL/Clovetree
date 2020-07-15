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
(defclass song-parts-view-view  (clim:gadget-view) ())

(defvar +song-information-view+ (make-instance 'song-information-view))
(defvar +song-selection-view+   (make-instance 'song-selection-view))
(defvar +song-parts-view-view+  (make-instance 'song-parts-view-view))


;;; Presentation types with CLOS classes
(clim:define-presentation-type part ())
(clim:define-presentation-type parts-view ())
(clim:define-presentation-type instrument ())
(clim:define-presentation-type staff ())
(clim:define-presentation-type grand-staff ()
  :inherit-from '(sequence staff)
  :description "Combination of two staves (for a one performer)")

;;; Presentation types without CLOS classes
(clim:define-presentation-type song-title    ())
(clim:define-presentation-type song-composer ())
(clim:define-presentation-type song-lyrics   ())
(clim:define-presentation-type song-date     ())

(clim:define-presentation-type instruments-group ())
(clim:define-presentation-type parts-group       ())
(clim:define-presentation-type views-group       ())

;;; Presentation type abbreviations
(clim:define-presentation-type-abbreviation parts-view-oid ()
  `(or part parts-view))


;;; Presentation methods for the presentation generic function PRESENT
(clim:define-presentation-method clim:present
    (object (type song) stream (view song-information-view) &key)
  (flet ((show-field (name value ptype)
           (clim:with-output-as-presentation (stream object ptype :single-box t)
             (clim:with-text-face (stream :bold)
               (format stream "~a: " name))
             (clim:with-text-face (stream :italic)
               (princ (or value "(empty)") stream)))
           (terpri stream))
         (list-group (name objects object-ptype group-ptype)
           (clim:with-output-as-presentation (stream object group-ptype)
             (clim:with-text-face (stream :bold)
               (format stream "~a~%" name)))
           (clim:format-textual-list objects
                                     (lambda (object stream)
                                       (clim:present object object-ptype
                                                     :stream stream
                                                     :view view
                                                     :single-box t))
                                     :stream stream)
           (when objects
             (format stream ".~%"))
           (terpri stream)))
    (show-field "Title"     (title object)           'song-title)
    (show-field "Composer"  (composer object)        'song-composer)
    (show-field "Lyrics"    (lyrics-author object)   'song-lyrics)
    (show-field "Published" (publishing-date object) 'song-date)
    (terpri stream)
    (list-group "Instruments" (instruments object) 'instrument 'instruments-group)
    (list-group "Parts"       (parts object)       'part       'parts-group)
    (list-group "Views"       (views object)       'parts-view 'views-group)
    (terpri stream)))

(clim:define-presentation-method clim:present
    (object (type song) stream (view song-selection-view) &key)
  (format stream "Title: ~a~%" (title object))
  (format stream "Composer: ~a~%" (composer object)))

(clim:define-presentation-method clim:present
    ((object instrument) (type instrument) stream (view song-information-view) &key)
  (format stream "~a ~a" (name object) (key object)))

(clim:define-presentation-method clim:present
    (object (type part) stream (view song-information-view) &key)
  (format stream "~a (" (name object))
  (clim:present (instrument object) 'instrument :stream stream :view view)
  (format stream ")"))

(clim:define-presentation-method clim:present
    (object (type parts-view) stream (view song-information-view) &key)
  (format stream "~a (~d)" (name object) (length (parts object))))

(clim:define-presentation-method clim:present
    (object (type parts-view) stream (view song-parts-view-view) &key)
  ;; Normally we'd use CLIM:FORMATTING-TABLE here, but it needs a lot
  ;; of work to have it behave nicely (i.e to be able to center the
  ;; text in the cell).
  (loop for part in (parts object)
        do (clim:present part 'part :stream stream
                                    :view +song-parts-view-view+
                                    :sensitive nil)))

(clim:define-presentation-method clim:present
    (object (type part) stream (view song-parts-view-view) &key)
  (format stream "~a~%" (name object))
  (let ((staves (staves object)))
    (cond ((length= 0 staves)
           (princ "(no staves)" stream))
          ((length= 1 staves)
           (clim:present (first staves) 'staff
                         :stream stream
                         :view +song-parts-view-view+))
          ((length= 2 staves)
           (clim:present staves 'grand-staff
                         :stream stream
                         :view +song-parts-view-view+))
          #+ (or)
          ((length= 3 staves)
           ;; Grand staff with 1 extra (i.e organ + pedals). That said
           ;; our abstraction puts pedals as a separate instrument and
           ;; this should never happen.
           (clim:present staves 'grand-staff*
                         :stream stream
                         :view +song-parts-view-view+))
          (t
           (princ "(error: too many staves)"))))
  (fresh-line stream)
  (clim:stream-increment-cursor-position stream 0 20))

(clim:define-presentation-method clim:present
    (object (type staff) stream (view song-parts-view-view) &key)
  (let* ((height 75)
         (width (clime:stream-line-width stream)))
    (clim:with-room-for-graphics
        (stream :first-quadrant nil :height height :move-cursor t)
      (clim:draw-rectangle* stream 0 0 width height :filled nil))))

(clim:define-presentation-method clim:present
    (object (type grand-staff) stream (view song-parts-view-view) &key)
  (let* ((height 75))
    (clim:with-room-for-graphics
        (stream :first-quadrant nil :move-cursor nil)
      (clim:draw-line* stream 10 10 10 (- (* (length object) height) 10)))
    (loop for staff in object
          do (clim:present staff 'staff :stream stream :view view))))

