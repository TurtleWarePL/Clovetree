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


(defconstant +song-information-view+ (make-instance 'song-information-view))
(defconstant +song-selection-view+   (make-instance 'song-selection-view))
(defconstant +song-parts-view-view+  (make-instance 'song-parts-view-view))


;;; Presentation types with CLOS classes
(clim:define-presentation-type instrument ())
(clim:define-presentation-type part ())
(clim:define-presentation-type parts-view ())

;;; Presentation types without CLOS classes
(clim:define-presentation-type song-title    ())
(clim:define-presentation-type song-composer ())
(clim:define-presentation-type song-lyrics   ())
(clim:define-presentation-type song-date     ())

#+ (or) ;; That would be cool if it did work.
(clim:define-presentation-type parts-view-oid ()
  :inherit-from `(or part parts-view))

;;; Presentation type abbreviations
(clim:define-presentation-type-abbreviation parts-view-oid ()
  `(or part parts-view))


;;; Presentation methods
(clim:define-presentation-method clim:present
    ((object song) (type song) stream (view song-information-view) &key)
  (flet ((show-field (name value ptype)
           (clim:with-output-as-presentation (stream object ptype :single-box t)
             (clim:with-text-face (stream :bold)
               (format stream "~a: " name))
             (clim:with-text-face (stream :italic)
               (princ (or value "(empty)") stream)))
           (terpri stream))
         (list-group (name objects presentation-type)
           (clim:with-text-face (stream :bold)
             (format stream "~a~%" name))
           (clim:format-textual-list objects
                                     (lambda (object stream)
                                       (clim:present object
                                                     presentation-type
                                                     :stream stream
                                                     :view view
                                                     :single-box t))
                                     :stream stream)
           (when objects
             (format stream ".~%"))
           (terpri stream)))
    (show-field "Title" (title object) 'song-title)
    (show-field "Composer" (composer object) 'song-composer)
    (show-field "Lyrics" (lyrics-author object) 'song-lyrics)
    (show-field "Published" (publishing-date object) 'song-date)
    (terpri stream)
    (list-group "Instruments" (instruments object) 'instrument)
    (list-group "Parts" (parts object) 'part)
    (list-group "Views" (views object) 'parts-view)))

(clim:define-presentation-method clim:present
    ((object song) (type song) stream (view song-selection-view) &key)
  (format stream "Title: ~a~%" (title object))
  (format stream "Composer: ~a~%" (composer object)))

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


;;; I'd rather have one method on the type PARTS-VIEW-OID defined as a
;;; class inheriting from the OR presentation type type instead, but
;;; the PRESENTATION-SUBTYPEP is wonky - it only special-cases
;;; presentation type specifiers which start with symbols OR and AND
;;; instead of checking the specifier metaclass. There is an elaborate
;;; explanation in the source code why it is like this. Grokking the
;;; presentation type system is a fight for another day.

(clim:define-presentation-method clim:present
    ((object parts-view) (type parts-view) stream (view song-parts-view-view) &key)
  (loop for part in (parts object)
        do (clim:present part 'part :stream stream
                                    :view +song-parts-view-view+
                                    :sensitive nil)))

(clim:define-presentation-method clim:present
    ((object part) (type part) stream (view song-parts-view-view) &key)
  (format stream "~a~%" (name object))
  (let ((height 75)
        (width (clime:stream-line-width stream)))
    (if-let ((staves (staves object)))
      (loop for staff in staves do
        (clim:with-room-for-graphics
            (stream :first-quadrant t :height height :move-cursor t)
          (clim:draw-rectangle* stream 0 0 width height :filled nil))
        (fresh-line stream)
        (clim:stream-increment-cursor-position stream 0 20))
      (princ "(no staves)" stream))
    (fresh-line stream)
    (clim:stream-increment-cursor-position stream 0 20)))
