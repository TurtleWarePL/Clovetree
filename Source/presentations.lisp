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

(defclass new-instrument-view (song-information-view) ())
(defclass new-part-view       (song-information-view) ())
(defclass new-parts-view-view (song-information-view) ())
(defclass new-staff-view      (song-information-view) ())

(defclass mod-parts-view-view (song-information-view)
  ((parts-view :type parts-view :accessor parts-view :initarg :parts-view)))

(defclass mod-staff-view (song-information-view)
  ((part :type part :accessor part :initarg :part)))

(defvar +song-information-view+ (make-instance 'song-information-view))
(defvar +song-selection-view+   (make-instance 'song-selection-view))
(defvar +song-parts-view-view+  (make-instance 'song-parts-view-view))

(defvar +new-instrument-view+ (make-instance 'new-instrument-view))
(defvar +new-part-view+       (make-instance 'new-part-view))
(defvar +new-parts-view-view+ (make-instance 'new-parts-view-view))
(defvar +new-staff-view+      (make-instance 'new-staff-view))


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

(clim:define-presentation-type-abbreviation note () *notes*)
(clim:define-presentation-type-abbreviation staff-type () *staff-types*)


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
    (object (type instrument) stream (view song-information-view) &key)
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
  (princ (name object) stream)
  (terpri stream)
  (let ((staves (staves object)))
    (cond ((length= 0 staves)
           (princ "(no staves)" stream))
          ((length= 1 staves)
           (clim:present (first staves) 'staff
                         :stream stream
                         :view +song-parts-view-view+))
          (t
           (clim:present staves 'grand-staff
                         :stream stream
                         :view +song-parts-view-view+))
          ;; It is not specified to add an upper limit to a number of staves.
          #+ (or)
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
          #+ (or)
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
      (clim:draw-rectangle* stream 0 0 width height :filled nil)
      (clim:draw-text* stream (name object)
                       20 (/ height 2) :align-y :center))))

(clim:define-presentation-method clim:present
    (object (type grand-staff) stream (view song-parts-view-view) &key)
  (let* ((height 75))
    (clim:with-room-for-graphics
        (stream :first-quadrant nil :move-cursor nil)
      (clim:draw-line* stream 10 10 10 (- (* (length object) height) 10)))
    (loop for staff in object
          do (clim:present staff 'staff :stream stream :view view))))

(clim:define-presentation-method clim:present
    (object (type staff) stream (view mod-staff-view) &key)
  (format stream "~A" (name object)))


;;; Presentation methods for ACCEPT

(defclass song-info-pane (clim:application-pane) ()
  (:default-initargs :text-margins '(:left (:absolute 15)
                                     :top  (:absolute 10))))

(defclass song-main-pane (clim:application-pane) ()
  (:default-initargs :text-margins '(:left (:absolute 15)
                                     :top  (:absolute 10)
                                     :right (:absolute 770))))

;;; FIXME normally this hack wouldn't be needed, but ACCEPT-1 goes through
;;; Drei's input editing which assumes that during accept the window is never
;;; cleared. We avoid that for our "specialized" methods.

;;; FIXME this really should be using ACCEPTING-VALUES but that macro is
;;; currently garbage in McCLIM.

(defmethod clim:stream-accept ((stream song-info-pane) type
                               &key view &allow-other-keys)
  (if (member type '(part song parts-view staff grand-staff))
      (clim:funcall-presentation-generic-function clim:accept type stream view)
      (call-next-method)))

(clim:define-presentation-method clim:accept
    ((type song)
     (output song-info-pane)
     (view song-information-view)
     &key &allow-other-keys)
  (let ((frame clim:*application-frame*))
    (clim:window-clear output)
    (clim:with-drawing-options
        (output :text-size :larger :text-face :bold)
      (princ "Pick a song" output)
      (terpri output)
      (clim:stream-increment-cursor-position
       output 0 (clim:stream-line-height output)))
    (clim:format-textual-list
     (songs frame)
     (lambda (object stream)
       (clim:present object 'song :stream stream
                                  :view +song-selection-view+
                                  :single-box t))
     :stream output
     :separator #\newline)
    (terpri output)
    (clim:with-input-context ('song :override t)
        (object)
        (handler-case (loop (clim:stream-read-gesture output))
          (clim:abort-gesture ()
            (return-from clim:accept nil)))
      (song
       (return-from clim:accept object)))))

(clim:define-presentation-method clim:accept
    ((type instrument)
     (stream song-info-pane)
     (view new-instrument-view)
     &key default default-type)
  (declare (ignore default default-type))
  (let (name key)
    (clim:accepting-values ()
      (setf name
            (clim:accept 'string :prompt "Instrument name"))
      (terpri)
      (setf key
            (clim:accept 'note
                         :prompt "Key"
                         :view clim:+option-pane-view+
                         :default :c)))
    (unless name
      (setf name (format nil "~a" (gensym "i"))))
    (make-instance 'instrument :name name :key key)))

(clim:define-presentation-method clim:accept
    ((type part)
     (output song-info-pane)
     (view new-part-view)
     &key default default-type)
  (declare (ignore default default-type))
  (let* ((frame clim:*application-frame*)
         (name nil))
    (setf name (or (clim:accept 'string :prompt "Part name")
                   (format nil "~a" (gensym "p"))))
    (clim:window-clear output)
    (clim:with-drawing-options
        (output :text-size :larger :text-face :bold)
      (princ "Pick the instrument" output)
      (terpri output)
      (clim:stream-increment-cursor-position
       output 0 (clim:stream-line-height output)))
    (clim:format-textual-list
     (instruments (current-song frame))
     (lambda (object stream)
       (clim:present object 'instrument :stream stream
                                        :view +song-information-view+
                                        :single-box t))
     :stream output
     :separator #\newline)
    (terpri output)
    (clim:with-input-context ('instrument :override t)
        (object)
        (handler-case (loop (clim:stream-read-gesture output))
          (clim:abort-gesture ()))
      (instrument
       (make-instance 'part :instrument object :name name)))))

(defun select-multiple-parts (frame output parts)
  (loop
    (clim:window-clear output)
    (clim:with-drawing-options
        (output :text-size :larger :text-face :bold)
      (princ "Select parts" output)
      (terpri output)
      (clim:stream-increment-cursor-position
       output 0 (clim:stream-line-height output)))
    (clim:format-textual-list
     (parts (current-song frame))
     (lambda (object stream)
       (clim:with-drawing-options (stream :text-size :large
                                          :ink (if (find object parts)
                                                   clim:+dark-green+
                                                   clim:+black+))
         (clim:present object 'part :stream stream
                                    :view +song-information-view+
                                    :single-box t)))
     :stream output
     :separator #\newline)
    (terpri output)
    (terpri output)
    (clim:with-output-as-gadget (output)
      (clim:make-pane :push-button
                      :label "Done"
                      :activate-callback
                      (lambda (gadget)
                        (declare (ignore gadget))
                        (return-from select-multiple-parts
                          (values parts t)))))
    (clim:with-input-context ('part :override t)
        (object)
        (handler-case (loop (clim:stream-read-gesture output))
          (clim:abort-gesture ()
            (return-from select-multiple-parts
             (values nil nil))))
      (part
       (if (find object parts)
           (setf parts (delete object parts))
           (push object parts))))))

(clim:define-presentation-method clim:accept
    ((type parts-view)
     (output song-info-pane)
     (view new-parts-view-view)
     &key default default-type)
  (declare (ignore default default-type))
  (let ((frame clim:*application-frame*)
        (parts nil)
        (name nil))
    (setf name (or (clim:accept 'string :prompt "View name")
                   (format nil "~a" (gensym "v"))))
    (multiple-value-bind (parts ok)
        (select-multiple-parts frame output parts)
      (and ok
           (make-instance 'parts-view :parts parts :name name)))))

(clim:define-presentation-method clim:accept
    ((type parts-view)
     (output song-info-pane)
     (view mod-parts-view-view)
     &key default default-type)
  (declare (ignore default default-type))
  (let ((frame clim:*application-frame*)
        (parts (parts (parts-view view))))
    (multiple-value-bind (parts ok)
        (select-multiple-parts frame output parts)
      (when ok
        (setf (parts (parts-view view)) parts)))))

(clim:define-presentation-method clim:accept
    ((type staff)
     (stream song-info-pane)
     (view new-staff-view)
     &key default default-type)
  (declare (ignore view default default-type))
  (let (name stype)
    (clim:accepting-values ()
      (setf name
            (clim:accept 'string :prompt "Staff name"))
      (terpri)
      (setf stype
            (clim:accept 'staff-type
                         :prompt "Type"
                         :view clim:+option-pane-view+
                         :default (second *staff-types*))))
    (unless name
      (setf name (format nil "~a" (gensym "s"))))
    (make-instance 'staff :name name :staff-type stype)))

;;; kludge?
(define-condition staff-condition ()
  ((staff :initarg :staff :reader staff)
   (action :initarg :action :reader action)))

(clim:define-presentation-method clim:accept
    ((type staff)
     (output song-info-pane)
     (view mod-staff-view)
     &key default default-type)
  (declare (ignore default default-type))
  (let* ((part (part view))
         (old-staves (copy-list (staves part))))
    (loop
      (handler-case
          (progn (clim:window-clear output)
                 (clim:with-drawing-options
                     (output :text-size :larger :text-face :bold)
                   (princ "Part's staves" output)
                   (terpri output)
                   (clim:stream-increment-cursor-position
                    output 0 (clim:stream-line-height output)))
                 (clim:format-textual-list
                  (staves part)
                  (lambda (object stream)
                    (clim:present object 'staff :stream stream
                                                :view view
                                                :single-box t))
                  :stream output
                  :separator #\newline)
                 (terpri output)
                 (clim:with-input-context ('staff :override t)
                     (object)
                     (handler-case (loop (clim:stream-read-gesture output))
                       (clim:abort-gesture ()
                         (setf (staves part) old-staves)
                         (return-from clim:accept nil)))
                   (staff
                    (return-from clim:accept object))))
        (staff-condition (c)
          (let ((object (staff c))
                (staves (staves part)))
            (ecase (action c)
              (:up
               (let ((pos (position object staves)))
                 (rotatef (nth pos staves) (nth (1- pos) staves))))
              (:down
               (let ((pos (position object staves)))
                 (rotatef (nth pos staves) (nth (1+ pos) staves))))
              (:delete
               (setf (staves part)
                     (delete object staves))))))))))
