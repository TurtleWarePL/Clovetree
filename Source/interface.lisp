(in-package #:clovetree-implementation)

(clim:define-application-frame clovetree ()
  ((songs :initarg :songs :accessor songs)
   (current-song :accessor current-song)
   (parts-object :accessor parts-object))
  (:geometry :width 1200 :height 800)
  (:menu-bar menubar-command-table)
  (:pointer-documentation t)
  (:panes (tab song-info-pane :display-function #'display)
          (app song-main-pane :display-function #'display)
          (int :interactor))
  (:layouts (simple
             (clim:horizontally (:height 800)
               (400 tab)
               (800 app)))
            (advanced
             (clim:horizontally ()
               (400 tab)
               (800 (clim:vertically ()
                      (600 app)
                      (200 int)))))))

(defmethod initialize-instance :after ((frame clovetree) &key songs)
  (if songs
      (mapc (lambda (song) (check-type song song)) songs)
      (setf songs (list (make-instance 'song))
            (songs frame) songs))
  (setf (slot-value frame 'current-song) nil)
  (setf (current-song frame) (first songs)))

(defmethod (setf current-song) :around (new-val (frame clovetree))
  (let ((song (current-song frame)))
    (unless (eq song new-val)
      (call-next-method)
      (setf (parts-object frame) (or (first (views new-val))
                                     (parts new-val))))))


;;; Commands

(define-clovetree-command (com-switch-layout :keystroke (#\n :control))
    ()
  (let* ((frame clim:*application-frame*)
         (all (clim:frame-all-layouts frame))
         (cur (clim:frame-current-layout frame)))
    (loop for (layout . rest) on all
          when (eq layout cur)
            do (if (null rest)
                   (setf (clim:frame-current-layout frame) (first all))
                   (setf (clim:frame-current-layout frame) (first rest))))))

(define-clovetree-command (com-new-song :name t)
    ()
  (let ((frame clim:*application-frame*)
        (song (make-instance 'song :title nil
                                   :composer nil
                                   :lyrics-author nil
                                   :publishing-date nil)))
    (push song (songs frame))
    (setf (current-song frame) song)))

;;; Normally we'd have path as an argument for the command, but the
;;; prompt is erased when in the parts-view-view window. McCLIM bug.
(define-clovetree-command (com-load-song :name t)
    ()
  (let ((path (clim:accept 'pathname :prompt "Song pathname")))
    (format *standard-input* "Implement me ~s!" path)))

(define-clovetree-command (com-close-song :name t)
    ()
  (let* ((frame clim:*application-frame*)
         (new-songs (remove (current-song frame) (songs frame))))
    (setf (songs frame) new-songs)
    (if new-songs
        (setf (current-song frame) (first new-songs))
        (com-new-song))))

;;; Normally we'd have arguments as arguments for the command, but the
;;; prompt is erased when in the parts-view-view window. McCLIM bug.
(define-clovetree-command (com-save-song :name t)
    ()
  (let ((path (clim:accept 'pathname :prompt "Pathname")))
    (format *standard-input* "Implement me! ~s~%" path)))

(define-clovetree-command (com-quit :name t)
    ()
  (clim:frame-exit clim:*application-frame*))

(define-clovetree-command (com-pick-song :name t)
    ()
  (when-let ((song (clim:accept 'song :view +song-information-view+ :prompt nil)))
    (setf (current-song clim:*application-frame*) song)))

(define-clovetree-command (com-show-parts :name t)
    ((object parts-view-oid :gesture :select))
  (setf (parts-object clim:*application-frame*) object))

(macrolet ((change-song-command (what)
             (let ((name (alexandria:symbolicate 'com-change-song- what)))
               `(define-clovetree-command (,name :name t)
                    ((object song) (new-value string))
                  (setf (,what object) new-value)))))
  (change-song-command title)
  (change-song-command composer)
  (change-song-command lyrics-author)
  (change-song-command publishing-date))

(define-clovetree-command (com-add-instrument :name t) ()
  (when-let ((instrument (clim:accept 'instrument
                                      :view +new-instrument-view+
                                      :prompt nil)))
    (push instrument (instruments (current-song clim:*application-frame*)))))

(define-clovetree-command (com-add-part :name t) ()
  (when-let ((part (clim:accept 'part :view +new-part-view+ :prompt nil)))
    (push part (parts (current-song clim:*application-frame*)))))

(define-clovetree-command (com-add-view :name t) ()
  (when-let ((view (clim:accept 'parts-view
                                :view +new-parts-view-view+
                                :prompt nil)))
    (push view (views (current-song clim:*application-frame*)))))


;;; Menu
(clim:make-command-table 'file-command-table
                         :errorp nil
                         :menu '(("New song" :command com-new-song)
                                 ("Load song" :command com-load-song)
                                 ("Save song" :command com-save-song)
                                 ("Close song" :command com-close-song)
                                 (nil :divider t)
                                 ("Quit" :command com-quit)))

(clim:make-command-table 'menubar-command-table
                         :errorp nil
                         :menu '(("File" :menu file-command-table)
                                 ("Song" :command com-pick-song)
                                 #+ (or) ("Help" :command nil)))


;;; Translators

(macrolet ((change-song-translator (ptype what doc prompt)
             (let ((cmd (alexandria:symbolicate 'com-change-song- what))
                   (name (alexandria:symbolicate 'tr-change-song- what)))
               `(clim:define-presentation-to-command-translator ,name
                    (,ptype ,cmd clovetree :documentation ,doc :gesture nil)
                    (object)
                  (list object
                        (clim:accepting-values ()
                          (clim:accept 'string :prompt ,prompt
                                               :delimiter-gestures '(:enter))))))))
  (change-song-translator song-title title
                          "Change the song title" "Title")
  (change-song-translator song-composer composer
                          "Change the song composer" "Composer")
  (change-song-translator song-lyrics lyrics-author
                          "Change the song lyrics author" "Lyrics author")
  (change-song-translator song-date publishing-date
                          "Change the song publishing data" "Publishing date"))

(clim:define-presentation-to-command-translator tr-remove-song-lyrics-author
    (song-lyrics com-change-song-lyrics-author clovetree
                 :documentation "Remove the song lyrics author" :gesture nil)
    (object)
  (list object nil))

(clim:define-presentation-to-command-translator tr-remove-song-publishing-date
    (song-date com-change-song-publishing-date clovetree
               :documentation "Remove the song publishing data" :gesture nil)
    (object)
  (list object nil))

(clim:define-presentation-to-command-translator tr-add-instrument
    (instruments-group com-add-instrument clovetree
                       :documentation "Add an instrument"
                       :gesture nil)
    (object)
  ())

(clim:define-presentation-to-command-translator tr-add-part
    (parts-group com-add-part clovetree
                 :documentation "Add a part"
                 :gesture nil)
    (object)
  ())

(clim:define-presentation-to-command-translator tr-add-view
    (views-group com-add-view clovetree
                 :documentation "Add a view"
                 :gesture nil)
    (object)
  ())


;;; Display methods

(defmethod display ((frame clovetree) (stream song-info-pane))
  (clim:present (current-song frame) 'song
                :stream stream
                :view +song-information-view+
                :single-box t))

(defmethod display ((frame clovetree) (stream song-main-pane))
  (if-let ((object (parts-object frame)))
    (clim:present object
                  (clim:presentation-type-of object)
                  :stream stream
                  :view +song-parts-view-view+
                  :sensitive nil)
    (format stream "La la la.~%")))


(defun run (&rest args &key songs new-process)
  (declare (ignore songs))
  (remf args :new-process)
  (let ((frame (apply #'clim:make-application-frame 'clovetree args)))
    (if new-process
        (clim-sys:make-process (lambda () (clim:run-frame-top-level frame)))
        (clim:run-frame-top-level frame))))
