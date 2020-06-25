(in-package #:clovetree-implementation)

(defclass song-info-pane (clim:application-pane) ()
  (:default-initargs :width 400 :height 600))

(defclass song-main-pane (clim:application-pane) ()
  (:default-initargs :width 800))

(clim:define-application-frame clovetree ()
  ((songs :initarg :songs :accessor songs)
   (current-song :accessor current-song)
   (parts-object :accessor parts-object))
  (:geometry :width 1080 :height 600)
  (:menu-bar menubar-command-table)
  (:panes (app song-main-pane :display-function #'display)
          (int :interactor)
          (tab song-info-pane :display-function #'display
                              :text-margins '(:left (:absolute 15)
                                              :top  (:absolute 10))))
  (:layouts (advanced
             (clim:horizontally ()
               tab
               (clim:make-pane 'clime:box-adjuster-gadget)
               (clim:vertically ()
                 (3/5 app)
                 (clim:make-pane 'clime:box-adjuster-gadget)
                 (2/5 int))))
            (simple
             (clim:horizontally ()
               (1/3 tab)
               (clim:make-pane 'clime:box-adjuster-gadget)
               (2/3 app)))))

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
  (let (title composer lyrics)
    ;; Doesn't work due to McCLIM bug       vvvvvvvvvvvvvvvv
    (clim:accepting-values (t :own-window '(:left 15 :top 10)
                              :align-prompts t)
      (setf title    (clim:accept 'string :prompt "Song title"
                                          :default "(Untitled)"
                                          :display-default nil))
      (setf composer (clim:accept 'string :prompt "Composer"
                                          :default "(Unknown)"
                                          :insert-default t))
      (setf lyrics   (clim:accept 'string :prompt "Lyrics author"))
      #+ (or)
      (clim:accept 'local-time:timestamp :prompt "Publishing date"))
    (let ((frame clim:*application-frame*)
          (song (make-instance 'song :title title
                                     :composer composer
                                     :lyrics-author lyrics)))
      (push song (songs frame))
      (setf (current-song frame) song))))

(define-clovetree-command (com-load-song :name t)
    ((path pathname))
  (declare (ignore path))
  (format *standard-input* "Implement me!"))

(define-clovetree-command (com-save-song :name t)
    ((song song) (path pathname))
  (declare (ignore song path))
  (format *standard-input* "Implement me!"))

(define-clovetree-command (com-quit :name t)
    ()
  (clim:frame-exit clim:*application-frame*))

(define-clovetree-command (com-pick-song :name t)
    ()
  (let* ((frame clim:*application-frame*)
         (output (clim:find-pane-named frame 'tab)))
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
    (setf (current-song frame) (clim:accept 'song))))

(define-clovetree-command (com-show-parts :name t)
    ((object t))
  (setf (parts-object clim:*application-frame*) object))


;;; Menu
(clim:make-command-table 'file-command-table
                         :errorp nil
                         :menu '(("New song"  :command com-new-song)
                                 ("Load song" :command com-load-song)
                                 ("Save song" :command com-save-song)
                                 (nil :divider t)
                                 ("Quit" :command com-quit)))

(clim:make-command-table 'menubar-command-table
                         :errorp nil
                         :menu '(("File" :menu file-command-table)
                                 ("Song" :command com-pick-song)
                                 #+ (or) ("Help" :command nil)))


;;; Translators
(clim:define-presentation-to-command-translator tr-show-parts
    (parts-view-oid com-show-parts clovetree)
    (object)
  (list object))


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
                  :view +song-parts-view-view+)
    (princ "La la la." stream)))


(defun run (&rest args &key songs new-process)
  (declare (ignore songs))
  (remf args :new-process)
  (let ((frame (apply #'clim:make-application-frame 'clovetree args)))
    (if new-process
        (clim-sys:make-process (lambda () (clim:run-frame-top-level frame)))
        (clim:run-frame-top-level frame))))
