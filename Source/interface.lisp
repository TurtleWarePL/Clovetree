(in-package #:clovetree-implementation)

(defclass song-info-pane (clim:application-pane) ())
(defclass song-main-pane (clim:application-pane) ())

(clim:define-application-frame clovetree ()
  ((songs :initarg :songs :accessor songs)
   (current-song :accessor current-song))
  (:geometry :width 970 :height 600)
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
                 (4/5 app)
                 (clim:make-pane 'clime:box-adjuster-gadget)
                 (1/5 int))))
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
  (setf (current-song frame) (first songs)))

(defmethod display ((frame clovetree) (stream song-info-pane))
  (clim:present (current-song frame) 'song
                :stream stream
                :view +song-information-view+
                :single-box t)
  (terpri stream)
  (clim:present (current-song frame) 'song
                :stream stream
                :view +song-parts-view+))

(defun run (&rest args &key songs new-process)
  (declare (ignore songs))
  (remf args :new-process)
  (let ((frame (apply #'clim:make-application-frame 'clovetree args)))
    (if new-process
        (clim-sys:make-process (lambda () (clim:run-frame-top-level frame)))
        (clim:run-frame-top-level frame))))

(define-clovetree-command (com-pick-song :name t) ((song song))
  (format *standard-output* "~s" song))

#+ (or)
(run :songs (list (make-instance 'song :title "Say you love me"
                                       :parts '(part1 part2))
                  (make-instance 'song :title "Say you hate me")
                  (make-instance 'song :title "Don't say nothing")))
