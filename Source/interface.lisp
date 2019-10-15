(in-package #:clovetree-implementation)

(clim:define-application-frame clovetree ()
  ((songs :initarg :songs :accessor songs))
  (:pane :application :display-function #'display))

(defmethod initialize-instance :after ((frame clovetree) &key songs)
  (if songs
      (mapc (lambda (song)
              (check-type song song))
            songs)
      (setf (songs frame) (list (make-instance 'song)))))

(defun run (&rest args &key songs new-process)
  (declare (ignore songs))
  (remf args :new-process)
  (let ((frame (apply #'clim:make-application-frame 'clovetree args)))
    (if new-process
        (clim-sys:make-process (lambda () (clim:run-frame-top-level frame)))
        (clim:run-frame-top-level frame))))
