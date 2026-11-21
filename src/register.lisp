(in-package #:fs-backend-linux)

;;; Loading the system registers :INOTIFY on every OS. The probe makes it
;;; available only on Linux, for a directory on a LOCAL-FILESYSTEM, when an
;;; inotify instance can be made right now (it opens one and closes it), so
;;; :AUTO falls back to :POLL rather than failing once
;;; fs.inotify.max_user_instances is used up.

(defun inotify-available-p (&optional fs directory)
  "True when FS-WATCH can make an :INOTIFY journal: on Linux, FS (when given)
a LOCAL-FILESYSTEM, and inotify_init1 succeeding now. DIRECTORY (an absolute
directory pathname on FS) is accepted for the registry's probe and not
inspected further. Always NIL off Linux."
  (declare (ignorable fs directory))
  (cond ((not (%linux-p)) nil)
        ((and fs (not (typep fs 'pathlib:local-filesystem))) nil)
        (t (let ((fd (%inotify-init1 (logior +in-nonblock+ +in-cloexec+))))
             (when (>= fd 0)
               (%close fd)
               t)))))

(pathlib:register-watch-mechanism :inotify '%make-inotify-journal
                                  :rank +inotify-rank+
                                  :available-p 'inotify-available-p)
