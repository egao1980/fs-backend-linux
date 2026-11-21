(in-package #:fs-backend-linux/tests)

;;; JOURNAL-CLOSE releases the inotify fd and its watches; no fd leaks.

(deflinux inotify-close-releases-fd-and-watches
  (with-scratch (root)
    (put root "a/b/c.txt" "c")
    (let* ((before (inotify-fds))
           (j (watch root))
           (fd (inotify-journal-fd j)))
      (ok (integerp fd))
      (ok (member fd (inotify-fds)) "the fd is an inotify instance of this process")
      (ok (not (member fd before)) "a new one")
      (ok (= 3 (kernel-watch-count fd)) "root, a, a/b")
      (ok (= 3 (inotify-journal-watch-count j)))
      (ok (null (pathlib:journal-close j)))
      (ok (not (pathlib:journal-open-p j)))
      (ok (null (inotify-journal-fd j)))
      (ok (= 0 (inotify-journal-watch-count j)))
      (ok (null (inotify-journal-watched-directories j)))
      (ok (equal before (inotify-fds)) "the fd is closed")
      (ok (null (pathlib:journal-close j)) "a second close does nothing")
      (ok (equal before (inotify-fds)))
      (ok (signals (pathlib:journal-drain j) 'pathlib:journal-error) "drain after close")
      (ok (signals (inject-inotify-event j +in-q-overflow+ :wd -1) 'pathlib:journal-error)))))

(deflinux inotify-area-close-releases-fd
  (with-scratch (root)
    (put root "base/d/a.txt" "a")
    (let* ((before (inotify-fds))
           (area (pathlib:open-area (sub root "base" :directory t)
                                    :directory (sub root "area" :directory t)
                                    :journal :inotify)))
      (ok (= (1+ (length before)) (length (inotify-fds))))
      (put (pathlib:area-root area) "d/b.txt" "b")
      (ok (equal '(:added) (mapcar #'pathlib:fs-change-kind (pathlib:area-changes area))))
      (pathlib:close-area area)
      (ok (equal before (inotify-fds)) "close-area closes the journal's fd"))))

(deflinux inotify-no-fd-leak-after-50-cycles
  (with-scratch (root)
    (put root "a/b/c.txt" "c")
    (put root "d/e.txt" "e")
    (let ((before (inotify-fds)))
      (dotimes (k 50)
        (let ((j (watch root)))
          (put root (format nil "a/f~D.txt" k) "f")
          (when (evenp k) (drain j))
          (when (zerop (mod k 10))
            (inject-inotify-event j +in-q-overflow+ :wd -1)
            (drain j))
          (pathlib:journal-close j)))
      (ok (equal before (inotify-fds)) "50 open/close cycles leave no inotify fd behind"))))

(deflinux inotify-no-fd-leak-after-lost-roots
  ;; Closing journals whose root was deleted (the kernel already dropped the
  ;; root watch) still closes every fd.
  (with-scratch (scratch)
    (let ((before (inotify-fds)))
      (dotimes (k 50)
        (let* ((root (sub scratch (format nil "r~D" k) :directory t))
               (j (progn (put root "s/x.txt" "x") (watch root))))
          (pathlib:rmtree root)
          (when (oddp k) (drain j))
          (pathlib:journal-close j)))
      (ok (equal before (inotify-fds))))))

(deflinux inotify-fs-watch-missing-directory
  ;; FS-WATCH on a missing directory signals PATH-NOT-FOUND and leaves no fd.
  (with-scratch (root)
    (let ((before (inotify-fds)))
      (ok (every (lambda (k)
                   (handler-case (progn (watch (sub root (format nil "missing~D" k) :directory t)) nil)
                     (pathlib:path-not-found () t)))
                 (loop for k below 50 collect k))
          "50 times PATH-NOT-FOUND")
      (ok (equal before (inotify-fds))))))
