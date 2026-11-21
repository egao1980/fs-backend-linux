(in-package #:fs-backend-linux/tests)

;;; Registration at load time, on every OS; availability by OS and filesystem.

(deftest inotify-registered-at-load
  (let ((names (pathlib:watch-mechanisms)))
    (ok (member :inotify names) "loading fs-backend-linux registers :inotify")
    (ok (< (position :inotify names) (position :poll names)) "ranked above :poll")
    (ok (integerp +inotify-rank+))
    (ok (plusp +inotify-rank+) "above the default rank 0")))

(deftest inotify-registration-scoped-by-let
  ;; The registration is an ordinary registry entry: a LET scopes its removal.
  (let ((pathlib:*watch-mechanisms* pathlib:*watch-mechanisms*))
    (pathlib:unregister-watch-mechanism :inotify)
    (ok (not (member :inotify (pathlib:watch-mechanisms)))))
  (ok (member :inotify (pathlib:watch-mechanisms)) "back outside the LET"))

(deftest inotify-unavailable-on-memory-filesystem
  (let ((fs (pathlib:make-memory-filesystem)))
    (pathlib:with-filesystem (fs)
      (pathlib:mkdir "/w" :parents t)
      (ok (not (inotify-available-p fs #p"/w/")))
      (ok (not (member :inotify (pathlib:watch-mechanisms fs "/w"))) "memory: not offered")
      (ok (eq :poll (first (pathlib:watch-mechanisms fs "/w"))))
      (ok (signals (pathlib:fs-watch fs "/w" :mechanism :inotify)
              'pathlib:unsupported-operation)))))

(deftest inotify-unavailable-off-linux
  (if (linux-p)
      (skip "Linux: the off-Linux behaviour is checked on macOS and Windows")
      (with-scratch (root)
        (let ((fs (pathlib:path-filesystem root)))
          (ok (not (inotify-available-p)))
          (ok (not (inotify-available-p fs (native-dir root))))
          (ok (not (member :inotify (pathlib:watch-mechanisms fs (native-dir root))))
              "not offered for a local directory")
          (ok (signals (pathlib:fs-watch fs (native-dir root) :mechanism :inotify)
                  'pathlib:unsupported-operation))
          (let ((j (pathlib:fs-watch fs (native-dir root))))
            (ok (not (eq :inotify (pathlib:journal-mechanism j))) ":auto does not pick it")
            (pathlib:journal-close j))))))

(deflinux inotify-available-on-linux
  (with-scratch (root)
    (let ((fs (pathlib:path-filesystem root)))
      (ok (inotify-available-p))
      (ok (inotify-available-p fs (native-dir root)))
      (ok (eq :inotify (first (pathlib:watch-mechanisms fs (native-dir root))))
          ":inotify is the best available mechanism for a local directory")
      (let ((j (pathlib:fs-watch fs (native-dir root))))
        (unwind-protect
             (progn
               (ok (inotify-journal-p j) ":auto picks it")
               (ok (eq :inotify (pathlib:journal-mechanism j)))
               (ok (pathlib:journal-open-p j))
               (ok (integerp (inotify-journal-fd j)))
               (ok (eq fs (pathlib:journal-filesystem j)))
               (ok (equal (native-dir root) (pathlib:path-pathname (pathlib:journal-root j)))))
          (pathlib:journal-close j))))))

(deflinux inotify-area-auto
  ;; :JOURNAL :AUTO on an area attaches the :INOTIFY journal on Linux.
  (with-scratch (root)
    (put root "base/a.txt" "a")
    (let ((area (pathlib:open-area (sub root "base" :directory t)
                                   :directory (sub root "area" :directory t))))
      (unwind-protect
           (let ((j (pathlib:area-journal area)))
             (ok (inotify-journal-p j))
             (ok (equal '("") (inotify-journal-watched-directories j)) "the copy's root"))
        (pathlib:close-area area)))))
