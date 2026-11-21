(in-package #:fs-backend-linux/tests)

;;; The R4b law through cl-stack-pathlib's journal suite: FS-AREA-CHANGES with
;;; the :INOTIFY journal is EQUAL to the full rescan after every corpus step.

(deflinux inotify-journal-suite
  (multiple-value-bind (failures steps) (run-journal-suite :inotify)
    (ok (null failures) (format nil ":inotify keeps the law: ~S" failures))
    (ok (equal (journal-suite-steps :backend :local) steps) "every applicable step ran")
    (ok (member :burst steps))
    (ok (member :child-process steps))
    (ok (member :directory-rename steps))))

(deflinux inotify-journal-suite-settle-0
  ;; The kernel queues events before the write returns: no settle needed.
  (ok (null (run-journal-suite :inotify :settle 0))))

(deflinux inotify-journal-suite-auto
  ;; :AUTO picks :INOTIFY on a local directory and keeps the law.
  (ok (null (run-journal-suite :auto))))
