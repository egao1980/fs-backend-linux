(in-package #:fs-backend-linux/tests)

;;; Linux, on the volume of the temp directory (ext4 on the runner host, the
;;; container's overlay in the ci-base job): no reflink there, so the probe
;;; says :UNSUPPORTED-VOLUME, :AUTO copies with that reason, and FICLONE-FILE
;;; answers (NIL :UNSUPPORTED-VOLUME). The probe never writes the source and
;;; leaves nothing in the destination. open(2) failures are FICLONE-ERRORs.

(defun scratch-tree (root)
  "A small source tree under ROOT/src: two text files, a nested file, a
script, an empty file."
  (put root "src/a.txt" "alpha")
  (put root "src/sub/b.txt" "beta")
  (put root "src/sub/deep/c.txt" "gamma")
  (chmod (native (put root "src/run.sh" "#!/bin/sh")) #o755)
  (put root "src/empty" "")
  (sub root "src" :directory t))

(defun forced-ficlone ()
  "Register :FICLONE-FORCED (FICLONE-FILE with no probe: always available) in
the current dynamic binding of PATHLIB:*CLONE-MECHANISMS*, so pathlib calls
FICLONE-FILE even where the probe would refuse."
  (pathlib:register-clone-mechanism :ficlone-forced 'ficlone-file :rank 1000))

(defficlone ficlone-temp-volume-unsupported
  (with-scratch (root)
    (let* ((fs (pathlib:path-filesystem root))
           (src (scratch-tree root))
           (dir (native-dir root)))
      (ok (equal '(nil :unsupported-volume)
                 (multiple-value-list (ficlone-available-p fs (native-dir src) dir)))
          (format nil "~A: no reflink" (volume-type dir)))
      (ok (equal '(nil :unsupported-volume)
                 (multiple-value-list (pathlib:fs-clone-supported-p fs (native-dir src) dir))))
      (ok (equal '(:copy) (pathlib:clone-mechanisms fs (native-dir src) dir)) "only :copy is offered"))))

(defficlone ficlone-auto-copies-with-reason
  (with-scratch (root)
    (let* ((fs (pathlib:path-filesystem root))
           (src (scratch-tree root))
           (r (pathlib:fs-clone-tree fs src (sub root "dst" :directory t))))
      (ok (eq :copy (pathlib:clone-report-mechanism r)))
      (ok (eq :unsupported-volume (pathlib:clone-report-reason r)))
      (ok (eql 0 (pathlib:clone-report-cloned r)))
      (ok (eql 5 (pathlib:clone-report-copied r)))
      (ok (equal "beta" (pathlib:read-text (sub root "dst/sub/b.txt"))))
      (ok (signals (pathlib:fs-clone-tree fs src (sub root "dst2" :directory t) :mechanism :ficlone)
              'pathlib:unsupported-operation)
          "by name: refused")
      (ok (signals (pathlib:fs-clone-tree fs src (sub root "dst3" :directory t) :mechanism :clone)
              'pathlib:unsupported-operation)
          ":clone: refused")
      (ok (not (pathlib:exists-p (sub root "dst2" :directory t))))
      (ok (not (pathlib:exists-p (sub root "dst3" :directory t)))))))

(defficlone ficlone-area-auto-copies
  (with-scratch (root)
    (scratch-tree root)
    (let ((area (pathlib:open-area (sub root "src" :directory t)
                                   :directory (sub root "area" :directory t))))
      (unwind-protect
           (let ((r (pathlib:area-clone-report area)))
             (ok (eq :copy (pathlib:area-mechanism area)))
             (ok (eq :copy (pathlib:clone-report-mechanism r)))
             (ok (eq :unsupported-volume (pathlib:clone-report-reason r))))
        (pathlib:close-area area)))))

(defficlone ficlone-probe-reads-only
  (with-scratch (root)
    (let* ((fs (pathlib:path-filesystem root))
           (src (native-dir (scratch-tree root)))
           (dst (native-dir (mkdirs root "dst")))
           (before (tree-state src))
           (dst-before (entry-names dst)))
      (sleep 1.1)                       ; a write would move an mtime visibly
      (multiple-value-list (ficlone-available-p fs src dst))
      (ok (equal before (tree-state src)) "the source tree is unchanged: entries, sizes, modes, mtimes")
      (ok (equal dst-before (entry-names dst)) "the destination is as it was")
      (ok (null (probe-leftovers dst)) "no probe file left")
      (ok (null (probe-leftovers src)) "no probe file in the source either"))))

(defficlone ficlone-probe-read-only-source
  ;; A source directory nobody may write to is probed all the same.
  (if (running-as-root-p)
      (skip +root-skip-note+)
      (with-scratch (root)
        (let* ((fs (pathlib:path-filesystem root))
               (src (native-dir (scratch-tree root)))
               (dst (native-dir (mkdirs root "dst"))))
          (chmod (native (sub root "src/sub/deep" :directory t)) #o555)
          (chmod (native (sub root "src/sub" :directory t)) #o555)
          (chmod src #o555)
          (unwind-protect
               (ok (equal '(nil :unsupported-volume)
                          (multiple-value-list (ficlone-available-p fs src dst)))
                   "not :unavailable: nothing is created in SOURCE-DIR")
            (chmod src #o755)
            (chmod (native (sub root "src/sub" :directory t)) #o755)
            (chmod (native (sub root "src/sub/deep" :directory t)) #o755))))))

(defficlone ficlone-probe-empty-source
  ;; No regular file to borrow: the destination-only probe (two temp files there).
  (with-scratch (root)
    (let* ((fs (pathlib:path-filesystem root))
           (src (native-dir (mkdirs root "src/only/dirs")))
           (dst (native-dir (mkdirs root "dst"))))
      (declare (ignore src))
      (let ((src-root (native-dir (sub root "src" :directory t)))
            (before (tree-state (native-dir (sub root "src" :directory t)))))
        (ok (equal '(nil :unsupported-volume)
                   (multiple-value-list (ficlone-available-p fs src-root dst))))
        (ok (equal before (tree-state src-root)) "source unchanged")
        (ok (null (entry-names dst)) "both temp files removed")))))

(defficlone ficlone-probe-unwritable-destination
  (if (running-as-root-p)
      (skip +root-skip-note+)
      (with-scratch (root)
        (let* ((fs (pathlib:path-filesystem root))
               (src (native-dir (scratch-tree root)))
               (dst (native-dir (mkdirs root "dst"))))
          (chmod dst #o555)
          (unwind-protect
               (ok (equal '(nil :unavailable)
                          (multiple-value-list (ficlone-available-p fs src dst)))
                   "a temp file cannot be created: :unavailable, no condition")
            (chmod dst #o755))))))

(defficlone ficlone-file-unsupported-volume
  (with-scratch (root)
    (let* ((fs (pathlib:path-filesystem root))
           (src (native (put root "a.txt" "alpha")))
           (dst (native (sub root "b.txt"))))
      (ok (equal '(nil :unsupported-volume) (multiple-value-list (ficlone-file fs src dst))))
      (ok (not (probe-file dst)) "DESTINATION unlinked")
      (ok (equal "alpha" (pathlib:read-text (sub root "a.txt"))) "SOURCE untouched"))))

(defficlone ficlone-file-open-errors
  (with-scratch (root)
    (let* ((fs (pathlib:path-filesystem root))
           (src (native (put root "a.txt" "alpha"))))
      (testing "missing source: ENOENT, no destination made"
        (let ((c (handler-case (progn (ficlone-file fs (native (sub root "nope.txt")) (native (sub root "n.txt"))) nil)
                   (ficlone-error (c) c))))
          (ok (typep c 'ficlone-error))
          (ok (and c (equal "open" (pathlib:clone-error-operation c))))
          (ok (and c (eql 2 (pathlib:clone-error-code c))))
          (ok (not (probe-file (native (sub root "n.txt")))))))
      (testing "existing destination: EEXIST, left as it was"
        (put root "taken.txt" "mine")
        (let ((c (handler-case (progn (ficlone-file fs src (native (sub root "taken.txt"))) nil)
                   (ficlone-error (c) c))))
          (ok (and c (eql 17 (pathlib:clone-error-code c))))
          (ok (equal "mine" (pathlib:read-text (sub root "taken.txt"))))))
      (testing "symlink source: O_NOFOLLOW, ELOOP"
        (sh root "ln -s a.txt link")
        (let ((c (handler-case (progn (ficlone-file fs (native (sub root "link")) (native (sub root "l.txt"))) nil)
                   (ficlone-error (c) c))))
          (ok (and c (eql 40 (pathlib:clone-error-code c))))
          (ok (not (probe-file (native (sub root "l.txt"))))))))))

(defficlone ficlone-file-eacces-offers-use-copy
  ;; EACCES on the destination directory: a FICLONE-ERROR (errno 13), raised
  ;; inside pathlib's USE-COPY restart.
  (if (running-as-root-p)
      (skip +root-skip-note+)
      (with-scratch (root)
        (let* ((fs (pathlib:path-filesystem root))
               (src (native (put root "a.txt" "alpha")))
               (locked (native-dir (mkdirs root "locked"))))
          (chmod locked #o555)
          (unwind-protect
               (progn
                 (let ((c (handler-case (progn (ficlone-file fs src (merge-pathnames "b.txt" locked)) nil)
                            (ficlone-error (c) c))))
                   (ok (typep c 'ficlone-error))
                   (ok (and c (eql 13 (pathlib:clone-error-code c))))
                   (ok (and c (eq :ficlone (pathlib:clone-error-mechanism c))))
                   (ok (and c (pathlib:path-error-path c)) "names the file"))
                 (let ((pathlib:*clone-mechanisms* pathlib:*clone-mechanisms*)
                       (seen nil))
                   (forced-ficlone)
                   (block called
                     (handler-bind ((ficlone-error
                                      (lambda (c)
                                        (setf seen (list (pathlib:clone-error-code c)
                                                         (and (find-restart 'pathlib:use-copy c) t)))
                                        (return-from called))))
                       (pathlib:fs-clone-file fs src (merge-pathnames "c.txt" locked)
                                              :mechanism :ficlone-forced)))
                   (ok (equal '(13 t) seen) "signalled through pathlib with USE-COPY offered")))
            (chmod locked #o755))))))

(defficlone ficlone-per-file-fallback-on-ext4
  ;; With the probe bypassed, every file falls back to the copy rung with
  ;; :UNSUPPORTED-VOLUME and the tree is whole.
  (with-scratch (root)
    (let ((fs (pathlib:path-filesystem root))
          (src (scratch-tree root))
          (pathlib:*clone-mechanisms* pathlib:*clone-mechanisms*))
      (forced-ficlone)
      (let ((r (pathlib:fs-clone-tree fs src (sub root "dst" :directory t) :mechanism :ficlone-forced)))
        (ok (eq :ficlone-forced (pathlib:clone-report-mechanism r)))
        (ok (eql 0 (pathlib:clone-report-cloned r)))
        (ok (eql 5 (pathlib:clone-report-copied r)))
        (ok (equal '(:unsupported-volume)
                   (remove-duplicates (mapcar #'cdr (pathlib:clone-report-fallbacks r)))))
        (ok (equal "gamma" (pathlib:read-text (sub root "dst/sub/deep/c.txt"))))
        (ok (eql #o755 (file-mode (native (sub root "dst/run.sh")))))))))

(defficlone ficlone-copy-suite-with-ficlone-loaded
  ;; The :copy rung keeps every law with :ficlone registered (and refusing).
  (multiple-value-bind (failures steps) (run-clone-suite :copy)
    (ok (null failures) (format nil ":copy keeps the clone laws: ~S" failures))
    (ok (equal (clone-suite-steps :mechanism :copy) steps) "every applicable step ran")))
