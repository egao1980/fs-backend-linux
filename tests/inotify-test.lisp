(in-package #:fs-backend-linux/tests)

;;; inotify-specific behaviour: exact paths, the add-watch race, overflow and
;;; the watch limit, the root going away, moves across the root, what is
;;; watched (ignored directories and symlinks are not).

;;; --- exact paths --------------------------------------------------------------

(deflinux inotify-reports-exact-paths
  (with-scratch (root)
    (put root "a.txt" "a")
    (put root "src/deep/b.lisp" "b")
    (put root "gone.txt" "g")
    (put root "run.sh" "echo")
    (with-watch (j root)
      (ok (equal '(nil t) (drain j)) "nothing touched yet: complete and empty")
      (ok (equal '("" "src" "src/deep") (inotify-journal-watched-directories j)))
      (ok (= 3 (inotify-journal-watch-count j)))
      (ok (= 3 (kernel-watch-count (inotify-journal-fd j))) "the kernel agrees")
      (put root "a.txt" "zzz")
      (put root "src/deep/b.lisp" "bb")
      (put root "src/deep/new.lisp" "n")
      (pathlib:unlink (sub root "gone.txt"))
      (pathlib:fs-chmod (pathlib:path-filesystem root) (native (sub root "run.sh")) #o755)
      (ok (equal '(("a.txt" "gone.txt" "run.sh" "src/deep/b.lisp" "src/deep/new.lisp") t)
                 (drain j :settle 0))
          "a watched directory reports each entry's own path, with :settle 0")
      (ok (equal '(nil t) (drain j)) "a drain consumes what it reported"))))

(deflinux inotify-root-self-events-not-reported
  ;; Events on the root itself (no name) are not entries: they report nothing,
  ;; in particular not "" (which would mean "rescan everything").
  (with-scratch (root)
    (put root "a.txt" "a")
    (with-watch (j root)
      (pathlib:fs-chmod (pathlib:path-filesystem root) (native-dir root) #o750)
      (ok (equal '(nil t) (drain j))))))

(deflinux inotify-child-process-edits
  (with-scratch (root)
    (put root "a.txt" "a")
    (mkdirs root "d")
    (with-watch (j root)
      (sh root "printf b > b.txt && printf zz > a.txt && printf c > d/c.txt && mv d/c.txt d/c2.txt")
      (ok (equal '(("a.txt" "b.txt" "d/c.txt" "d/c2.txt") t) (drain j))))))

;;; --- the add-watch race -----------------------------------------------------------

(deflinux inotify-new-directory-filled-at-once
  ;; The directory is created and filled before the drain that sees its
  ;; IN_CREATE adds the watch: its own path covers what is inside.
  (with-scratch (root)
    (with-watch (j root)
      (put root "d/1.txt" "1")
      (put root "d/e/2.txt" "2")
      (mkdirs root "d/e/f/g")
      (destructuring-bind (paths complete-p) (drain j)
        (ok complete-p)
        (ok (all-covered-p '("d" "d/1.txt" "d/e" "d/e/2.txt" "d/e/f/g") paths)
            (format nil "the new tree is covered: ~S" paths)))
      (ok (equal '("" "d" "d/e" "d/e/f" "d/e/f/g") (inotify-journal-watched-directories j))
          "the whole new tree is watched after that drain")
      (put root "d/e/f/g/3.txt" "3")
      (ok (equal '(("d/e/f/g/3.txt") t) (drain j)) "and later edits deep inside are exact"))))

(deflinux inotify-new-directories-from-a-child-process
  ;; mkdir -p + write in a tight loop in another process: no edit lost.
  (with-scratch (root)
    (with-watch (j root)
      (sh root "i=0; while [ $i -lt 40 ]; do mkdir -p n$i/a/b && printf x > n$i/a/b/f && i=$((i+1)); done")
      (destructuring-bind (paths complete-p) (drain j)
        (ok complete-p)
        (ok (all-covered-p (loop for i below 40 collect (format nil "n~D/a/b/f" i)) paths)
            "every file in every new directory is covered"))
      (ok (= (+ 1 (* 3 40)) (inotify-journal-watch-count j)) "root + 3 directories per loop")
      (put root "n39/a/b/g" "g")
      (ok (equal '(("n39/a/b/g") t) (drain j))))))

(deflinux inotify-new-subdirectory-reports-itself
  ;; A directory created inside a watched one is reported by its own path,
  ;; which covers what was written into it before its watch existed.
  (with-scratch (root)
    (with-watch (j root)
      (dotimes (k 20)
        (put root (format nil "r~D/s/t.txt" k) "t"))
      (ok (complete-with j (loop for k below 20 collect (format nil "r~D/s/t.txt" k))))
      (dotimes (k 20)
        (put root (format nil "r~D/s/u/v.txt" k) "v"))
      (destructuring-bind (paths complete-p) (drain j)
        (ok complete-p)
        (ok (every (lambda (k) (member (format nil "r~D/s/u" k) paths :test #'string=))
                   (loop for k below 20 collect k))
            "a new subdirectory of a watched directory reports its own path")))))

;;; --- overflow and the watch limit --------------------------------------------------

(deflinux inotify-injected-overflow
  (with-scratch (root)
    (put root "a.txt" "a")
    (with-watch (j root)
      (put root "a.txt" "b")
      (inject-inotify-event j +in-q-overflow+ :wd -1)
      (ok (equal '(nil nil) (drain j)) "IN_Q_OVERFLOW: rescan everything")
      (ok (equal '(nil t) (drain j)) "the journal recovers: the next drain is complete")
      (put root "a.txt" "c")
      (ok (equal '(("a.txt") t) (drain j))))))

(deflinux inotify-overflow-resyncs-watches
  ;; Directories made while events were lost are watched again by the drain
  ;; that saw the overflow.
  (with-scratch (root)
    (with-watch (j root)
      (inject-inotify-event j +in-q-overflow+ :wd -1)
      (put root "late/inner/x.txt" "x")
      (ok (equal '(nil nil) (drain j)))
      (ok (equal '("" "late" "late/inner") (inotify-journal-watched-directories j)))
      (put root "late/inner/y.txt" "y")
      (ok (equal '(("late/inner/y.txt") t) (drain j))))))

(deflinux inotify-overflow-drops-removed-watches
  ;; A resync forgets directories that disappeared while events were lost.
  (with-scratch (root)
    (put root "old/sub/x.txt" "x")
    (with-watch (j root)
      (ok (equal '("" "old" "old/sub") (inotify-journal-watched-directories j)))
      (inject-inotify-event j +in-q-overflow+ :wd -1)
      (mv (sub root "old") (sub root "renamed"))
      (ok (equal '(nil nil) (drain j)))
      (ok (equal '("" "renamed" "renamed/sub") (inotify-journal-watched-directories j)))
      (put root "renamed/sub/y.txt" "y")
      (ok (equal '(("renamed/sub/y.txt") t) (drain j)) "no path under the old name"))))

(deflinux inotify-real-queue-overflow
  ;; More events than fs.inotify.max_queued_events between two drains.
  (let ((limit (max-queued-events)))
    (if (or (null limit) (> limit 100000))
        (skip (format nil "fs.inotify.max_queued_events is ~A: too many files to force an overflow" limit))
        (with-scratch (root)
          (mkdirs root "flood")
          (with-watch (j root)
            (sh (sub root "flood" :directory t)
                (format nil "i=0; while [ $i -lt ~D ]; do : > f$i; i=$((i+1)); done" (+ limit 100)))
            (ok (equal '(nil nil) (drain j))
                (format nil "~D creates in one directory overflow a queue of ~D" (+ limit 100) limit))
            (pathlib:rmtree (sub root "flood"))
            (drain j)
            (put root "after.txt" "a")
            (ok (equal '(("after.txt") t) (drain j)) "and the journal recovers"))))))

(deflinux inotify-watch-limit-at-open
  ;; More directories than *INOTIFY-MAX-WATCHES* at FS-WATCH: no error, but
  ;; the journal cannot promise anything.
  (with-scratch (root)
    (mkdirs root "a")
    (mkdirs root "b")
    (mkdirs root "c")
    (let ((j (let ((*inotify-max-watches* 2)) (watch root))))
      (unwind-protect
           (progn
             (ok (<= (inotify-journal-watch-count j) 2))
             (ok (= (inotify-journal-watch-count j) (kernel-watch-count (inotify-journal-fd j))))
             (ok (equal '(nil nil) (drain j)) "a directory without a watch: rescan")
             (put root "a/x.txt" "x")
             (ok (equal '(nil nil) (drain j)) "still missing a watch: still rescan"))
        (pathlib:journal-close j)))))

(deflinux inotify-watch-limit-on-new-directories
  (with-scratch (root)
    (mkdirs root "a")
    (let ((j (let ((*inotify-max-watches* 3)) (watch root))))
      (unwind-protect
           (progn
             (ok (equal '(nil t) (drain j)) "root and a: within the limit")
             (mkdirs root "b")
             (mkdirs root "c")
             (ok (equal '(nil nil) (drain j)) "the fourth watch fails: rescan")
             (ok (= 3 (inotify-journal-watch-count j)))
             (put root "a/x.txt" "x")
             (ok (equal '(nil nil) (drain j)) "each drain retries and still fails")
             (let ((missing (if (member "b" (inotify-journal-watched-directories j) :test #'string=)
                                "c" "b")))
               (pathlib:rmdir (sub root missing :directory t))
               (destructuring-bind (paths complete-p) (drain j)
                 (ok complete-p "every remaining directory is watched again")
                 (ok (member missing paths :test #'string=)))))
        (pathlib:journal-close j)))))

;;; --- the root -------------------------------------------------------------------

(deflinux inotify-root-deleted
  (with-scratch (scratch)
    (let ((root (sub scratch "root" :directory t)))
      (put root "a.txt" "a")
      (put root "d/b.txt" "b")
      (let ((j (watch root)))
        (unwind-protect
             (progn
               (pathlib:rmtree root)
               (ok (equal '(nil nil) (drain j)) "the root was deleted: rescan")
               (pathlib:mkdir root :parents t :exist-ok t)
               (put root "a.txt" "again")
               (ok (equal '(nil nil) (drain j)) "and every later drain too, even with a new root"))
          (pathlib:journal-close j))
        (ok (not (pathlib:journal-open-p j)))
        (ok (null (inotify-journal-fd j)) "close after the root went away releases the fd")))))

(deflinux inotify-root-moved
  (with-scratch (scratch)
    (let ((root (sub scratch "root" :directory t)))
      (put root "a.txt" "a")
      (with-watch (j root)
        (mv root (sub scratch "elsewhere"))
        (ok (equal '(nil nil) (drain j)) "the root was moved away: rescan")
        (put (sub scratch "elsewhere" :directory t) "b.txt" "b")
        (ok (equal '(nil nil) (drain j)) "edits under its new name are not reported as root edits")))))

(deflinux inotify-root-ignored-event
  ;; IN_IGNORED on the root's watch (the kernel dropped it): the root is lost.
  (with-scratch (root)
    (with-watch (j root)
      (inject-inotify-event j +in-ignored+ :wd :root)
      (ok (equal '(nil nil) (drain j)))
      (put root "a.txt" "a")
      (ok (equal '(nil nil) (drain j))))))

;;; --- moves across the root --------------------------------------------------------

(deflinux inotify-move-in
  (with-scratch (scratch)
    (let ((root (sub scratch "root" :directory t))
          (outside (sub scratch "outside" :directory t)))
      (pathlib:mkdir root :parents t :exist-ok t)
      (put outside "in.txt" "i")
      (put outside "tree/sub/f.txt" "f")
      (with-watch (j root)
        (mv (sub outside "in.txt") (sub root "in.txt"))
        (mv (sub outside "tree") (sub root "tree"))
        (destructuring-bind (paths complete-p) (drain j)
          (ok complete-p)
          (ok (member "in.txt" paths :test #'string=))
          (ok (member "tree" paths :test #'string=) "a moved-in directory reports its own path"))
        (ok (equal '("" "tree" "tree/sub") (inotify-journal-watched-directories j))
            "and is watched with its subtree")
        (put root "tree/sub/g.txt" "g")
        (ok (equal '(("tree/sub/g.txt") t) (drain j)))))))

(deflinux inotify-move-out
  (with-scratch (scratch)
    (let ((root (sub scratch "root" :directory t))
          (outside (sub scratch "outside" :directory t)))
      (pathlib:mkdir outside :parents t :exist-ok t)
      (put root "f.txt" "f")
      (put root "out/sub/x.txt" "x")
      (put root "keep/k.txt" "k")
      (with-watch (j root)
        (ok (equal '("" "keep" "out" "out/sub") (inotify-journal-watched-directories j)))
        (mv (sub root "f.txt") (sub outside "f.txt"))
        (mv (sub root "out") (sub outside "out"))
        (destructuring-bind (paths complete-p) (drain j)
          (ok complete-p)
          (ok (member "f.txt" paths :test #'string=))
          (ok (member "out" paths :test #'string=)))
        (ok (equal '("" "keep") (inotify-journal-watched-directories j))
            "the watches of a moved-out tree are removed")
        (ok (= 2 (kernel-watch-count (inotify-journal-fd j))))
        (put outside "out/sub/y.txt" "y")
        (ok (equal '(nil t) (drain j)) "edits outside the root are not reported")))))

(deflinux inotify-rename-within
  (with-scratch (root)
    (put root "a/sub/x.txt" "x")
    (with-watch (j root)
      (mv (sub root "a") (sub root "b"))
      (destructuring-bind (paths complete-p) (drain j)
        (ok complete-p)
        (ok (member "a" paths :test #'string=))
        (ok (member "b" paths :test #'string=)))
      (ok (equal '("" "b" "b/sub") (inotify-journal-watched-directories j))
          "the watches follow the directory")
      (put root "b/sub/y.txt" "y")
      (ok (equal '(("b/sub/y.txt") t) (drain j)) "reported under the new name"))))

(deflinux inotify-move-across-directories
  (with-scratch (root)
    (put root "src/x.txt" "x")
    (mkdirs root "dst")
    (put root "src/tree/t.txt" "t")
    (with-watch (j root)
      (mv (sub root "src/x.txt") (sub root "dst/x.txt"))
      (mv (sub root "src/tree") (sub root "dst/tree"))
      (ok (equal '(("dst/tree" "dst/x.txt" "src/tree" "src/x.txt") t) (drain j)))
      (ok (equal '("" "dst" "dst/tree" "src") (inotify-journal-watched-directories j)))
      (put root "dst/tree/u.txt" "u")
      (ok (equal '(("dst/tree/u.txt") t) (drain j))))))

;;; --- what is watched ------------------------------------------------------------

(deflinux inotify-ignored-and-symlinked-directories-not-watched
  (with-scratch (root)
    (put root ".gitignore" (format nil "build/~%*.log~%"))
    (put root "build/sub/o.o" "o")
    (put root "src/c.lisp" "c")
    (pathlib:create-symlink (sub root "lnk") "src")
    (with-watch (j root :ignore :auto)
      (ok (equal '("" "src") (inotify-journal-watched-directories j))
          "build/ (ignored) and lnk (a symlink) have no watch")
      (put root "build/new.o" "n")
      (put root "x.log" "l")
      (put root "src/d.lisp" "d")
      (ok (equal '(("src/d.lisp") t) (drain j)))
      (mkdirs root "build/more")
      (ok (equal '("" "src") (inotify-journal-watched-directories j))
          "a new directory under an ignored one is not watched either"))))
