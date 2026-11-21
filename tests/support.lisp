(in-package #:fs-backend-linux/tests)

;;; Helpers for the inotify tests: Linux-only tests, scratch trees, edits,
;;; drains, and what the kernel says about this process's inotify fds
;;; (/proc/<pid>/fd and /proc/<pid>/fdinfo).

(defun linux-p ()
  #+linux t
  #-linux nil)

(defparameter +skip-note+
  "inotify is Linux only: skipped on this OS (CI runs it on ubuntu-latest)")

(defmacro deflinux (name &body body)
  "A DEFTEST whose BODY runs on Linux only; elsewhere the test is one SKIP with a note."
  `(deftest ,name
     (if (linux-p)
         (progn ,@body)
         (skip +skip-note+))))

(defun local-fs () (pathlib:make-local-filesystem))

(defun call-with-scratch (fn)
  "Call FN with a fresh empty local directory PATH (by truename), *FILESYSTEM*
bound to a local filesystem; the directory is deleted afterwards."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "fs-backend-linux-~36R" (random (expt 36 8) (make-random-state t)))
                               (uiop:temporary-directory)))))
    (ensure-directories-exist dir)
    (unwind-protect
         (pathlib:with-filesystem ((local-fs))
           (funcall fn (pathlib:ensure-path (truename dir) :directory t)))
      (ignore-errors (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(defmacro with-scratch ((var) &body body)
  `(call-with-scratch (lambda (,var) ,@body)))

(defun sub (root rel &key directory)
  (let ((p (pathlib:join root rel)))
    (if directory (pathlib:ensure-directory p) p)))

(defun put (root rel &optional (text "x"))
  (let ((p (sub root rel)))
    (pathlib:mkdir (pathlib:parent p) :parents t :exist-ok t)
    (pathlib:write-text p text)
    p))

(defun mkdirs (root rel)
  (pathlib:mkdir (sub root rel) :parents t :exist-ok t))

(defun native (path)
  (pathlib:path-pathname path))

(defun native-dir (path)
  (uiop:ensure-directory-pathname (pathlib:path-pathname path)))

(defun sh (directory command)
  "Run COMMAND with /bin/sh in DIRECTORY (a PATH); → its stdout."
  (uiop:run-program (list "/bin/sh" "-c" command)
                    :directory (native-dir directory)
                    :output :string :error-output :string))

(defun mv (from to)
  "Rename FROM to TO (PATHs, absolute), files or directories, with mv(1)."
  (uiop:run-program (list "mv" (uiop:native-namestring (pathlib:path-pathname from))
                          (string-right-trim "/" (uiop:native-namestring (pathlib:path-pathname to))))
                    :output :string :error-output :string))

(defun watch (root &key ignore)
  "An :INOTIFY journal over ROOT (a directory PATH)."
  (pathlib:fs-watch (pathlib:path-filesystem root) (native-dir root)
                    :mechanism :inotify :ignore ignore))

(defmacro with-watch ((var root &rest keys) &body body)
  `(let ((,var (watch ,root ,@keys)))
     (unwind-protect (progn ,@body)
       (pathlib:journal-close ,var))))

(defun drain (journal &rest keys)
  "→ (paths complete-p) as a list."
  (multiple-value-list (apply #'pathlib:journal-drain journal keys)))

(defun covered-p (rel paths)
  "True when REL is one of PATHS or below one of them (a path covers its subtree)."
  (some (lambda (p)
          (or (string= p rel)
              (and (< (length p) (length rel))
                   (string= p rel :end2 (length p))
                   (char= #\/ (char rel (length p))))))
        paths))

(defun all-covered-p (rels paths)
  (every (lambda (r) (covered-p r paths)) rels))

(defun complete-with (journal rels)
  "Drain JOURNAL: T when COMPLETE-P is T and every REL is covered."
  (destructuring-bind (paths complete-p) (drain journal)
    (and complete-p (all-covered-p rels paths))))

;;; --- the kernel's view of this process ---------------------------------------

(defun proc-self ()
  "/proc/<pid>/ of this process, as a directory pathname."
  (truename #p"/proc/self/"))

(defun inotify-fds ()
  "File descriptors of this process that are inotify instances, sorted."
  (let* ((dir (uiop:native-namestring (proc-self)))
         (out (uiop:run-program
               (list "/bin/sh" "-c"
                     (format nil "for f in ~Afd/*; do printf '%s %s\\n' \"${f##*/}\" \"$(readlink \"$f\")\"; done"
                             dir))
               :output :lines :ignore-error-status t)))
    (sort (loop for line in out
                for space = (position #\Space line)
                when (and space (search "anon_inode:inotify" line :start2 space))
                  collect (parse-integer line :end space))
          #'<)))

(defun kernel-watch-count (fd)
  "Watches the kernel lists for the inotify FD of this process (fdinfo)."
  (let ((info (merge-pathnames (format nil "fdinfo/~D" fd) (proc-self))))
    (count-if (lambda (line) (eql 0 (search "inotify wd:" line)))
              (uiop:read-file-lines info))))

(defun max-queued-events ()
  "fs.inotify.max_queued_events, or NIL when it cannot be read."
  (ignore-errors
   (parse-integer (string-trim '(#\Space #\Newline)
                               (uiop:read-file-string #p"/proc/sys/fs/inotify/max_queued_events")))))
