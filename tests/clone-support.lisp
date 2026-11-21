(in-package #:fs-backend-linux/tests)

;;; Helpers for the FICLONE tests: reflink volumes from the environment,
;;; files with known content, sharing, and the user the tests run as.
;;;
;;; FS_BACKEND_LINUX_REFLINK_DIRS: colon-separated directories on reflink
;;; volumes (CI job `reflink`: /mnt/btrfs:/mnt/xfs). Unset: each reflink
;;; test is one SKIP with +REFLINK-SKIP-NOTE+.
;;; FS_BACKEND_LINUX_REQUIRE_REFLINK=1: a missing variable, a missing
;;; directory, or a directory where the probe does not say T FAILS instead,
;;; so the `reflink` job can never go green by skipping.

(defparameter +ficlone-linux-note+
  "FICLONE is Linux only: skipped on this OS (CI runs it on ubuntu-latest and in the `reflink` job)")

(defmacro defficlone (name &body body)
  "A DEFTEST whose BODY runs on Linux only; elsewhere one SKIP with +FICLONE-LINUX-NOTE+."
  `(deftest ,name
     (if (linux-p)
         (progn ,@body)
         (skip +ficlone-linux-note+))))

(defparameter +reflink-skip-note+
  "FICLONE needs a reflink volume: set FS_BACKEND_LINUX_REFLINK_DIRS (CI job `reflink`)")

(defun require-reflink-p ()
  (equal "1" (uiop:getenv "FS_BACKEND_LINUX_REQUIRE_REFLINK")))

(defun reflink-dir-strings ()
  "The entries of FS_BACKEND_LINUX_REFLINK_DIRS, in order (empty ones dropped)."
  (let ((v (uiop:getenv "FS_BACKEND_LINUX_REFLINK_DIRS")))
    (when (and v (plusp (length v)))
      (remove "" (uiop:split-string v :separator ":") :test #'string=))))

(defun dir-pathname (string)
  (uiop:ensure-directory-pathname (uiop:parse-native-namestring string)))

(defun volume-type (directory)
  "The filesystem type name of DIRECTORY (a pathname) per stat -f: \"btrfs\",
\"xfs\", \"ext2/ext3\", \"tmpfs\", \"overlayfs\", …"
  (string-trim '(#\Space #\Newline)
               (uiop:run-program (list "stat" "-f" "-c" "%T"
                                       (uiop:native-namestring directory))
                                 :output :string)))

(defun %reflink-volume-problem (string)
  "NIL when STRING names an existing directory where the :FICLONE probe
says T, else a description of the problem."
  (let ((dir (dir-pathname string)))
    (cond ((not (uiop:directory-exists-p dir))
           (format nil "~A does not exist" string))
          (t (let ((ok (ignore-errors
                        (ficlone-available-p (local-fs) (truename dir) (truename dir)))))
               (unless ok
                 (format nil "~A (~A): the :ficlone probe does not say T"
                         string (ignore-errors (volume-type dir)))))))))

(defun call-with-reflink-dirs (fn)
  "Call FN with (dir other) for each reflink directory (truenames): OTHER is
the next one in FS_BACKEND_LINUX_REFLINK_DIRS (cyclically), or NIL when there
is only one. Skips or fails per the rules above."
  (let ((strings (reflink-dir-strings)))
    (cond ((not (linux-p)) (skip +ficlone-linux-note+))
          ((null strings)
           (if (require-reflink-p)
               (fail "FS_BACKEND_LINUX_REQUIRE_REFLINK=1 but FS_BACKEND_LINUX_REFLINK_DIRS is unset")
               (skip +reflink-skip-note+)))
          (t
           (let ((problems (remove nil (mapcar #'%reflink-volume-problem strings))))
             (if problems
                 (if (require-reflink-p)
                     (fail (format nil "reflink volumes not usable: ~{~A~^; ~}" problems))
                     (skip (format nil "~A (~{~A~^; ~})" +reflink-skip-note+ problems)))
                 (let ((dirs (mapcar (lambda (s) (truename (dir-pathname s))) strings)))
                   (loop for (dir . rest) on dirs
                         for other = (if (rest dirs) (or (first rest) (first dirs)) nil)
                         do (testing (format nil "~A (~A)" (uiop:native-namestring dir)
                                             (volume-type dir))
                              (funcall fn dir other))))))))))

(defmacro defreflink (name (dir &optional (other (gensym "OTHER"))) &body body)
  "A DEFTEST running BODY once per reflink directory, DIR and OTHER bound
as in CALL-WITH-REFLINK-DIRS."
  `(deftest ,name
     (call-with-reflink-dirs
      (lambda (,dir ,other)
        (declare (ignorable ,dir ,other))
        ,@body))))

(defun call-with-volume-scratch (parent fn)
  "Call FN with a fresh empty directory PATH under PARENT (a directory
pathname, by truename); *FILESYSTEM* is a local filesystem; deleted afterwards."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "fbl-clone-~36R" (random (expt 36 8) (make-random-state t)))
                               parent))))
    (ensure-directories-exist dir)
    (unwind-protect
         (pathlib:with-filesystem ((local-fs))
           (funcall fn (pathlib:ensure-path (truename dir) :directory t)))
      (ignore-errors
       (uiop:run-program (list "chmod" "-R" "u+rwx" (uiop:native-namestring dir))
                         :ignore-error-status t))
      (ignore-errors (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

(defmacro with-volume-scratch ((var parent) &body body)
  `(call-with-volume-scratch ,parent (lambda (,var) ,@body)))

(defun pseudo-random-octets (n &optional (seed 1))
  "N octets of a fixed pseudo-random sequence (xorshift), reproducible."
  (let ((v (make-array n :element-type '(unsigned-byte 8)))
        (x (logior 1 (ldb (byte 32 0) (* seed 2654435761)))))
    (dotimes (i n v)
      (setf x (ldb (byte 32 0) (logxor x (ash x 13)))
            x (logxor x (ash x -17))
            x (ldb (byte 32 0) (logxor x (ash x 5))))
      (setf (aref v i) (ldb (byte 8 0) x)))))

(defun write-octets (pathname octets)
  (ensure-directories-exist pathname)
  (with-open-file (s pathname :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :supersede :if-does-not-exist :create)
    (write-sequence octets s))
  pathname)

(defun read-octets (pathname)
  (with-open-file (s pathname :element-type '(unsigned-byte 8))
    (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence v s)
      v)))

(defun overwrite-in-place (pathname position octets)
  "Write OCTETS at POSITION inside PATHNAME, in place (no truncation)."
  (with-open-file (s pathname :direction :output :element-type '(unsigned-byte 8)
                              :if-exists :overwrite)
    (file-position s position)
    (write-sequence octets s))
  pathname)

(defun file-hash (pathname)
  (pathlib:fs-content-hash (local-fs) pathname))

(defun file-mode (pathname)
  "Permission bits of PATHNAME (stat -c %a)."
  (parse-integer (uiop:run-program (list "stat" "-c" "%a" (uiop:native-namestring pathname))
                                   :output :string)
                 :radix 8 :junk-allowed t))

(defun chmod (pathname mode)
  (pathlib:fs-chmod (local-fs) pathname mode))

(defun entry-names (directory)
  "Names of the entries of DIRECTORY (a pathname), dotfiles included, sorted."
  (sort (remove "" (uiop:run-program (list "ls" "-A" (uiop:native-namestring directory))
                                     :output :lines)
                :test #'string=)
        #'string<))

(defun probe-leftovers (directory)
  "Entries of DIRECTORY named like the probe's temp files."
  (remove-if-not (lambda (n) (eql 0 (search ".cl-stack-ficlone-probe-" n)))
                 (entry-names directory)))

(defun tree-state (directory)
  "Every entry below DIRECTORY with its kind, size, mode and mtime, by find(1):
equal before and after means nothing below DIRECTORY was written, created,
removed, chmodded or touched (the directory itself included)."
  (uiop:run-program (list "find" (uiop:native-namestring directory)
                          "-printf" "%P %y %s %m %T@\\n")
                    :output :lines))

(defun running-as-root-p ()
  "True when the tests run with euid 0 (the ci-base container): permission
bits do not stop root, so EACCES cannot be provoked."
  (equal "0" (string-trim '(#\Space #\Newline)
                          (uiop:run-program '("id" "-u") :output :string))))

(defparameter +root-skip-note+
  "running as root: permission bits cannot provoke EACCES (the `reflink` job runs this as the runner user)")
