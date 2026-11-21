(in-package #:fs-backend-linux)

;;; R4b-1: the :INOTIFY journal (brief: cl-stack/docs/capabilities/fs-backend-linux.md).
;;;
;;; One inotify instance per journal (non-blocking, close-on-exec), one watch
;;; per directory under the root: the root and every real (non-symlink),
;;; non-ignored directory below it. No thread: events wait in the kernel queue
;;; until JOURNAL-DRAIN reads them all (non-blocking READ until EAGAIN). The
;;; kernel queues an event before the system call that caused it returns, so
;;; the settle rule holds without waiting.
;;;
;;; Raw paths go to the :AROUND method of PATHLIB:JOURNAL-DRAIN, which strips,
;;; filters ignored paths, dedups and sorts. Summary of the locked rules
;;; (details in the brief):
;;;   - an event named N on the watch of directory D reports D/N;
;;;     name-less events (on a watched directory itself) report nothing;
;;;   - a directory that appears (IN_CREATE or an unpaired IN_MOVED_TO with
;;;     IN_ISDIR) is reported by its own path, which covers its subtree, and
;;;     is watched: watch first, then list, recursively (the add-watch race);
;;;   - IN_MOVED_FROM + IN_MOVED_TO with the same cookie inside the root:
;;;     both paths, and the watches below the old path move to the new one;
;;;     an IN_MOVED_FROM left unpaired when the queue is empty is a move out
;;;     of the root: its path, and every watch at or below it is removed;
;;;   - IN_Q_OVERFLOW, a watch that could not be added (ENOSPC, EACCES,
;;;     *INOTIFY-MAX-WATCHES*), or a name that is not UTF-8: that drain
;;;     answers (NIL NIL) and resyncs the watches; later drains answer NIL
;;;     until a resync has watched every directory again;
;;;   - the root deleted, moved or unmounted (IN_DELETE_SELF, IN_MOVE_SELF,
;;;     IN_UNMOUNT, IN_IGNORED on the root watch): every later drain answers
;;;     (NIL NIL), until JOURNAL-CLOSE.

(defconstant +inotify-rank+ 20
  "Rank of :INOTIFY in PATHLIB:*WATCH-MECHANISMS* (above 0, the default of
other registrations; :POLL is always last).")

(defvar *inotify-max-watches* nil
  "NIL, or a positive integer: the most watches one journal holds. Read by
FS-WATCH when the journal is made. A watch beyond it fails as if
inotify_add_watch returned ENOSPC (the kernel's fs.inotify.max_user_watches),
with the same outcome: drains answer COMPLETE-P NIL until it fits again.")

(defun %r4b1-pending (what)
  (error "fs-backend-linux R4b-1: ~A is not implemented yet" what))

;;; --- the journal --------------------------------------------------------------------

(defclass inotify-journal (pathlib:journal)
  ((fd :initarg :fd :initform nil :accessor %inotify-fd
       :documentation "The inotify file descriptor; NIL once closed.")
   (max-watches :initarg :max-watches :initform nil :reader %inotify-max-watches
                :documentation "*INOTIFY-MAX-WATCHES* when the journal was made.")
   (watches :initform (make-hash-table) :reader %inotify-watches
            :documentation "wd → relative directory path (\"\" is the root).")
   (directories :initform (make-hash-table :test #'equal) :reader %inotify-directories
                :documentation "relative directory path → wd.")
   (injected :initform '() :accessor %inotify-injected
             :documentation "Synthetic events (wd mask cookie name), oldest first.")
   (incomplete :initform nil :accessor %inotify-incomplete
               :documentation "T while some directory under the root lacks a watch.")
   (root-lost :initform nil :accessor %inotify-root-lost
              :documentation "T once the root was deleted, moved or unmounted."))
  (:default-initargs :mechanism :inotify)
  (:documentation "The :INOTIFY journal: one inotify instance, one watch per
watched directory. Made by FS-WATCH on a LOCAL-FILESYSTEM on Linux."))

(defun inotify-journal-p (x) (typep x 'inotify-journal))

(defun inotify-journal-fd (journal)
  "The inotify file descriptor of JOURNAL (an integer), NIL once closed."
  (check-type journal inotify-journal)
  (%inotify-fd journal))

(defun inotify-journal-watch-count (journal)
  "Number of inotify watches JOURNAL holds now (0 once closed)."
  (check-type journal inotify-journal)
  (%r4b1-pending 'inotify-journal-watch-count))

(defun inotify-journal-watched-directories (journal)
  "The relative POSIX paths of the directories JOURNAL watches now, sorted
STRING< (\"\" is the root), with no trailing \"/\". NIL once closed. Ignored
directories and symlinks to directories are never watched."
  (check-type journal inotify-journal)
  (%r4b1-pending 'inotify-journal-watched-directories))

(defun inject-inotify-event (journal mask &key (wd :root) (cookie 0) name)
  "For tests: queue a synthetic inotify event on the open JOURNAL. The next
drain handles it before the events it reads from the kernel, exactly as if
the kernel had queued it. MASK is the event's mask (e.g. +IN-Q-OVERFLOW+,
which the kernel sends with wd -1); WD is a watch descriptor or :ROOT (the
root's watch); NAME a string or NIL. Returns JOURNAL."
  (declare (ignorable wd))
  (check-type journal inotify-journal)
  (check-type mask (unsigned-byte 32))
  (check-type cookie (unsigned-byte 32))
  (check-type name (or null string))
  (unless (pathlib:journal-open-p journal)
    (error 'pathlib:journal-error :journal journal :message "inject into a closed journal"))
  (%r4b1-pending 'inject-inotify-event))

(defun %make-inotify-journal (fs directory matcher)
  "The registry constructor (PATHLIB:REGISTER-WATCH-MECHANISM): an open
INOTIFY-JOURNAL over the absolute directory pathname DIRECTORY on FS with
the ignore MATCHER (or NIL), every directory under it watched before it
returns. inotify_init1 failing → INOTIFY-ERROR (nothing left open). A watch
that cannot be added does not signal: the first drain answers NIL."
  (declare (ignore fs directory matcher))
  (%r4b1-pending '%make-inotify-journal))

(defmethod pathlib:journal-drain ((journal inotify-journal) &key settle)
  (declare (ignore settle))
  (%r4b1-pending 'journal-drain))

(defmethod pathlib:journal-close ((journal inotify-journal))
  ;; Runs once (the :AROUND on JOURNAL). Removes the watches, closes the fd,
  ;; empties the tables; never signals for a root that is already gone.
  (%r4b1-pending 'journal-close))
