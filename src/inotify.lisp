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
  (hash-table-count (%inotify-watches journal)))

(defun inotify-journal-watched-directories (journal)
  "The relative POSIX paths of the directories JOURNAL watches now, sorted
STRING< (\"\" is the root), with no trailing \"/\". NIL once closed. Ignored
directories and symlinks to directories are never watched."
  (check-type journal inotify-journal)
  (sort (loop for rel being the hash-keys of (%inotify-directories journal) collect rel)
        #'string<))

(defun inject-inotify-event (journal mask &key (wd :root) (cookie 0) name)
  "For tests: queue a synthetic inotify event on the open JOURNAL. The next
drain handles it before the events it reads from the kernel, exactly as if
the kernel had queued it. MASK is the event's mask (e.g. +IN-Q-OVERFLOW+,
which the kernel sends with wd -1); WD is a watch descriptor or :ROOT (the
root's watch); NAME a string or NIL. Returns JOURNAL."
  (check-type journal inotify-journal)
  (check-type mask (unsigned-byte 32))
  (check-type cookie (unsigned-byte 32))
  (check-type name (or null string))
  (check-type wd (or (eql :root) (signed-byte 32)))
  (unless (pathlib:journal-open-p journal)
    (error 'pathlib:journal-error :journal journal :message "inject into a closed journal"))
  (setf (%inotify-injected journal)
        (append (%inotify-injected journal) (list (list wd mask cookie name))))
  journal)

;;; --- paths ------------------------------------------------------------------------

(defconstant +watch-mask+
  (logior +in-create+ +in-delete+ +in-modify+ +in-attrib+ +in-close-write+
          +in-moved-from+ +in-moved-to+ +in-delete-self+ +in-move-self+
          +in-onlydir+ +in-dont-follow+ +in-excl-unlink+)
  "The mask of every watch. No IN_OPEN / IN_ACCESS / IN_CLOSE_NOWRITE: our
own listings (opendir) would otherwise queue events named after the listed
directory on its parent's watch.")

(defconstant +read-buffer-size+ 65536
  "Bytes per read(2): far above one event (16 + NAME_MAX + 1), so read never
fails with EINVAL.")

(defun %child (dir name)
  "The relative path of NAME in the relative directory DIR (\"\" is the root)."
  (if (string= dir "") name (concatenate 'string dir "/" name)))

(defun %at-or-below-p (rel prefix)
  "True when the relative path REL is PREFIX or below it."
  (or (string= prefix "")
      (string= rel prefix)
      (and (> (length rel) (length prefix))
           (string= prefix rel :end2 (length prefix))
           (char= #\/ (char rel (length prefix))))))

(defun %native (journal rel)
  "Native absolute namestring of the directory REL under JOURNAL's root. The
root keeps its trailing \"/\"; a subdirectory never has one, since a trailing
slash makes path resolution follow a final symlink even with IN_DONT_FOLLOW."
  (let* ((s (uiop:native-namestring (pathlib:path-pathname (pathlib:journal-root journal))))
         (root (if (and (plusp (length s)) (char= #\/ (char s (1- (length s)))))
                   s
                   (concatenate 'string s "/"))))
    (if (string= rel "") root (concatenate 'string root rel))))

(defun %dir-path (journal rel)
  "The PATH of the directory REL under JOURNAL's root, for PATHLIB:ITERDIR."
  (if (string= rel "")
      (pathlib:journal-root journal)
      (pathlib:ensure-directory (pathlib:join (pathlib:journal-root journal) rel))))

(defun %ignored-dir-p (journal rel)
  "True when the ignore matcher of JOURNAL drops the directory REL."
  (let ((matcher (pathlib:journal-ignore journal)))
    (and matcher (plusp (length rel))
         (pathlib:ignore-path-p matcher rel :directory t))))

;;; --- the watch tables ---------------------------------------------------------------
;;;
;;; Between drains WATCHES (wd → rel) and DIRECTORIES (rel → wd) are inverse
;;; maps whose wds are exactly the kernel's watches on the fd. Every wd we
;;; drop is inotify_rm_watch'ed (result ignored: EINVAL when the kernel
;;; already dropped it) or was dropped by the kernel (IN_IGNORED).

(defun %root-wd (journal)
  (gethash "" (%inotify-directories journal)))

(defun %forget-wd (journal wd)
  "Drop WD from both tables (no system call)."
  (let* ((ws (%inotify-watches journal))
         (ds (%inotify-directories journal))
         (rel (gethash wd ws)))
    (when (and rel (eql (gethash rel ds) wd))
      (remhash rel ds))
    (remhash wd ws)))

(defun %drop-wd (journal wd)
  "inotify_rm_watch WD (result ignored) and forget it."
  (%inotify-rm-watch (%inotify-fd journal) wd)
  (%forget-wd journal wd))

(defun %remove-prefix (journal prefix)
  "Remove every watch at or below the relative directory PREFIX."
  (let ((victims (loop for rel being the hash-keys of (%inotify-directories journal)
                         using (hash-value wd)
                       when (%at-or-below-p rel prefix) collect wd)))
    (dolist (wd victims)
      (%drop-wd journal wd))))

(defun %bind (journal wd rel)
  "WD watches REL now. WD's old path goes; another wd that claimed REL (a
directory replaced by mv, or a deleted inode kept alive elsewhere) is removed."
  (let* ((ws (%inotify-watches journal))
         (ds (%inotify-directories journal))
         (old (gethash wd ws))
         (other (gethash rel ds)))
    (when (and old (eql (gethash old ds) wd))
      (remhash old ds))
    (when (and other (/= other wd))
      (%drop-wd journal other))
    (setf (gethash wd ws) rel
          (gethash rel ds) wd)))

(defun %add-watch (journal rel)
  "Watch the directory REL. → (values wd new-p), or NIL when REL vanished
(ENOENT / ENOTDIR: its parent reports it). ENOSPC, EACCES, any other errno,
and a new watch past *INOTIFY-MAX-WATCHES* (as ENOSPC) signal INOTIFY-ERROR."
  (let* ((fd (%inotify-fd journal))
         (wd (%inotify-add-watch fd (%native journal rel) +watch-mask+)))
    (if (< wd 0)
        (let ((errno (%errno)))
          (if (or (= errno +enoent+) (= errno +enotdir+))
              nil
              (error 'inotify-error :journal journal
                                    :operation "inotify_add_watch" :errno errno)))
        (let* ((ws (%inotify-watches journal))
               (new-p (not (nth-value 1 (gethash wd ws))))
               (max (%inotify-max-watches journal)))
          (when (and new-p max (>= (hash-table-count ws) max))
            (%inotify-rm-watch fd wd)   ; never counted: the kernel agrees
            (error 'inotify-error :journal journal
                                  :operation "inotify_add_watch" :errno +enospc+))
          (%bind journal wd rel)
          (values wd new-p)))))

(defun %subdirectories (journal rel)
  "The real (non-symlink), non-ignored subdirectories of REL, as relative
paths. An entry whose name is not a string signals INOTIFY-ERROR (it cannot
be watched by name)."
  (loop for entry in (pathlib:iterdir (%dir-path journal rel))
        when (and (not (pathlib:symlink-p entry))
                  (pathlib:directory-p entry))
          append (let ((name (pathlib:name entry)))
                   (unless (stringp name)
                     (error 'inotify-error :journal journal :operation "readdir" :errno nil))
                   (let ((child (%child rel name)))
                     (unless (%ignored-dir-p journal child)
                       (list child))))))

(defun %watch-tree (journal rel &optional seen)
  "Watch REL first, then list it and recurse into every real, non-ignored
subdirectory (the add-watch race rule). SEEN, a hash table or NIL, collects
every wd add_watch returned. A failed watch or listing is handled here, at
the step that can skip the directory. → (values complete-p fresh), FRESH
the relative paths that got a new wd."
  (let ((ok t) (fresh '()))
    (labels ((walk (rel)
               (handler-case
                   (multiple-value-bind (wd new-p) (%add-watch journal rel)
                     (cond ((null wd)                       ; vanished
                            (when (string= rel "")
                              (setf (%inotify-root-lost journal) t)))
                           (t
                            (when seen (setf (gethash wd seen) t))
                            (when new-p (push rel fresh))
                            (mapc #'walk (%subdirectories journal rel)))))
                 (pathlib:path-not-found () nil)            ; vanished while listed
                 ((or inotify-error pathlib:path-error file-error) ()
                   (setf ok nil)))))
      (walk rel))
    (values ok fresh)))

(defun %resync (journal)
  "Walk the whole tree: watch every directory that lacks a watch (add_watch
of a watched inode returns its wd, which renames it), then remove every wd
the walk did not see (vanished or moved out). A root that vanished or got
another wd is the root lost. → (values complete-p fresh)."
  (let ((seen (make-hash-table))
        (root-wd (%root-wd journal)))
    (multiple-value-bind (ok fresh) (%watch-tree journal "" seen)
      (let ((stale (loop for wd being the hash-keys of (%inotify-watches journal)
                         unless (gethash wd seen) collect wd)))
        (dolist (wd stale)
          (%drop-wd journal wd)))
      (when (and root-wd (not (eql root-wd (%root-wd journal))))
        (setf (%inotify-root-lost journal) t))
      (setf (%inotify-incomplete journal) (not ok))
      (values ok fresh))))

;;; --- the constructor --------------------------------------------------------------

(defun %make-inotify-journal (fs directory matcher)
  "The registry constructor (PATHLIB:REGISTER-WATCH-MECHANISM): an open
INOTIFY-JOURNAL over the absolute directory pathname DIRECTORY on FS with
the ignore MATCHER (or NIL), every directory under it watched before it
returns. inotify_init1 failing → INOTIFY-ERROR (nothing left open). A watch
that cannot be added does not signal: the first drain answers NIL."
  (let ((max-watches *inotify-max-watches*))
    (check-type max-watches (or null (integer 1)))
    (let ((fd (%inotify-init1 (logior +in-nonblock+ +in-cloexec+))))
      (when (< fd 0)
        (error 'inotify-error :operation "inotify_init1" :errno (%errno)))
      (let ((done nil))
        (unwind-protect
             (let ((journal (make-instance 'inotify-journal
                                           :filesystem fs :root directory :ignore matcher
                                           :fd fd :max-watches max-watches)))
               (unless (%watch-tree journal "")
                 (setf (%inotify-incomplete journal) t))
               (setf done t)
               journal)
          (unless done
            (%close fd)))))))

;;; --- reading the queue --------------------------------------------------------------

(defun %event-name (buf start len)
  "The NUL-padded name at BUF[START, START+LEN) decoded as strict UTF-8; NIL
when empty, :INVALID when it is not UTF-8."
  (let ((end (loop for i from start below (+ start len)
                   when (zerop (cffi:mem-ref buf :uint8 i)) return i
                   finally (return (+ start len)))))
    (when (> end start)
      (let ((octets (make-array (- end start) :element-type '(unsigned-byte 8))))
        (loop for i from start below end
              for k from 0
              do (setf (aref octets k) (cffi:mem-ref buf :uint8 i)))
        (handler-case (pathlib:utf8-decode-strict octets)
          (pathlib:utf8-decoding-error () :invalid))))))

(defun %map-kernel-events (journal fn)
  "Call FN with (wd mask cookie name) for every event in the kernel queue,
reading until EAGAIN. A read error other than EAGAIN / EINTR signals
INOTIFY-ERROR."
  (let ((fd (%inotify-fd journal)))
    (cffi:with-foreign-pointer (buf +read-buffer-size+)
      (loop
        (let ((n (%read fd buf +read-buffer-size+)))
          (cond ((plusp n)
                 (loop with off = 0
                       while (<= (+ off +inotify-event-header-size+) n)
                       do (let* ((wd (cffi:mem-ref buf :int off))
                                 (mask (cffi:mem-ref buf :uint32 (+ off 4)))
                                 (cookie (cffi:mem-ref buf :uint32 (+ off 8)))
                                 (len (cffi:mem-ref buf :uint32 (+ off 12)))
                                 (start (+ off +inotify-event-header-size+)))
                            (funcall fn wd mask cookie (%event-name buf start len))
                            (setf off (+ start len)))))
                ((zerop n) (return))
                (t (let ((errno (%errno)))
                     (cond ((= errno +eintr+))
                           ((= errno +eagain+) (return))
                           (t (error 'inotify-error :journal journal
                                                    :operation "read" :errno errno)))))))))))

;;; --- the drain ----------------------------------------------------------------------

(defmethod pathlib:journal-drain ((journal inotify-journal) &key settle)
  ;; Never sleeps: the kernel queues each event before the call that caused
  ;; it returns, so SETTLE has nothing to wait for.
  (declare (ignore settle))
  (let ((injected (shiftf (%inotify-injected journal) '())))
    (when (%inotify-root-lost journal)
      (return-from pathlib:journal-drain (values nil nil)))
    (let ((raw '())
          (lost nil)
          ;; cookie → detached (wd . suffix) entries of a moved directory
          (moves (make-hash-table)))
      (labels
          ((emit (rel dirp)
             (push (if dirp (concatenate 'string rel "/") rel) raw))
           (grow (rel)
             ;; A directory that appeared: watch first, list second.
             (unless (or (%ignored-dir-p journal rel)
                         (%watch-tree journal rel))
               (setf lost t)))
           (detach (rel)
             (let ((entries '()))
               (maphash (lambda (r wd)
                          (when (%at-or-below-p r rel)
                            (push (cons wd (subseq r (length rel))) entries)))
                        (%inotify-directories journal))
               (dolist (e entries)
                 (%forget-wd journal (car e)))
               entries))
           (reattach (rel entries)
             (cond ((%ignored-dir-p journal rel)
                    ;; moved to an ignored name: ignored directories have no watch
                    (dolist (e entries)
                      (%inotify-rm-watch (%inotify-fd journal) (car e))))
                   (t
                    (%remove-prefix journal rel) ; the victim of mv over a directory
                    (dolist (e entries)
                      (%bind journal (car e) (concatenate 'string rel (cdr e))))
                    ;; Nothing came along (it had no watch), or ignore rules
                    ;; may differ at the new place: walk it again. add_watch
                    ;; returns the same wds, so the count does not change.
                    (when (or (null entries) (pathlib:journal-ignore journal))
                      (grow rel)))))
           (named (dir mask cookie name)
             (if (eq name :invalid)
                 (setf lost t)
                 (let ((rel (%child dir name))
                       (dirp (logtest mask +in-isdir+)))
                   (emit rel dirp)
                   (when dirp
                     (cond ((logtest mask +in-moved-from+)
                            (setf (gethash cookie moves) (detach rel)))
                           ((logtest mask +in-moved-to+)
                            (multiple-value-bind (entries pairedp) (gethash cookie moves)
                              (cond (pairedp
                                     (remhash cookie moves)
                                     (reattach rel entries))
                                    (t (grow rel))))) ; move in = create
                           ((logtest mask +in-create+)
                            (grow rel)))))))
           (event (wd mask cookie name)
             (let ((root-wd (%root-wd journal)))
               (cond ((or lost (%inotify-root-lost journal))) ; the rest is thrown away
                     ((logtest mask +in-q-overflow+)
                      (setf lost t))
                     ((and root-wd (= wd root-wd))
                      (if (logtest mask (logior +in-delete-self+ +in-move-self+
                                                +in-unmount+ +in-ignored+))
                          (setf (%inotify-root-lost journal) t)
                          (when name (named "" mask cookie name))))
                     (t
                      (let ((dir (gethash wd (%inotify-watches journal))))
                        (cond ((null dir))      ; detached, removed by us, or stale
                              ((logtest mask +in-ignored+) (%forget-wd journal wd))
                              ((logtest mask +in-unmount+) (setf lost t))
                              (name (named dir mask cookie name)))))))))
        ;; Injected events first, through the same path (:ROOT resolved now).
        (loop for (wd mask cookie name) in injected
              do (event (if (eq wd :root) (or (%root-wd journal) -1) wd) mask cookie name))
        (unless (%inotify-root-lost journal)
          (handler-case (%map-kernel-events journal #'event)
            (inotify-error () (setf lost t))))
        ;; The queue is empty: every FROM still pending is a move out of the
        ;; root. A wd a later create or move in reclaimed is kept.
        (maphash (lambda (cookie entries)
                   (declare (ignore cookie))
                   (dolist (e entries)
                     (unless (nth-value 1 (gethash (car e) (%inotify-watches journal)))
                       (%inotify-rm-watch (%inotify-fd journal) (car e)))))
                 moves)
        (cond ((%inotify-root-lost journal) (values nil nil))
              (lost
               (%resync journal)
               (values nil nil))
              ((%inotify-incomplete journal)
               ;; An older gap: retry. Directories that got their first watch
               ;; now may hold writes no event saw: report them by their path.
               (multiple-value-bind (ok fresh) (%resync journal)
                 (if (and ok (not (%inotify-root-lost journal)))
                     (values (nconc (mapcar (lambda (r) (concatenate 'string r "/")) fresh)
                                    raw)
                             t)
                     (values nil nil))))
              (t (values raw t)))))))

;;; --- close --------------------------------------------------------------------------

(defmethod pathlib:journal-close ((journal inotify-journal))
  ;; Runs once (the :AROUND on JOURNAL). Removes the watches, closes the fd,
  ;; empties the tables; never signals for a root that is already gone
  ;; (rm_watch and close answer -1, which is ignored).
  (let ((fd (%inotify-fd journal)))
    (unwind-protect
         (when fd
           (unwind-protect
                (loop for wd being the hash-keys of (%inotify-watches journal)
                      do (%inotify-rm-watch fd wd))
             (%close fd)))
      (clrhash (%inotify-watches journal))
      (clrhash (%inotify-directories journal))
      (setf (%inotify-injected journal) '()
            (%inotify-fd journal) nil))))
