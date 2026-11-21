(in-package #:fs-backend-linux)

;;; R4c-2: the :FICLONE clone mechanism (brief:
;;; cl-stack/docs/capabilities/fs-backend-linux.md § R4c-2; protocol:
;;; cl-stack/docs/capabilities/pathlib.md § R4c-0).
;;;
;;; pathlib walks the tree, makes directories and symlinks, sets modes and
;;; mtimes, and calls FICLONE-FILE for each regular file. FICLONE-FILE is the
;;; per-file primitive: open SOURCE read-only (O_NOFOLLOW), create DESTINATION
;;; (O_EXCL, #o600), ioctl(dst, FICLONE, src), close both. No tree hook.
;;;
;;; Locked errno mapping (FICLONE-FILE and the probe):
;;;   EOPNOTSUPP ENOTTY ENOSYS          → (NIL :UNSUPPORTED-VOLUME)
;;;   EXDEV                             → (NIL :CROSS-VOLUME)
;;;   EINVAL EPERM ETXTBSY EBUSY        → (NIL :CLONE-FAILED)
;;;       (btrfs NOCOW vs COW, immutable / append-only files, swap files)
;;;   anything else, and a failing open → FICLONE-ERROR (errno in CODE)
;;; Before answering NIL or signalling, a DESTINATION it created is unlinked.
;;;
;;; Detection is a probe clone on the target volume (no cache): it borrows an
;;; existing regular file of SOURCE-DIR, opened read-only (the source tree is
;;; never written), clones it into DESTINATION-DIR/.cl-stack-ficlone-probe-<hex>
;;; and unlinks that. With no file to borrow it clones a 4096-octet temp file
;;; to a second temp file, both in DESTINATION-DIR.

(defconstant +ficlone-rank+ 100
  "Rank of :FICLONE in PATHLIB:*CLONE-MECHANISMS* (:COPY is always last).")

(defparameter +probe-prefix+ ".cl-stack-ficlone-probe-"
  "Name prefix of the probe's temp files in DESTINATION-DIR (always unlinked).")

(defparameter +probe-max-entries+ 1000
  "The probe lists at most this many entries of SOURCE-DIR looking for a file.")

(defparameter +probe-max-depth+ 2
  "… and descends at most this many levels below SOURCE-DIR.")


(defparameter +probe-max-candidates+ 8
  "The probe tries at most this many borrowed files: one that cannot be opened,
or that refuses a clone by itself (:CLONE-FAILED: btrfs NOCOW, immutable), is
followed by the next one.")

(defconstant +o-nonblock+ #o4000
  "O_NONBLOCK (asm-generic), added to the probe's open of a borrowed file only,
so that a file that turned into a FIFO since it was listed never blocks it.")

(defvar *probe-random-state* (make-random-state t)
  "Random state for the hex part of the probe's temp file names.")

;;; --- errno → reason --------------------------------------------------------------

(defun %ficlone-errno-reason (errno)
  "The locked mapping of an errno from ioctl FICLONE: :UNSUPPORTED-VOLUME
(EOPNOTSUPP, ENOTTY, ENOSYS), :CROSS-VOLUME (EXDEV), :CLONE-FAILED (EINVAL,
EPERM, ETXTBSY, EBUSY), or NIL: not a fallback, FICLONE-FILE signals
FICLONE-ERROR."
  (cond ((member errno (list +eopnotsupp+ +enotty+ +enosys+)) :unsupported-volume)
        ((eql errno +exdev+) :cross-volume)
        ((member errno (list +einval+ +eperm+ +etxtbsy+ +ebusy+)) :clone-failed)
        (t nil)))

;;; --- file descriptors ------------------------------------------------------------
;;; errno is read as the first thing after the failing call: close(2) and
;;; unlink(2) would overwrite it.

(defun %clone-namestring (designator)
  "The native namestring of DESIGNATOR (a pathname, or already a namestring)."
  (if (stringp designator) designator (uiop:native-namestring designator)))

(defun %ficlone-error (operation errno path)
  (error 'ficlone-error :operation operation :code errno :path path
                        :message (format nil "~A failed (errno ~A)" operation errno)))

(defun %open-or-signal (namestring flags mode path)
  "open(2) NAMESTRING → the fd, retrying on EINTR. Signals FICLONE-ERROR
(operation \"open\", PATH) with the errno otherwise."
  (loop (let ((fd (%open namestring flags mode)))
          (when (>= fd 0)
            (return fd))
          (let ((errno (%errno)))
            (unless (eql errno +eintr+)
              (%ficlone-error "open" errno path))))))

(defun %ioctl-ficlone (dst-fd src-fd)
  "ioctl(DST-FD, FICLONE, SRC-FD), retrying on EINTR → NIL on success, else the errno."
  (loop (when (zerop (%ioctl-int dst-fd +ficlone+ src-fd))
          (return nil))
        (let ((errno (%errno)))
          (unless (eql errno +eintr+)
            (return errno)))))

(defun %ficlone-names (source destination source-path destination-path
                       &key (extra-source-flags 0))
  "The one owner of the fds: reflink the file SOURCE to the new file
DESTINATION (native namestrings). → T, or (values NIL reason) per
%FICLONE-ERRNO-REASON, DESTINATION unlinked. Signals FICLONE-ERROR (PATH
SOURCE-PATH or DESTINATION-PATH) for a failing open and for an unmapped ioctl
errno, the latter only once DESTINATION is unlinked. SOURCE is opened first,
so a failing source open creates nothing; a failing create (EEXIST) leaves an
existing DESTINATION alone. Both fds are always closed."
  (let ((src (%open-or-signal source
                              (logior +o-rdonly+ +o-cloexec+ +o-nofollow+ extra-source-flags)
                              0 source-path)))
    (unwind-protect
         (let ((dst (%open-or-signal destination
                                     (logior +o-wronly+ +o-creat+ +o-excl+ +o-cloexec+)
                                     #o600 destination-path))
               (keep nil)
               (unlinked nil))
           (unwind-protect
                (let ((errno (%ioctl-ficlone dst src)))
                  (if (null errno)
                      (progn (setf keep t) t)
                      (progn
                        (%close dst)
                        (setf dst nil)
                        (%unlink destination)
                        (setf unlinked t)
                        (let ((reason (%ficlone-errno-reason errno)))
                          (if reason
                              (values nil reason)
                              (%ficlone-error "ioctl FICLONE" errno destination-path))))))
             ;; A non-local exit between the create and the answer.
             (when dst (%close dst))
             (unless (or keep unlinked) (%unlink destination))))
      (%close src))))

;;; --- the probe -------------------------------------------------------------------

(defun %probe-temp-name (dir)
  "DIR (a native directory namestring ending in /) + +PROBE-PREFIX+ + 16 hex digits."
  (format nil "~A~A~(~16,'0X~)" dir +probe-prefix+
          (random (expt 2 64) *probe-random-state*)))

(defun %probe-candidates (source-dir)
  "Up to +PROBE-MAX-CANDIDATES+ regular files below SOURCE-DIR to borrow,
non-empty ones first, in the order seen. Breadth first, at most
+PROBE-MAX-ENTRIES+ entries listed and +PROBE-MAX-DEPTH+ levels below
SOURCE-DIR. Reads only (readdir, lstat). Symlinks (to directories too) and
special files are skipped; an unreadable directory is skipped."
  (let ((fs (pathlib:make-local-filesystem))
        (queue (list (cons (uiop:ensure-directory-pathname source-dir) 0)))
        (budget +probe-max-entries+)
        (full '())
        (empty '()))
    (loop while (and queue (plusp budget) (< (length full) +probe-max-candidates+))
          do (destructuring-bind (dir . depth) (pop queue)
               (dolist (entry (handler-case (pathlib:fs-iterdir fs dir)
                                ((or pathlib:path-error file-error) () '())))
                 (when (or (<= budget 0) (>= (length full) +probe-max-candidates+))
                   (return))
                 (decf budget)
                 (if (uiop:directory-pathname-p entry)
                     (when (and (< depth +probe-max-depth+)
                                (not (pathlib:fs-symlink-p fs entry)))
                       (setf queue (nconc queue (list (cons entry (1+ depth))))))
                     (let ((stat (handler-case (pathlib:fs-stat fs entry :follow-symlinks nil)
                                   ((or pathlib:path-error file-error) () nil))))
                       (when (and stat (eq :file (pathlib:file-stat-kind stat)))
                         (if (plusp (or (pathlib:file-stat-size stat) 0))
                             (push entry full)
                             (push entry empty))))))))
    (let ((all (append (nreverse full) (nreverse empty))))
      (subseq all 0 (min (length all) +probe-max-candidates+)))))

(defun %probe-clone-into (source dir)
  "Reflink the file SOURCE (a native namestring) to a fresh temp file in DIR
and unlink that. → T, a fallback reason, :SOURCE (SOURCE cannot be opened) or
:UNAVAILABLE (no temp file can be created, or an unmapped ioctl errno).
Never signals for an errno."
  (loop repeat 8
        do (let ((tmp (%probe-temp-name dir)))
             (handler-case
                 (multiple-value-bind (ok reason)
                     (%ficlone-names source tmp source tmp :extra-source-flags +o-nonblock+)
                   (when ok
                     (%unlink tmp))
                   (return (if ok t reason)))
               (ficlone-error (c)
                 (let ((path (pathlib:path-error-path c)))
                   (cond ((eq path source) (return :source))
                         ((and (eq path tmp)
                               (equal "open" (pathlib:clone-error-operation c))
                               (eql +eexist+ (pathlib:clone-error-code c))))  ; another name
                         (t (return :unavailable)))))))
        finally (return :unavailable)))

(defun %probe-write-seed (dir)
  "A fresh 4096-octet temp file in DIR (a native directory namestring) → its
native namestring, or NIL when none can be created or written."
  (loop repeat 8
        do (let* ((name (%probe-temp-name dir))
                  (created nil)
                  (written
                    (handler-case
                        (with-open-file (s (uiop:parse-native-namestring name)
                                           :direction :output
                                           :element-type '(unsigned-byte 8)
                                           :if-exists nil :if-does-not-exist :create)
                          (when s
                            (setf created t)
                            (write-sequence (make-array 4096 :element-type '(unsigned-byte 8)
                                                             :initial-element 0)
                                            s)
                            (finish-output s)
                            t))
                      ((or file-error stream-error) ()
                        (when created (%unlink name))
                        (return nil)))))
             (when written
               (return name)))
        finally (return nil)))

(defun %probe-destination-only (dir)
  "No file to borrow: clone a 4096-octet temp file in DIR to a second one."
  (let ((seed (%probe-write-seed dir)))
    (if (null seed)
        (values nil :unavailable)
        (unwind-protect
             (let ((answer (%probe-clone-into seed dir)))
               (case answer
                 ((t) t)
                 (:source (values nil :unavailable))
                 (t (values nil answer))))
          (%unlink seed)))))

(defun %probe-clone (source-dir destination-dir)
  "The Linux branch of FICLONE-AVAILABLE-P: a real clone on the target
volume, as described above. → (values available-p reason); never signals for
an errno (a temp file that cannot be created → (NIL :UNAVAILABLE))."
  (let ((dir (%clone-namestring (uiop:ensure-directory-pathname destination-dir)))
        (clone-failed nil))
    (dolist (candidate (%probe-candidates source-dir))
      (let ((answer (%probe-clone-into (%clone-namestring candidate) dir)))
        (case answer
          ((t) (return-from %probe-clone t))
          (:clone-failed (setf clone-failed t)) ; this file refuses: the next one
          (:source)                             ; cannot be opened: the next one
          (t (return-from %probe-clone (values nil answer))))))
    (if clone-failed
        (values nil :clone-failed)
        (%probe-destination-only dir))))

(defun ficlone-available-p (fs source-dir destination-dir)
  "The registry probe of :FICLONE → (values available-p reason).
FS not a LOCAL-FILESYSTEM → (NIL :UNSUPPORTED-FILESYSTEM). Off Linux →
(NIL :UNAVAILABLE). On Linux, a probe clone from SOURCE-DIR (read only) into
DESTINATION-DIR (both absolute directory pathnames, DESTINATION-DIR existing):
T, or (NIL :CROSS-VOLUME), (NIL :UNSUPPORTED-VOLUME), (NIL :CLONE-FAILED),
(NIL :UNAVAILABLE). It leaves no file behind."
  (cond ((not (typep fs 'pathlib:local-filesystem))
         (values nil :unsupported-filesystem))
        ((not (%linux-p))
         (values nil :unavailable))
        (t (%probe-clone source-dir destination-dir))))

;;; --- the per-file primitive ------------------------------------------------------

(defun ficlone-file (fs source destination)
  "Reflink SOURCE (absolute pathname of a regular file) to DESTINATION
(absolute pathname, absent; its directory exists) with ioctl FICLONE.
→ T: DESTINATION (mode #o600; pathlib sets bits and mtime) shares SOURCE's
extents. → (values NIL reason) per the errno mapping, DESTINATION gone.
Signals FICLONE-ERROR (operation \"open\" or \"ioctl FICLONE\", code the errno)
otherwise, DESTINATION gone unless it existed before (EEXIST). Both fds are
always closed. FS is the LOCAL-FILESYSTEM (unused)."
  (declare (ignore fs))
  (%ficlone-names (%clone-namestring source) (%clone-namestring destination) source destination))

;;; --- the sharing check (tests) ---------------------------------------------------

(defparameter +fiemap-batch+ 32
  "Extents read per FS_IOC_FIEMAP call.")

(defun %fiemap-call (fd buffer path)
  "ioctl(FD, FS_IOC_FIEMAP, BUFFER), retrying on EINTR; signals FICLONE-ERROR."
  (loop (when (zerop (%ioctl-pointer fd +fs-ioc-fiemap+ buffer))
          (return))
        (let ((errno (%errno)))
          (unless (eql errno +eintr+)
            (%ficlone-error "ioctl FS_IOC_FIEMAP" errno path)))))

(defun %fiemap-all-shared-p (fd path)
  "T when FD's file has an extent and every extent up to FIEMAP_EXTENT_LAST
carries FIEMAP_EXTENT_SHARED."
  (let ((size (+ +fiemap-header-size+ (* +fiemap-batch+ +fiemap-extent-size+)))
        (start 0)
        (seen nil))
    (cffi:with-foreign-pointer (buf size)
      (loop
        (dotimes (i size) (setf (cffi:mem-aref buf :uint8 i) 0))
        (cffi:with-foreign-slots ((fm-start fm-length fm-flags fm-extent-count)
                                  buf (:struct fiemap))
          (setf fm-start start
                fm-length #xFFFFFFFFFFFFFFFF ; FIEMAP_MAX_OFFSET
                fm-flags +fiemap-flag-sync+
                fm-extent-count +fiemap-batch+))
        (%fiemap-call fd buf path)
        (let ((n (cffi:foreign-slot-value buf '(:struct fiemap) 'fm-mapped-extents))
              (next start))
          (when (zerop n)
            (return seen))              ; an empty file: NIL
          (dotimes (i n)
            (let ((e (cffi:inc-pointer buf (+ +fiemap-header-size+ (* i +fiemap-extent-size+)))))
              (cffi:with-foreign-slots ((fe-logical fe-length fe-flags) e (:struct fiemap-extent))
                (unless (logtest fe-flags +fiemap-extent-shared+)
                  (return-from %fiemap-all-shared-p nil))
                (setf seen t)
                (when (logtest fe-flags +fiemap-extent-last+)
                  (return-from %fiemap-all-shared-p t))
                (setf next (+ fe-logical fe-length)))))
          (when (<= next start)
            (return seen))              ; no progress: answer from what was seen
          (setf start next))))))

(defun file-extents-shared-p (pathname)
  "For tests: T when PATHNAME (a non-empty regular file) has at least one
extent and every extent carries FIEMAP_EXTENT_SHARED, read with
ioctl FS_IOC_FIEMAP (FIEMAP_FLAG_SYNC, all extents up to the one marked
FIEMAP_EXTENT_LAST). NIL for an empty file or when some extent is not shared.
Signals FICLONE-ERROR (operation \"open\" or \"ioctl FS_IOC_FIEMAP\") when the
file cannot be opened or mapped, and off Linux."
  (let ((fd (%open-or-signal (%clone-namestring pathname) (logior +o-rdonly+ +o-cloexec+) 0 pathname)))
    (unwind-protect (%fiemap-all-shared-p fd pathname)
      (%close fd))))
