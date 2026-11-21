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

(defun %r4c2-pending (what)
  (error "fs-backend-linux R4c-2: ~A is not implemented yet" what))

;;; --- errno → reason --------------------------------------------------------------

(defun %ficlone-errno-reason (errno)
  "The locked mapping of an errno from ioctl FICLONE: :UNSUPPORTED-VOLUME
(EOPNOTSUPP, ENOTTY, ENOSYS), :CROSS-VOLUME (EXDEV), :CLONE-FAILED (EINVAL,
EPERM, ETXTBSY, EBUSY), or NIL: not a fallback, FICLONE-FILE signals
FICLONE-ERROR."
  (declare (ignorable errno))
  (%r4c2-pending '%ficlone-errno-reason))

;;; --- the probe -------------------------------------------------------------------

(defun %probe-clone (source-dir destination-dir)
  "The Linux branch of FICLONE-AVAILABLE-P: a real clone on the target
volume, as described above. → (values available-p reason); never signals for
an errno (a temp file that cannot be created → (NIL :UNAVAILABLE))."
  (declare (ignorable source-dir destination-dir))
  (%r4c2-pending 'ficlone-available-p))

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
  (declare (ignorable fs source destination))
  (%r4c2-pending 'ficlone-file))

;;; --- the sharing check (tests) ---------------------------------------------------

(defun file-extents-shared-p (pathname)
  "For tests: T when PATHNAME (a non-empty regular file) has at least one
extent and every extent carries FIEMAP_EXTENT_SHARED, read with
ioctl FS_IOC_FIEMAP (FIEMAP_FLAG_SYNC, all extents up to the one marked
FIEMAP_EXTENT_LAST). NIL for an empty file or when some extent is not shared.
Signals FICLONE-ERROR (operation \"open\" or \"ioctl FS_IOC_FIEMAP\") when the
file cannot be opened or mapped, and off Linux."
  (declare (ignorable pathname))
  (%r4c2-pending 'file-extents-shared-p))
