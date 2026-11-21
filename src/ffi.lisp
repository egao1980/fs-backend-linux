(in-package #:fs-backend-linux)

;;; libc bindings for inotify(7), through CFFI. Everything lives in libc,
;;; which every Lisp image already has open, so there is no foreign library
;;; to load. Off Linux the same names are Lisp functions that signal
;;; INOTIFY-ERROR, so the system loads (and registers :INOTIFY, unavailable)
;;; on every OS with no undefined-alien warnings.
;;;
;;; Constants are the generic Linux ABI (asm-generic: x86, x86-64, arm,
;;; arm64, riscv). IN_NONBLOCK / IN_CLOEXEC are O_NONBLOCK / O_CLOEXEC,
;;; which differ on alpha, sparc, mips and parisc (not supported).

;;; --- inotify(7) bits ---------------------------------------------------------------

(defconstant +in-access+        #x00000001)
(defconstant +in-modify+        #x00000002)
(defconstant +in-attrib+        #x00000004)
(defconstant +in-close-write+   #x00000008)
(defconstant +in-close-nowrite+ #x00000010)
(defconstant +in-open+          #x00000020)
(defconstant +in-moved-from+    #x00000040)
(defconstant +in-moved-to+      #x00000080)
(defconstant +in-create+        #x00000100)
(defconstant +in-delete+        #x00000200)
(defconstant +in-delete-self+   #x00000400)
(defconstant +in-move-self+     #x00000800)
(defconstant +in-unmount+       #x00002000)
(defconstant +in-q-overflow+    #x00004000)
(defconstant +in-ignored+       #x00008000)
(defconstant +in-onlydir+       #x01000000)
(defconstant +in-dont-follow+   #x02000000)
(defconstant +in-excl-unlink+   #x04000000)
(defconstant +in-mask-add+      #x20000000)
(defconstant +in-isdir+         #x40000000)
(defconstant +in-oneshot+       #x80000000)

(defconstant +in-nonblock+ #o0004000)
(defconstant +in-cloexec+  #o2000000)

;;; struct inotify_event { int wd; uint32_t mask; uint32_t cookie;
;;;                        uint32_t len; char name[]; }
(defconstant +inotify-event-header-size+ 16)

;;; errno values (asm-generic)
(defconstant +enoent+  2)
(defconstant +eintr+   4)
(defconstant +ebadf+   9)
(defconstant +eagain+ 11)
(defconstant +eacces+ 13)
(defconstant +enotdir+ 20)
(defconstant +emfile+ 24)
(defconstant +enospc+ 28)

;;; --- conditions ------------------------------------------------------------------

(define-condition inotify-error (pathlib:journal-error)
  ((operation :initarg :operation :reader inotify-error-operation :initform nil
              :documentation "The libc call that failed, a string such as \"inotify_init1\".")
   (errno :initarg :errno :reader inotify-error-errno :initform nil
          :documentation "errno after the failed call, or NIL (no inotify on this OS)."))
  (:documentation "An inotify system call failed where the journal cannot
answer COMPLETE-P NIL instead: creating the inotify instance in FS-WATCH
(EMFILE: fs.inotify.max_user_instances), or any call on a non-Linux OS.")
  (:report (lambda (c s)
             (format s "inotify: ~A failed~@[ (errno ~D)~]~@[: ~A~]"
                     (inotify-error-operation c) (inotify-error-errno c)
                     (ignore-errors (pathlib:journal-error-journal c))))))

;;; --- calls -------------------------------------------------------------------------

(defmacro %define-libc (lisp-name c-name return-type &rest args)
  "A CFFI binding of the libc function C-NAME on Linux; elsewhere a function
of the same arguments that signals INOTIFY-ERROR."
  (declare (ignorable return-type))
  #+linux
  `(cffi:defcfun (,c-name ,lisp-name) ,return-type ,@args)
  #-linux
  `(defun ,lisp-name ,(mapcar #'first args)
     (declare (ignore ,@(mapcar #'first args)))
     (error 'inotify-error :operation ,c-name :errno nil)))

(%define-libc %inotify-init1 "inotify_init1" :int (flags :int))
(%define-libc %inotify-add-watch "inotify_add_watch" :int
  (fd :int) (pathname :string) (mask :uint32))
(%define-libc %inotify-rm-watch "inotify_rm_watch" :int (fd :int) (wd :int))
(%define-libc %read "read" :ssize (fd :int) (buf :pointer) (count :size))
(%define-libc %close "close" :int (fd :int))
(%define-libc %errno-location "__errno_location" :pointer)

(defun %errno ()
  "errno of the calling thread, right after a failed call."
  (cffi:mem-ref (%errno-location) :int))

(defun %linux-p ()
  #+linux t
  #-linux nil)

;;; =================================================================================
;;; R4c-2: FICLONE reflink clones and the FIEMAP sharing check
;;; =================================================================================
;;;
;;; More libc calls (open, ioctl, unlink; close and __errno_location above).
;;; open(2) and ioctl(2) are variadic, so they go through
;;; CFFI:FOREIGN-FUNCALL-VARARGS. Off Linux the same names are Lisp functions
;;; that signal FICLONE-ERROR (a PATHLIB:CLONE-ERROR, so USE-COPY applies).

;;; --- ioctl numbers (asm-generic _IOC encoding: x86-64, arm64, riscv64) -----------

(defconstant +ficlone+ #x40049409
  "FICLONE = _IOW(0x94, 9, int): ioctl(dst_fd, FICLONE, src_fd) makes DST share
all of SRC's extents (a reflink). btrfs, XFS with reflink=1, bcachefs, OpenZFS
2.2+; anything else fails with EOPNOTSUPP / ENOTTY / EINVAL / EXDEV.")

(defconstant +fs-ioc-fiemap+ #xC020660B
  "FS_IOC_FIEMAP = _IOWR('f', 11, struct fiemap): the extent map of a file.")

(defconstant +fiemap-flag-sync+ #x00000001
  "FIEMAP_FLAG_SYNC: write dirty pages back before mapping (delalloc extents
would otherwise have no physical place yet).")

(defconstant +fiemap-extent-last+ #x00000001
  "FIEMAP_EXTENT_LAST: the last extent of the file.")

(defconstant +fiemap-extent-shared+ #x00002000
  "FIEMAP_EXTENT_SHARED: the extent is referenced by more than one file (a
reflink or a snapshot). FILE-EXTENTS-SHARED-P tests it on every extent.")

;;; struct fiemap { __u64 fm_start; __u64 fm_length; __u32 fm_flags;
;;;   __u32 fm_mapped_extents; __u32 fm_extent_count; __u32 fm_reserved;
;;;   struct fiemap_extent fm_extents[]; }                       ; 32 octets
(cffi:defcstruct (fiemap :size 32)
  (fm-start :uint64 :offset 0)
  (fm-length :uint64 :offset 8)
  (fm-flags :uint32 :offset 16)
  (fm-mapped-extents :uint32 :offset 20)
  (fm-extent-count :uint32 :offset 24)
  (fm-reserved :uint32 :offset 28))

;;; struct fiemap_extent { __u64 fe_logical; __u64 fe_physical; __u64 fe_length;
;;;   __u64 fe_reserved64[2]; __u32 fe_flags; __u32 fe_reserved[3]; } ; 56 octets
(cffi:defcstruct (fiemap-extent :size 56)
  (fe-logical :uint64 :offset 0)
  (fe-physical :uint64 :offset 8)
  (fe-length :uint64 :offset 16)
  (fe-flags :uint32 :offset 40))

(defconstant +fiemap-header-size+ 32)
(defconstant +fiemap-extent-size+ 56)

;;; --- open(2) flags ---------------------------------------------------------------
;;; O_NOFOLLOW (and O_DIRECTORY) differ on arm / arm64 from x86-64 and the
;;; asm-generic value riscv64 uses; the rest are the same on all three.

(defconstant +o-rdonly+  #o0)
(defconstant +o-wronly+  #o1)
(defconstant +o-creat+   #o100)
(defconstant +o-excl+    #o200)
(defconstant +o-cloexec+ #o2000000)
(defconstant +o-nofollow+
  #+(or arm arm64) #o100000
  #-(or arm arm64) #o400000)

;;; --- errno values the clone code maps (asm-generic; x86-64, arm64, riscv64) ---------

(defconstant +eperm+        1)
(defconstant +eio+          5)
(defconstant +ebusy+       16)
(defconstant +eexist+      17)
(defconstant +exdev+       18)
(defconstant +einval+      22)
(defconstant +enotty+      25)
(defconstant +etxtbsy+     26)
(defconstant +erofs+       30)
(defconstant +enosys+      38)
(defconstant +eloop+       40)
(defconstant +eopnotsupp+  95)
(defconstant +edquot+     122)

;;; --- the condition ---------------------------------------------------------------

(define-condition ficlone-error (pathlib:clone-error)
  ()
  (:default-initargs :mechanism :ficlone)
  (:documentation "A FICLONE clone failed for a reason a plain copy may still
get past: open(2) of SOURCE or DESTINATION failed, or ioctl FICLONE returned an
errno that is not a fallback reason (EACCES, ENOSPC, EDQUOT, EIO, EROFS, ...).
CLONE-ERROR-OPERATION is \"open\" or \"ioctl FICLONE\" (\"ioctl FS_IOC_FIEMAP\"
for FILE-EXTENTS-SHARED-P), CLONE-ERROR-CODE the errno (NIL off Linux),
PATH-ERROR-PATH the file. pathlib's USE-COPY restart is around every
FICLONE-FILE call it makes."))

;;; --- calls -----------------------------------------------------------------------

(defmacro %define-clone-call (lisp-name c-name lambda-list &body linux-body)
  "A function LISP-NAME of LAMBDA-LIST: LINUX-BODY on Linux; elsewhere it
signals FICLONE-ERROR with operation C-NAME and no errno."
  (declare (ignorable linux-body))
  #+linux
  `(defun ,lisp-name ,lambda-list ,@linux-body)
  #-linux
  `(defun ,lisp-name ,lambda-list
     (declare (ignorable ,@lambda-list))
     (error 'ficlone-error :operation ,c-name :code nil
            :path ,(when (member 'pathname lambda-list) 'pathname)
            :message "FICLONE is Linux only")))

(%define-clone-call %open "open" (pathname flags mode)
  ;; int open(const char *pathname, int flags, ... /* mode_t mode */);
  (cffi:foreign-funcall-varargs "open" (:string pathname :int flags)
                                :unsigned-int mode :int))

(%define-clone-call %ioctl-int "ioctl" (fd request arg)
  ;; int ioctl(int fd, unsigned long request, ... /* int */);  FICLONE
  (cffi:foreign-funcall-varargs "ioctl" (:int fd :unsigned-long request)
                                :int arg :int))

(%define-clone-call %ioctl-pointer "ioctl" (fd request arg)
  ;; int ioctl(int fd, unsigned long request, ... /* void * */);  FS_IOC_FIEMAP
  (cffi:foreign-funcall-varargs "ioctl" (:int fd :unsigned-long request)
                                :pointer arg :int))

(%define-clone-call %unlink "unlink" (pathname)
  (cffi:foreign-funcall "unlink" :string pathname :int))
