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
