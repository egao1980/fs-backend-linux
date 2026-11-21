(in-package #:fs-backend-linux/tests)

;;; :FICLONE on every OS: registration, constants, the condition, the errno
;;; mapping, the memory filesystem, and the off-Linux answers.

(deftest ficlone-registered-at-load
  (let ((names (pathlib:clone-mechanisms)))
    (ok (member :ficlone names) "loading fs-backend-linux registers :ficlone")
    (ok (eq :copy (car (last names))) ":copy stays last")
    (ok (< (position :ficlone names) (position :copy names)) "ahead of :copy")
    (ok (eql 100 +ficlone-rank+))
    (ok (member :inotify (pathlib:watch-mechanisms)) ":inotify is still registered beside it")))

(deftest ficlone-registration-scoped-by-let
  (let ((pathlib:*clone-mechanisms* pathlib:*clone-mechanisms*))
    (pathlib:unregister-clone-mechanism :ficlone)
    (ok (not (member :ficlone (pathlib:clone-mechanisms)))))
  (ok (member :ficlone (pathlib:clone-mechanisms)) "back outside the LET"))

(deftest ficlone-constants
  ;; _IOW(0x94, 9, int) and _IOWR('f', 11, struct fiemap) in the asm-generic encoding:
  ;; dir << 30 | size << 16 | type << 8 | nr.
  (ok (eql #x40049409 +ficlone+))
  (ok (eql +ficlone+ (logior (ash 1 30) (ash 4 16) (ash #x94 8) 9)) "FICLONE = _IOW(0x94, 9, int)")
  (ok (eql #xC020660B +fs-ioc-fiemap+))
  (ok (eql +fs-ioc-fiemap+ (logior (ash 3 30) (ash 32 16) (ash (char-code #\f) 8) 11))
      "FS_IOC_FIEMAP = _IOWR('f', 11, struct fiemap), 32-octet header")
  (ok (eql #x2000 +fiemap-extent-shared+))
  (ok (eql 32 (cffi:foreign-type-size '(:struct fs-backend-linux::fiemap))))
  (ok (eql 56 (cffi:foreign-type-size '(:struct fs-backend-linux::fiemap-extent)))))

(deftest ficlone-error-is-a-clone-error
  (let ((c (make-condition 'ficlone-error :operation "ioctl FICLONE" :code 13
                                          :path #p"/x/a.txt")))
    (ok (typep c 'pathlib:clone-error))
    (ok (typep c 'pathlib:path-error))
    (ok (eq :ficlone (pathlib:clone-error-mechanism c)) "mechanism :ficlone by default")
    (ok (equal "ioctl FICLONE" (pathlib:clone-error-operation c)))
    (ok (eql 13 (pathlib:clone-error-code c)))
    (ok (search "ioctl FICLONE" (princ-to-string c)))))

;;; The locked errno mapping (generic Linux errno values).
(deftest ficlone-errno-reasons
  (flet ((reason (errno) (fs-backend-linux::%ficlone-errno-reason errno)))
    (testing "not supported by the volume"
      (ok (eq :unsupported-volume (reason 95)) "EOPNOTSUPP")
      (ok (eq :unsupported-volume (reason 25)) "ENOTTY")
      (ok (eq :unsupported-volume (reason 38)) "ENOSYS"))
    (testing "two volumes"
      (ok (eq :cross-volume (reason 18)) "EXDEV"))
    (testing "this file cannot be cloned"
      (ok (eq :clone-failed (reason 22)) "EINVAL (btrfs NOCOW against COW)")
      (ok (eq :clone-failed (reason 1)) "EPERM (immutable, append-only)")
      (ok (eq :clone-failed (reason 26)) "ETXTBSY (swap file)")
      (ok (eq :clone-failed (reason 16)) "EBUSY"))
    (testing "errors: FICLONE-ERROR, use-copy"
      (dolist (errno '(13 28 122 5 30 9 2))   ; EACCES ENOSPC EDQUOT EIO EROFS EBADF ENOENT
        (ok (null (reason errno)) (format nil "errno ~D is not a fallback reason" errno))))))

(deftest ficlone-unsupported-on-memory-filesystem
  (let ((fs (pathlib:make-memory-filesystem)))
    (pathlib:with-filesystem (fs)
      (pathlib:mkdir "/w/src" :parents t)
      (pathlib:write-text "/w/src/a.txt" "a")
      (ok (equal '(nil :unsupported-filesystem)
                 (multiple-value-list (ficlone-available-p fs #p"/w/src/" #p"/w/")))
          "a memory filesystem is never reflinked")
      (ok (not (member :ficlone (pathlib:clone-mechanisms fs #p"/w/src/" #p"/w/"))))
      (ok (equal '(nil :unsupported-filesystem)
                 (multiple-value-list (pathlib:fs-clone-supported-p fs #p"/w/src/"))))
      (let ((r (pathlib:fs-clone-tree fs #p"/w/src/" #p"/w/dst/")))
        (ok (eq :copy (pathlib:clone-report-mechanism r)))
        (ok (eq :unsupported-filesystem (pathlib:clone-report-reason r))))
      (ok (signals (pathlib:fs-clone-tree fs #p"/w/src/" #p"/w/dst2/" :mechanism :ficlone)
              'pathlib:unsupported-operation))
      (ok (not (pathlib:exists-p "/w/dst2/")) "refused before anything is created"))))

(deftest ficlone-unavailable-off-linux
  (if (linux-p)
      (skip "Linux: the off-Linux behaviour is checked on macOS and Windows")
      (with-scratch (root)
        (let ((fs (pathlib:path-filesystem root))
              (dir (native-dir root)))
          (put root "src/a.txt" "a")
          (ok (equal '(nil :unavailable)
                     (multiple-value-list (ficlone-available-p fs (native-dir (sub root "src" :directory t)) dir))))
          (ok (not (member :ficlone (pathlib:clone-mechanisms fs (native-dir (sub root "src" :directory t)) dir)))
              "not offered for a local directory")
          (ok (signals (pathlib:fs-clone-tree fs (sub root "src" :directory t) (sub root "dst" :directory t)
                                              :mechanism :ficlone)
                  'pathlib:unsupported-operation)
              "by name: unsupported-operation")
          (ok (not (pathlib:exists-p (sub root "dst" :directory t))) "nothing created")))))

(deftest ficlone-calls-off-linux
  ;; The bindings are stand-ins that signal FICLONE-ERROR, so a direct call fails
  ;; cleanly and pathlib's USE-COPY would apply.
  (if (linux-p)
      (skip "Linux: the off-Linux stand-ins are checked on macOS and Windows")
      (with-scratch (root)
        (let* ((src (native (put root "a.txt" "a")))
               (dst (native (sub root "b.txt"))))
          (ok (signals (ficlone-file (pathlib:path-filesystem root) src dst) 'ficlone-error))
          (ok (not (probe-file dst)) "no destination left")
          (ok (signals (file-extents-shared-p src) 'ficlone-error))))))
