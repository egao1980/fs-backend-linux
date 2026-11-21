(defsystem "fs-backend-linux"
  :version "0.2.0"
  :description "Linux mechanisms for cl-stack-pathlib: the inotify change journal, FICLONE reflink clones"
  :author "egao1980"
  :license "MIT"
  ;; libc only (inotify_init1, inotify_add_watch, inotify_rm_watch, read,
  ;; open, ioctl, unlink, close, __errno_location): no foreign library to
  ;; load or ship.
  :depends-on ((:version "cl-stack-pathlib" "0.10.0") "cffi" "uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "ffi")
               (:file "inotify")
               (:file "clone")
               (:file "register"))
  :in-order-to ((test-op (test-op "fs-backend-linux/tests"))))

(defsystem "fs-backend-linux/tests"
  :depends-on ("fs-backend-linux" "cl-stack-pathlib/journal-suite"
               "cl-stack-pathlib/clone-suite" "rove" "uiop")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "registry-test")
               (:file "suite-test")
               (:file "inotify-test")
               (:file "lifecycle-test")
               (:file "clone-support")
               (:file "clone-registry-test")
               (:file "clone-detect-test")
               (:file "clone-reflink-test")
               (:file "clone-suite-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
