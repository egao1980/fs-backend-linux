(defsystem "fs-backend-linux"
  :version "0.1.0"
  :description "Linux mechanisms for cl-stack-pathlib: the inotify change journal"
  :author "egao1980"
  :license "MIT"
  :depends-on ("uiop")
  :serial t
  :pathname "src"
  :components ((:file "package")))
