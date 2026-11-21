(defpackage #:fs-backend-linux
  (:use #:cl)
  (:nicknames #:stack-fs-linux)
  (:local-nicknames (#:pathlib #:cl-stack-pathlib))
  (:export
   ;; the mechanism
   #:+inotify-rank+
   #:inotify-available-p
   #:*inotify-max-watches*
   ;; the journal
   #:inotify-journal
   #:inotify-journal-p
   #:inotify-journal-fd
   #:inotify-journal-watch-count
   #:inotify-journal-watched-directories
   #:inject-inotify-event
   ;; conditions
   #:inotify-error
   #:inotify-error-operation
   #:inotify-error-errno
   ;; inotify(7) event bits (the Linux ABI, the same on every architecture)
   #:+in-access+ #:+in-modify+ #:+in-attrib+ #:+in-close-write+
   #:+in-close-nowrite+ #:+in-open+ #:+in-moved-from+ #:+in-moved-to+
   #:+in-create+ #:+in-delete+ #:+in-delete-self+ #:+in-move-self+
   #:+in-unmount+ #:+in-q-overflow+ #:+in-ignored+
   #:+in-onlydir+ #:+in-dont-follow+ #:+in-excl-unlink+ #:+in-mask-add+
   #:+in-isdir+ #:+in-oneshot+))
