# fs-backend-linux

Linux mechanisms for [`cl-stack-pathlib`](https://github.com/egao1980/cl-stack-pathlib)
(≥ 0.9.0): the `:inotify` change journal behind `fs-watch`, so that
`fs-open-area … :journal :auto` re-examines only the paths that changed.

```lisp
(asdf:load-system "fs-backend-linux")      ; registers :inotify
(stack-pathlib:watch-mechanisms (stack-pathlib:make-local-filesystem) #p"/tmp/")
;; → (:INOTIFY :POLL) on Linux, (:POLL) elsewhere
```

- CFFI bindings to libc (`inotify_init1`, `inotify_add_watch`, …): no native
  library to ship. The system loads on every OS; `:inotify` is available on
  Linux only, for local directories.
- One inotify instance per journal, one watch per directory, no thread:
  `journal-drain` reads the kernel queue. Overflow, the watch limit, or the
  root going away answer COMPLETE-P NIL (rescan).

Brief: [cl-stack/docs/capabilities/fs-backend-linux.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/fs-backend-linux.md) (R4b-1).

## License

MIT
