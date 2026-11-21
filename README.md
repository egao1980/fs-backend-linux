# fs-backend-linux

Linux mechanisms for [`cl-stack-pathlib`](https://github.com/egao1980/cl-stack-pathlib)
(≥ 0.10.0):

- the `:inotify` change journal behind `fs-watch`, so that
  `fs-open-area … :journal :auto` re-examines only the paths that changed (0.1.0);
- the `:ficlone` clone mechanism behind `fs-clone-tree` / `fs-clone-file`, so
  that areas on btrfs, XFS (reflink=1) and bcachefs are reflinked instead of
  copied (0.2.0).

```lisp
(asdf:load-system "fs-backend-linux")      ; registers :inotify and :ficlone
(stack-pathlib:watch-mechanisms (stack-pathlib:make-local-filesystem) #p"/tmp/")
;; → (:INOTIFY :POLL) on Linux, (:POLL) elsewhere
(stack-pathlib:fs-clone-supported-p (stack-pathlib:make-local-filesystem) #p"/mnt/btrfs/")
;; → :FICLONE on a reflink volume; NIL, :UNSUPPORTED-VOLUME on ext4 / tmpfs
```

- CFFI bindings to libc (`inotify_init1`, `ioctl`, …): no native library to
  ship. The system loads on every OS; both mechanisms are available on Linux
  only, for local directories.
- One inotify instance per journal, one watch per directory, no thread:
  `journal-drain` reads the kernel queue. Overflow, the watch limit, or the
  root going away answer COMPLETE-P NIL (rescan).
- `ioctl(FICLONE)` per regular file; detection by a probe clone on the target
  volume that only reads the source. `EOPNOTSUPP` / `EXDEV` / `EINVAL` fall
  back to a copy with a reason; other errors are `ficlone-error`
  (a `clone-error`, so `use-copy` applies).

## Tests

`cl-repo test --system fs-backend-linux`. The reflink tests need reflink
volumes: set `FS_BACKEND_LINUX_REFLINK_DIRS` to colon-separated directories on
them (otherwise they skip); `FS_BACKEND_LINUX_REQUIRE_REFLINK=1` makes them
fail instead of skipping. CI job `reflink` mounts a btrfs and an XFS loop
volume on the ubuntu-latest host. Locally, in Docker:

```sh
docker run --rm --privileged -v "$PWD":/src:ro ghcr.io/egao1980/cl-repository/ci-base:latest bash -c '
  apt-get update -q && apt-get install -y -q btrfs-progs xfsprogs
  truncate -s 1G /b.img /x.img && mkfs.btrfs -q /b.img && mkfs.xfs -q -m reflink=1 /x.img
  mkdir -p /mnt/btrfs /mnt/xfs && mount -o loop /b.img /mnt/btrfs && mount -o loop /x.img /mnt/xfs
  cp -a /src /w && cd /w && FS_BACKEND_LINUX_REFLINK_DIRS=/mnt/btrfs:/mnt/xfs FS_BACKEND_LINUX_REQUIRE_REFLINK=1 cl-repo test --system fs-backend-linux'
```

Brief: [cl-stack/docs/capabilities/fs-backend-linux.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/fs-backend-linux.md) (R4b-1, R4c-2).

## License

MIT
