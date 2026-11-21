(in-package #:fs-backend-linux/tests)

;;; The R4c verdict: cl-stack-pathlib's clone suite with :FICLONE on each
;;; reflink directory (btrfs and XFS in the `reflink` job), with the other
;;; one as OTHER-VOLUME (so :CROSS-VOLUME runs) and without.

(defreflink ficlone-clone-suite (dir other)
  (multiple-value-bind (failures steps)
      (run-clone-suite :ficlone :directory dir :other-volume other)
    (ok (null failures) (format nil ":ficlone keeps the clone laws on ~A: ~S" (volume-type dir) failures))
    (ok (equal (clone-suite-steps :mechanism :ficlone :other-volume other) steps)
        "every applicable step ran")
    (if other
        (ok (member :cross-volume steps) ":cross-volume against the other reflink volume")
        (when (require-reflink-p)
          (fail "the reflink job needs a second reflink directory for :cross-volume")))))

(defreflink ficlone-clone-suite-one-volume (dir)
  (ok (null (run-clone-suite :ficlone :directory dir))))
