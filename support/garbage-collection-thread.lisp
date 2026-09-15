(in-package #:support)

(defun garbage-collection-thread (timeout)
  "Run a garbage collection thread every TIMEOUT seconds."
  (bt:make-thread (lambda ()
                    (sleep timeout)
                    (cl-user::gc :full t))))
