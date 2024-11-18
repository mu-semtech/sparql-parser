(in-package :support)

(define-condition semaphore-timeout (error)
  ((semaphore :initarg :semaphore :reader semaphore-timeout-semaphore)))

(defmacro with-semaphore ((semaphore &key timeout) &body body)
  "Executes BODY with SEMAPHORE held waiting at most TIMEOUT to acquire it.
Throws SEMAPHORE-TIMEOUT when timeout passed."
  (let ((semaphore-sym (gensym "SEMAPHORE")))
    `(let ((,semaphore-sym ,semaphore))
       (if (bt:wait-on-semaphore ,semaphore-sym :timeout ,timeout)
           (unwind-protect
                (progn ,@body)
             (sb-thread:signal-semaphore ,semaphore-sym))
           (error 'semaphore-timeout :semaphore ,semaphore-sym)))))

(defun with-multiple-semaphores* (semaphores functor &key timeout)
  "Executes functor when all fo the SEMAPHOREs"
  (if semaphores
      (with-semaphore ((first semaphores))
        (with-multiple-semaphores* (rest semaphores) functor :timeout timeout))
      (funcall functor)))

(defmacro with-multiple-semaphores ((semaphores &rest args &key timeout) &body body)
  (declare (ignore timeout))
  `(with-multiple-semaphores* ,semaphores (lambda () ,@body) ,@args))

