(in-package :support)

(define-condition semaphore-timeout (error)
  ((semaphore :initarg :semaphore :reader semaphore-timeout-semaphore)))

(defmacro with-semaphore ((semaphore &key timeout) &body body)
  "Executes BODY with SEMAPHORE held waiting at most TIMEOUT to acquire it.
Throws SEMAPHORE-TIMEOUT when timeout passed."
  (let ((semaphore-sym (gensym "SEMAPHORE")))
    `(let ((,semaphore-sym ,semaphore))
       (with-semaphore* (lambda () ,@body)
         ,semaphore-sym :timeout ,timeout))))

(defun with-semaphore* (functor semaphore &key timeout)
  (if (bt:wait-on-semaphore semaphore :timeout timeout)
      (unwind-protect (funcall functor)
        (sb-thread:signal-semaphore semaphore))
      (error 'semaphore-timeout :semaphore semaphore)))

(defun with-multiple-semaphores* (semaphores functor &key timeout)
  "Executes functor when all fo the SEMAPHOREs"
  (if semaphores
      (with-semaphore ((first semaphores))
        (with-multiple-semaphores* (rest semaphores) functor :timeout timeout))
      (funcall functor)))

(defmacro with-multiple-semaphores ((semaphores &rest args &key timeout) &body body)
  (declare (ignore timeout))
  `(with-multiple-semaphores* ,semaphores (lambda () ,@body) ,@args))

