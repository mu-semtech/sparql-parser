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
  (cond ((and timeout (< timeout 0))
         (error 'semaphore-timeout :semaphore semaphore))
        ((bt:wait-on-semaphore semaphore :timeout timeout)
         (unwind-protect (funcall functor)
           (sb-thread:signal-semaphore semaphore)))
        (t (error 'semaphore-timeout :semaphore semaphore))))

(defun with-multiple-semaphores* (semaphores functor &key individual-timeout total-timeout)
  "Executes functor once all SEMAPHOREs have been acquired in order,
waiting at most `INDIVIDUAL-TIMEOUT' seconds for each acquisition and
`TOTAL-TIMEOUT' in total.  IF both are nil, timeout is infinite."
  (if semaphores
      (let ((start (get-internal-real-time))
            (timeout (and (or individual-timeout total-timeout)
                          (apply #'min
                                 (remove-if-not
                                  #'identity
                                  (list individual-timeout total-timeout))))))
        (with-semaphore ((first semaphores) :timeout timeout)
          (with-multiple-semaphores* (rest semaphores)
            functor
            :individual-timeout individual-timeout
            :total-timeout (and total-timeout
                                (- total-timeout
                                   (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))
      (funcall functor)))

(defmacro with-multiple-semaphores ((semaphores &rest args &key individual-timeout total-timeout) &body body)
  (declare (ignore individual-timeout total-timeout))
  `(with-multiple-semaphores* ,semaphores (lambda () ,@body) ,@args))

