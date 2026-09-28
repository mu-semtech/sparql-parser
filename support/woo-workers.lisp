(in-package :woo.worker.utils)

(defparameter *cluster-reference* nil
  "WOO cluster used by sparql-parser.  Worker threads do not see the `WOO:RUN' binding of
`WOO.SPECIALS:*CLUSTER*' and we need it to reschedule threads.")

(defparameter *log-worker-crashes-p* t
  "When truethy, worker crashes are logged.")

(defun abort-thread-after-maybe-logging (condition debugger-hook)
  "Maybe logs the worker error and kills the thread.

When the thread launches the debugger we should kill it so a new worker can be spawned.  This function kills the thread
but first checks if we should log the condition through `*LOG-WORKER-CRASHES*'"
  (declare (ignore debugger-hook))
  (when *log-worker-crashes-p*
    (let ((*print-pretty* nil))
      (format t "~&Thread killed by unhandled condition: ~A~%~%Condition backtrace:~%"
              condition)
      (trivial-backtrace:print-backtrace condition :output *standard-output*))
    (finish-output))
  (sb-thread:abort-thread))

(let ((original-default-thread-bindings (fdefinition 'woo.specials:default-thread-bindings)))
  ;; We append to the original function definition to cope with changes on the upstream's end.
  (defun woo.specials::default-thread-bindings ()
    "Special bindings for new worker threads"
    (append (funcall original-default-thread-bindings)
            `((sb-ext:*invoke-debugger-hook* . ,#'abort-thread-after-maybe-logging)))))

(defparameter *log-commission-events* t
  "When truethy, commission and decommission events are logged.")

(defparameter *log-scheduling-on-decommissioned-workers* t
  "When truethy, log scheduling of requests on a decommissioned worker.
This scheduling should only happen when too many workers are
decommissioned.")

(defun decommission ()
  "Decommission the current worker.

Ensures no new assignments are given to the worker and its queue is
cleared for others."
  (let ((worker woo.worker::*worker*))
    (when *log-commission-events*
      (format t "~&Decommissioning ~A~%" (woo.worker::worker-id worker)))
    (setf (woo.worker::worker-status worker)
          :decommissioned)
    (let ((queue (woo.worker::worker-queue worker)))
      (loop until (woo.worker::queue-empty-p queue)
            do (woo.worker:add-job-to-cluster *cluster-reference*
                                              (woo.worker::dequeue queue))))))

(defun recommission ()
  "Recommission the current worker."
  (when *log-commission-events*
    (format t "~&Recommissioning ~A~%" (woo.worker::worker-id woo.worker::*worker*)))
  (setf (woo.worker::worker-status woo.worker::*worker*)
        :running))

(defparameter *max-tries-for-adding-job* 10
  "After this amount of tries, we assign the job to a worker, even if that worker is decommissioned.")

(defparameter *update-next-worker-lock* (bt:make-lock "add-job-to-cluster"))

(defun woo.worker::add-job-to-cluster (cluster job &key (tries *max-tries-for-adding-job*) (require-lock-p t))
  "Assigns `JOB' to `CLUSTER' in a commissioned worker.  If after `TRIES' we have
only found :decommissioned workers the job is added to any worker, even if
decomissioned."
  (let* ((workers (woo.worker::cluster-circular-workers cluster))
         (worker (car workers)))
    (bt:with-lock-held (*update-next-worker-lock*)
      (setf workers (woo.worker::cluster-circular-workers cluster))
      (setf worker (car workers))
      (setf (woo.worker::cluster-circular-workers cluster)
            (cdr workers)))
    (if (and (>= tries 0) 
             (eq (woo.worker::worker-status worker)
                 :decommissioned))
        (progn
          (when *log-commission-events*
            (format t "~&Skipping decommissioned worker ~A (~A tries left)~%"
                    (woo.worker::worker-id worker)
                    tries))
          (woo.worker::add-job-to-cluster cluster job :tries (1- tries)))
        (progn
          (when (eq (woo.worker::worker-status worker)
                    :decommissioned)
            (format t "~&Scheduling job on decommissioned worker ~A~%"
                    (woo.worker::worker-id worker)))
          (woo.worker::add-job worker job)
          (woo.worker::notify-new-job worker)))))

(let ((original-make-cluster (fdefinition 'woo.worker::make-cluster)))
  ;; same as the old definition, but we add our own cluster reference.
  (defun woo.worker::make-cluster (worker-num process-fn)
    (let ((cluster (funcall original-make-cluster worker-num process-fn)))
      (setf *cluster-reference* cluster)
      cluster)))

(let ((original-finalize-worker (fdefinition 'woo.worker::finalize-worker)))
  (defun woo.worker::finalize-worker (worker)
    ;; ensures *cluster* is available when a worker finalizes.
    (let ((woo.specials:*cluster* (or woo.specials:*cluster* *cluster-reference*)))
      (funcall original-finalize-worker worker))))
