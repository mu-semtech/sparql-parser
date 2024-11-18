(in-package :woo.worker.utils)

(defparameter *backup-cluster* nil)

(defparameter *log-commission-events* t
  "When truethy, commission and decommission events are logged.")

(defparameter *log-scheduling-on-decomissioned-workers* t
  "When truethy, log scheduling of requests on a decomissioned worker.
This scheduling should only happen when too many workers are
decomissioned.")

(defun decommission ()
  "Decommission the current worker.

Ensures no new assignments are given to the worker and its queue is
cleared for others."
  (let ((worker woo.worker::*worker*))
    (when *log-commission-events*
      (format t "~&Decomissioning ~A~%" (woo.worker::worker-id worker)))
    (setf (woo.worker::worker-status worker)
          :decommissioned)
    (let ((queue (woo.worker::worker-queue worker)))
      (loop until (woo.worker::queue-empty-p queue)
            do (woo.worker:add-job-to-cluster *backup-cluster*
                                              (woo.worker::dequeue queue))))))

(defun recommission ()
  "Recommission the current worker."
  (when *log-commission-events*
    (format t "~&Recomissioning ~A~%" (woo.worker::worker-id woo.worker::*worker*)))
  (setf (woo.worker::worker-status woo.worker::*worker*)
        :running))

(defparameter *max-tries-for-adding-job* 10
  "After this amount of tries, we assign the job to a worker, even if that worker is decommissioned.")

(defun woo.worker::add-job-to-cluster (cluster job &key (tries *max-tries-for-adding-job*))
  "Add a job to te cluster, overrides original function.

  Tries to assign to REMAINING-TRIES workers and forces when all of theme are decommissioned."
  ;; Try to assign to `*max-tries-for-adding-job*' workers, then force-queue
  (let* ((workers (woo.worker::cluster-circular-workers cluster))
         (worker (car workers)))
    (setf (woo.worker::cluster-circular-workers cluster)
          (cdr workers))
    (if (and (>= tries 0) 
             (eq (woo.worker::worker-status worker)
                 :decommissioned))
        (progn
          (when *log-commission-events*
            (format t "~&Skipping decomissioned worker ~A (~A tries left)~%"
                    (woo.worker::worker-id worker)
                    tries))
          (woo.worker::add-job-to-cluster cluster job :tries (1- tries)))
        (progn
          (when (eq (woo.worker::worker-status worker)
                    :decommissioned)
            (format t "~&Scheduling job on decomissioned worker ~A~%"
                    (woo.worker::worker-id worker)))
          (woo.worker::add-job worker job)
          (woo.worker::notify-new-job worker)))))

(defun woo.worker::make-cluster (worker-num process-fn)
  (let ((cluster (woo.worker::%make-cluster)))
    (labels ((make-new-worker ()
               (vom:debug "Starting a new worker...")
               (woo.worker::make-worker (lambda (&rest args) (apply process-fn args))
                            (lambda (worker)
                              (setf (woo.worker::cluster-workers cluster)
                                    (cons (make-new-worker)
                                          (remove worker (woo.worker::cluster-workers cluster) :test #'eq)))))))
      (setf (woo.worker::cluster-workers cluster)
            (loop repeat worker-num
                  collect (make-new-worker))))
    (setf *backup-cluster* cluster)))
