(in-package :woo.worker.utils)

(defun decomission ()
  "Decomission the current worker.

Ensures no new assignments are given to the worker and its queue is
cleared for others."
  (let ((worker woo.worker::*worker*))
    (setf (woo.worker::worker-status worker)
          :decomissioned)
    (let ((queue (woo.worker::worker-queue worker)))
      (loop until (woo.worker::queue-empty-p queue)
            do (woo.worker:add-job-to-cluster woo.specials:*cluster*
                                              (woo.worker::dequeue queue))))))

(defun recomission ()
  "Recomission the current worker."
  (setf (woo.worker::worker-status woo.worker::*worker*)
        :running))

(defparameter *max-tries-for-adding-job* 10
  "After this amount of tries, we assign the job to a worker, even if that worker is decomissioned.")

(defun woo.worker::add-job-to-cluster (cluster job &key (tries *max-tries-for-adding-job*))
  "Add a job to te cluster, overrides original function.

  Tries to assign to REMAINING-TRIES workers and forces when all of theme are decomissioned."
  ;; Try to assign to `*max-tries-for-adding-job*' workers, then force-queue
  (let* ((workers (woo.worker::cluster-circular-workers cluster))
         (worker (car workers)))
    (setf (woo.worker::cluster-circular-workers cluster)
          (cdr workers))
    (if (or (<= tries 0) 
            (eq (woo.worker::worker-status worker)
                :decomissioned))
        (woo.worker::add-job-to-cluster cluster job :tries (1- tries))
        (progn
          (woo.worker::add-job worker job)
          (woo.worker::notify-new-job worker)))))
