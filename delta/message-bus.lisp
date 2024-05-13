(in-package :delta-messenger)

(defparameter *message-bus-queue* nil)
(defparameter *message-bus-lock* (bt:make-lock "messenger"))
(defparameter *sleep-on-idle-bus* 1)
(defparameter *message-bus-consumer* nil
  "Consumer to be called for each message on the message bus.")

(defun message-bus-loop ()
  "Loop for the delta message bus."
  (handler-case
      (let (available-messages)
        (bt:with-lock-held (*message-bus-lock*)
          (setf available-messages (nreverse (copy-list *message-bus-queue*))) ; reverse takes a copy
          (setf *message-bus-queue* nil))
        (dolist (message available-messages)
          (apply *message-bus-consumer* message))
        (unless available-messages
          ;; TODO: make thread sleep if nothing is on the bus and wake through SCHEDULE-DELTA-MESSAGE
          (sleep *sleep-on-idle-bus*)))
    (error (e) (format t "~&Error occurred on messenger message bus!~% ~A~%; waiting 3s and continuing next cycle, ignoring current messages~&" e)))
  ;; (format t "~&Processing ~A delta messages from bus" (length available-messages))
  (message-bus-loop))

(defun launch-message-consumer (consuming-function)
  "launches a message consumer"
  (setf *message-bus-consumer* consuming-function)
  (bt:make-thread #'message-bus-loop :name "message-bus"))

(defun schedule-delta-message (&rest messages)
  "schedules messages for the notifier system."
  (bt:with-lock-held (*message-bus-lock*)
    (setf *message-bus-queue*
          (nconc (reverse messages) *message-bus-queue*))))
