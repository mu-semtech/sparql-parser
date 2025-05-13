(in-package :delta-messenger)

(defparameter *message-bus-queue* nil
  "Current queue on the message bus containing unconsumed messages.")
(defparameter *message-bus-lock* (bt:make-lock "messenger-lock")
  "Lock ensuring we don't modify the bus at the same time.")
(defparameter *message-bus-semaphore* (bt:make-semaphore :name "messenger-sem")
  "Semaphore used to notify about changes on the bus.
   Should be modified only with *MESSAGE-BUS-LOCK* but should not break.")
(defparameter *max-sleep-on-idle-bus* 60
  "Amount of seconds to sleep when the bus is considered idle.

   This is a safety setting that would cover an erroneous semaphore
   implementation. It could be set to NIL to disable the feature.")
(defparameter *message-bus-consumer* nil
  "Consumer to be called for each message on the message bus.")
(defparameter *log-delta-messenger-message-bus-processing* nil
  "Logs when the delta messenger runs and for what.")

(defun message-bus-loop ()
  "Loop for the delta message bus."
  (handler-case
      (let (available-messages)
        (bt:wait-on-semaphore *message-bus-semaphore* :timeout *max-sleep-on-idle-bus*)
        (bt:with-lock-held (*message-bus-lock*)
          (setf available-messages (nreverse (copy-list *message-bus-queue*))) ; reverse takes a copy
          (setf *message-bus-queue* nil))
        (if available-messages
            (progn
              (when *log-delta-messenger-message-bus-processing*
                (format t "~&[INFO][DELTA-BUS] Delta messenger will process ~A messages~%"
                        (length available-messages)))
              (dolist (message available-messages)
                (apply *message-bus-consumer* message)))
            (when *log-delta-messenger-message-bus-processing*
              (format t "~&[INFO][DELTA-BUS] Delta messenger woke without available message.~%"))))
    (error (e) (format t "~&Error occurred on messenger message bus!~% ~A~%; waiting 3s and continuing next cycle, ignoring current messages~&" e)))
  (message-bus-loop))

(defun launch-message-consumer (consuming-function)
  "launches a message consumer"
  (setf *message-bus-consumer* consuming-function)
  (bt:make-thread #'message-bus-loop :name "message-bus"))

(defun schedule-delta-message (&rest messages)
  "schedules messages for the notifier system."
  (bt:with-lock-held (*message-bus-lock*)
    (setf *message-bus-queue*
          (nconc (reverse messages) *message-bus-queue*))
    (unless (> (sb-thread:semaphore-count *message-bus-semaphore*) 0)
      ;; the consuming thread might already be awake and not have the
      ;; lock which means we'd still wake the consuming thread twice
      ;; even without delta messages being available.  That is not a big
      ;; problem, it will just be an extra empty loop on that end.
      (sb-thread:signal-semaphore *message-bus-semaphore*))))
