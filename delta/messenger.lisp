(in-package :delta-messenger)

;;;; Delta messenger
;;;;
;;;; We started modelling this as a class based system because we
;;;; thought some flexibility would be nice.  Whilst writing this we
;;;; realized this is likely a non-feature.  The code written here does
;;;; not actively help in making this more configurable through
;;;; environment variables and a direct implementation would likely be
;;;; cleaner.

(defparameter *delta-handlers* nil
  "Handlers for the delta messages.")

(defclass delta-handler () ()
  (:documentation "Superclass of all delta handlers."))

(defclass delta-logging-handler (delta-handler) ()
  (:documentation "Logs delta messages to standard output."))

(defclass delta-remote-handler (delta-handler)
  ((endpoint :initarg :endpoint :initform (error "Must supply target for delta post handler."))
   (method :initarg :method :initform :post))
  (:documentation "Sends a  delta post to the given endpoint."))

(defgeneric handle-delta (handler &key inserts deletes effective-inserts effective-deletes)
  (:documentation "Handles a delta message for the raw insert and delete quads.  This may include sending it out to an external provider.")
  (:method ((handler delta-logging-handler) &key inserts deletes effective-inserts effective-deletes sequence-id)
    (format t "~&Notify others on id ~A quads having been written from ~A:~% Deleted Quads: ~{~%  ~A~}~% Inserted Quads: ~{~%  ~A~}~% Effectively Deleted Quads: ~{~%  ~A~}~% Effectively Inserted Quads: ~{~%  ~A~}"
            sequence-id
            (connection-globals:source-ip)
            (mapcar (alexandria:compose #'jsown:to-json #'quad-to-jsown-binding) deletes)
            (mapcar (alexandria:compose #'jsown:to-json #'quad-to-jsown-binding) inserts)
            (mapcar (alexandria:compose #'jsown:to-json #'quad-to-jsown-binding) effective-deletes)
            (mapcar (alexandria:compose #'jsown:to-json #'quad-to-jsown-binding) effective-inserts)))
  (:method ((handler delta-remote-handler) &key sequence-id inserts deletes effective-inserts effective-deletes)
    (when (or inserts deletes)
      (let ((delta-message (jsown:to-json
                            (jsown:new-js
                              ("changeSets"
                               (list
                                (delta-to-jsown :deletes deletes
                                                :inserts inserts
                                                :effective-deletes effective-deletes
                                                :effective-inserts effective-inserts
                                                :scope (connection-globals:mu-call-scope)
                                                :allowed-groups (if (connection-globals:mu-auth-sudo)
                                                                    "sudo"
                                                                    (connection-globals:mu-auth-allowed-groups))
                                                :source-ip (connection-globals:source-ip)
                                                :sequence-id sequence-id))))))
            (headers `(("content-type" . "application/json")
                       ("mu-call-id" . ,(connection-globals:mu-call-id))
                       ("mu-session-id" . ,(connection-globals:mu-session-id)))))
        (when (connection-globals:mu-call-id-trail)
          (push (cons "mu-call-id-trail" (connection-globals:mu-call-id-trail))
                headers))
        (when (connection-globals:mu-auth-allowed-groups)
          (push (cons "mu-auth-allowed-groups" (jsown:to-json (connection-globals:mu-auth-allowed-groups)))
                headers))
        (schedule-delta-message (list handler delta-message headers))))))

(defun execute-scheduled-remote-delta-message (delta-remote-handler json-delta-message headers)
  (support:with-exponential-backoff-retry
      (:max-time-spent 600 :max-retries 20 :initial-pause-interval 1 :pause-interval-multiplier 1.5)
    (handler-case
        ;; TODO: share following headers for this request with the new request
        ;;   - mu-auth-sudo (or make that influence mu-auth-allowed-groups?)
        (with-slots (endpoint method) delta-remote-handler
          (multiple-value-bind (body status resp-headers uri socket)
              (dex:request endpoint
                           :method method
                           :headers headers
                           :content json-delta-message)
            (declare (ignore body resp-headers uri socket))
            (unless (<= 200 status 299)
              (format t "~&Discovered incorrect status code ~A~%" status)
              (error 'simple-error
                     "Received status code ~A from delta messenger which is not in 2xx range"
                     status))))
      (FAST-HTTP.ERROR:CB-MESSAGE-COMPLETE (e)
        (format t
                "~&Encountered error from FAST-HTTP during delta: ~A~&~@[Delta message leading to failure: ~A~&~]"
                e json-delta-message)
        (support:report-exponential-backoff-failure e))
      (error (e)
        (format t
                "~&Encountered general error when sending delta: ~A~&~@[Delta leading to failure: ~A~&~]"
                e json-delta-message)
        (support:report-exponential-backoff-failure e)))))

(defun quad-to-jsown-binding (quad)
  "Converts QUAD to a jsown binding."
  (jsown:new-js
    ("subject" (handle-update-unit::match-as-binding (getf quad :subject)))
    ("predicate" (handle-update-unit::match-as-binding (getf quad :predicate)))
    ("object" (handle-update-unit::match-as-binding (getf quad :object)))
    ("graph" (handle-update-unit::match-as-binding (getf quad :graph)))))

(defun delta-to-jsown (&key inserts deletes effective-inserts effective-deletes scope allowed-groups source-ip sequence-id)
  "Convert delta inserts and deletes message to jsown body for inserts and deletes."
  (let ((delta
          (jsown:new-js
            ("insert" (mapcar #'quad-to-jsown-binding inserts))
            ("delete" (mapcar #'quad-to-jsown-binding deletes))
            ("effectiveInsert" (mapcar #'quad-to-jsown-binding effective-inserts))
            ("effectiveDelete" (mapcar #'quad-to-jsown-binding effective-deletes))
            ("sequenceId" sequence-id)
            ("origin" source-ip)))) ; source ip is shared in origin key
    (when allowed-groups
      (setf (jsown:val delta "allowedGroups") (if (equal allowed-groups "sudo")
                                                  "sudo"
                                                  (jsown:to-json allowed-groups))))
    (when (and scope (not (eq scope acl:_)))
      (setf (jsown:val delta "scope") scope))
    delta))

(defun delta-notify (&key deletes inserts effective-deletes effective-inserts sequence-id)
  "Entrypoint of the delta messenger.  Dispatches messages to all relevant places."
  (mapcar (alexandria:rcurry #'handle-delta
                             :sequence-id sequence-id
                             :deletes deletes
                             :inserts inserts
                             :effective-deletes effective-deletes
                             :effective-inserts effective-inserts)
          *delta-handlers*))

(defun add-delta-messenger (target &key (method :post))
  "Adds a new delta messenger to the list of targets."
  (push (make-instance 'delta-remote-handler :endpoint target :method method)
        *delta-handlers*))

(defun add-delta-logger ()
  "Logs delta messages to the terminal."
  (push (make-instance 'delta-logging-handler) *delta-handlers*))

(launch-message-consumer #'execute-scheduled-remote-delta-message)

;; (push (make-instance 'delta-logging-handler) *delta-handlers*)
;; (add-delta-messenger "http://localhost:8089")
