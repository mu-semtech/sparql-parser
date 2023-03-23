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
  (:documentation "Logs deltaa messages to standard output."))

(defclass delta-remote-handler (delta-handler)
  ((endpoint :initarg :endpoint :initform (error "Must supply target for delta post handler."))
   (method :initarg :method :initform :post))
  (:documentation "Sends a  delta post to the given endpoint."))

(defgeneric handle-delta (handler &key inserts deletes)
  (:documentation "Handles a delta message for the raw insert and delete quads.  This may include sending it out to an external provider.")
  (:method ((handler delta-logging-handler) &key inserts deletes)
    (format t "~&Notify others on quads having been written:~% Inserted Quads: ~A~% Deleted Quads: ~A~%"
            inserts deletes))
  (:method ((handler delta-remote-handler) &key inserts deletes)
    (flet ((quad-to-jsown-binding (quad)
             (jsown:new-js
               ("subject" (handle-update-unit::match-as-binding (getf quad :subject)))
               ("predicate" (handle-update-unit::match-as-binding (getf quad :predicate)))
               ("object" (handle-update-unit::match-as-binding (getf quad :object))))))
      ;; TODO: share following headers for this request with the new request
      ;;   - mu-auth-allowed-groups
      ;;   - mu-session-id
      ;;   - mu-call-id (does this need to be shadowed here?)
      (when (or inserts deletes)
        (with-slots (endpoint method) handler
         (dex:request endpoint
                      :method method
                      :content (format nil "{ \"inserts\": ~A, \"deletes\": ~A }"
                                       (mapcar (alexandria:compose #'jsown:to-json #'quad-to-jsown-binding) inserts)
                                       (mapcar (alexandria:compose #'jsown:to-json #'quad-to-jsown-binding) deletes))))))))

(defun delta-notify (&key inserts deletes)
  "Entrypoint of the delta messenger.  Dispatches messages to all relevant places."
  (mapcar (alexandria:rcurry #'handle-delta :inserts inserts :deletes deletes)
          *delta-handlers*))

(defun add-delta-messenger (target &key (method :post))
  "Adds a new delta messenger to the list of targets."
  (push (make-instance 'delta-remote-handler :endpoint target :method method)
        *delta-handlers*))

(push (make-instance 'delta-logging-handler) *delta-handlers*)
