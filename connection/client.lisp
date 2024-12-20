(in-package #:client)

(defparameter *backend*
  (if (find :docker *features*)
      "http://triplestore:8890/sparql"
      "http://localhost:8891/sparql")
  "Backends to talk to.  If this is a list, ")

(defparameter *backends*
  nil
  "If this special variable is set, it will contain objects representing the current backends.  It will replace *backend* over time.")

(defparameter *max-concurrent-connections* 8
  "The maximum amount of concurrent queries sent to a sparql endpoint.")

(defparameter *log-sparql-query-roundtrip* nil)

(defparameter *aquire-db-semaphore-timeout* 55
  "Amount of time (in seconds) to wait to aquire the semaphore (default is now 55).

NIL symolizes to wait forever.")

(defstruct sparql-endpoint
  "Descriptor struct for a SPARQL endpoint to execute max connections."
  (url "http://triplestore:8890/sparql" :type string)
  (semaphore (bt:make-semaphore :name (format nil "endpoint-semaphore")
                                :count *max-concurrent-connections*)))

(defun make-backend-structs (backend-endpoints)
  "Returns the backend structs for `BACKEND-ENDPOINTS'.

This function may have a race condition making it return a different
list of backends.  Assuming setf is atomic, the impact should be local
to the first queries so we consider this to be fine."
  (loop for endpoint in backend-endpoints
        collect
        (make-sparql-endpoint :url endpoint)))

(defun backends ()
  "Retrieves the backends as a list."
  *backends*)

(defun (setf backends) (backends)
  (setf *backends* backends))

(defun set-backend-urls (&rest endpoint-urls)
  "Sets endpoints based on their URLs."
  (setf (backends) (make-backend-structs endpoint-urls)))

(defparameter *max-query-time-for-retries* 10
  "This is the max amount of time to spend within which we'll retry to send the query.")

(defparameter *log-failing-query-tries* t
  "Whether to log queries which fail in exponential backoff retry.")

(defparameter *log-failing-query-tries-with-condition* t
  "Whether to log the condition for queries which fail in exponential backoff retry.")

(defun query (string &key (send-to-single nil))
  "Sends a query to the backend and responds with the response body.

When SEND-TO-SINGLE is truethy and multple endpoints are available, the request is sent to only one of them."
  (unless *backends*
    (if (listp *backend*)
        (apply #'set-backend-urls *backend*)
        (set-backend-urls *backend*)))
  (let* ((selected-endpoints
           (if send-to-single
               (list (alexandria:random-elt (backends)))
               (backends)))
         (result nil))
    ;; 1. collect locks
    ;; NOTEs:
    ;; - we always get the semaphore locks in the same order which should ensure all of this is stable
    ;; - a single failing endpoint will bring the whole setup down in this implementation
    ;; - the implementation does not fire off the queries in parallel, we may want a thread per semaphore for that
    ;; - a full-fledged and parallel implementation likely means rewriting this whole logic and the construction of the sparql-endpoint struct
    (support:with-multiple-semaphores ((mapcar #'sparql-endpoint-semaphore selected-endpoints) :timeout *aquire-db-semaphore-timeout*)
      (let ((post-handler (lambda () nil))) ; overwritten with handler on error
        (unwind-protect
             (support:with-exponential-backoff-retry
                 (:max-time-spent *max-query-time-for-retries* :max-retries 10 :initial-pause-interval 0.5 :pause-interval-multiplier 2 :log *log-failing-query-tries* :log-condition *log-failing-query-tries-with-condition*)
               (dolist (endpoint selected-endpoints)
                 ;; if we took too long, we should ensure no one else is waiting
                 (when (> support:*total-time-spent* 5)
                   (setf post-handler #'woo.worker.utils:recommission)
                   (woo.worker.utils:decommission))
                 ;; 2. send out queries
                 (handler-case
                     (multiple-value-bind (body code headers)
                         (let ((uri (quri:uri (sparql-endpoint-url endpoint)))
                               (headers `(("accept" . "application/sparql-results+json")
                                          ("mu-call-id" . ,(mu-call-id))
                                          ("mu-session-id" . ,(mu-session-id)))))
                           (if (< (length string) 1000) ;; resources guesses 5k, we guess 1k for Virtuoso
                               (progn
                                 (setf (quri:uri-query-params uri)
                                       `(("query" . ,string)))
                                 (dex:request uri
                                              :method :get
                                              :use-connection-pool t
                                              :keep-alive t
                                              :force-string t
                                              ;; :verbose t
                                              :headers headers))
                               (dex:request uri
                                            :method :post
                                            :use-connection-pool nil
                                            :keep-alive nil
                                            :force-string t
                                            :headers headers
                                            :content `(("query" . ,string)))))
                       (declare (ignore code headers))
                       (when *log-sparql-query-roundtrip*
                         (format t "~&Requested:~%~A~%and received~%~A~%"
                                 string body))
                       (setf result body))
                   (FAST-HTTP.ERROR:CB-MESSAGE-COMPLETE (e)
                     ;; This should also be logged in exponential backoff retry so might be good enough to log there.
                     ;; (format t
                     ;;         "~&Encountered error from FAST-HTTP: ~A~&~@[Query leading to failure: ~A~&~]"
                     ;;         e (when *log-sparql-query-roundtrip* string))
                     (support:report-exponential-backoff-failure e))
                   (error (e)
                     ;; This should also be logged in exponential backoff retry so might be good enough to log there.
                     ;; (format t
                     ;;         "~&Encountered general error when executing query: ~A~&~@[Query leading to failure: ~A~&~]"
                     ;;         e (when *log-sparql-query-roundtrip* string))
                     (support:report-exponential-backoff-failure e)))))
          (funcall post-handler)))
      ;; 3. release locks
      )
    result))

(defun expand-bindings (bindings)
  "Expands bindings for URIs which actually represent a string.  May modify bindings in place."
  (loop for binding in bindings
        do
           (jsown:do-json-keys (key val) binding
             (when (string= (jsown:val val "type") "uri")
               (multiple-value-bind (string-replacement string-replacement-p)
                   (support:maybe-uri-to-string (jsown:val val "value"))
                 (when string-replacement-p
                   ;; TODO: support language typed strings and datatypes
                   (setf (jsown:val binding key)
                         (jsown:new-js
                           ("value" string-replacement)
                           ("type" "literal"))))))))
  bindings)

(defun bindings (query-result &key (convert-string-uris t))
  "Converts the string representation of the SPARQL query result into a set
of JSOWN compatible BINDINGS.

If CONVERT-STRING-URIS is truethy, any URI which actually represents a
string, will be expanded into its string representation for further
comparison."
  ;; TODO: introduce a database error type and expand it if the result was nil?
  (let ((bindings (jsown:filter (jsown:parse query-result)
                                "results" "bindings")))
    (if convert-string-uris
        (expand-bindings bindings)
        bindings)))

(defparameter *log-batch-mapping* nil
  "Set to t to warn on processes which want to execute batch mapping.  Batch mapping is not implemented yet and will process as one big query.")

(defun batch-map-solutions-for-select-query* (query &key (for :read) batch-size usage)
  (declare (ignore for batch-size))
  (sparql-parser:with-sparql-ast query
    (let* ((altered-query (if usage
                              (acl:apply-access-rights query :usage usage)
                              query))
           (query-string (sparql-generator:write-valid altered-query)))
      ;; (break "Batch mapping ~A" query-string)
      (when *log-batch-mapping*
        (format t "~&Batch mapping ~A~%" query-string))
      (client:bindings (client:query query-string :send-to-single t)))))

(defun batch-create-full-solution-for-select-query (query &key (for :read) batch-size usage)
  "Executes a sparql query, possibly batching the solutions and combining
them into a set of JSOWN compatible BINDINGS."
  (declare (ignore for batch-size))
  (sparql-parser:with-sparql-ast query
    (let* ((altered-query (if usage
                              (acl:apply-access-rights query :usage usage)
                              query))
           ;; TODO: We don't use write-when-valid here because in
           ;; practice the WHERE block comes from the MODIFY query which
           ;; has less expressivity and uses different terms.  Validate
           ;; this case higher up and provide an option here not to
           ;; verify further.
           (query-string (sparql-generator:write-valid altered-query)))
      (client:bindings (client:query query-string :send-to-single t)))))

(defmacro batch-map-solutions-for-select-query ((query &rest args &key for batch-size usage) (bindings) &body body)
  "Executes the given operation in batches.

  FOR can be used to identify a default batch size to be used as well as
  to calculate the batches requested from the server side through
  centralized configuration.

  BATCH-SIZE allows to override the amount of results fetched in one
  batch.

  Executes the query and returns a list of the results for each batch."
  (declare (ignore for batch-size usage))
  ;; TODO: move this file into a module about query execution.
  `(let ((,bindings (batch-map-solutions-for-select-query* ,query ,@args)))
     (list ,@body)))
