(in-package #:client)

(defparameter *backend*
  (if (find :docker *features*)
      "http://triplestore:8890/sparql"
      "http://localhost:8891/sparql"))

(defparameter *log-sparql-query-roundtrip* nil)

(defun query (string &key (send-to-single nil))
  "Sends a query to the backend and responds with the response body.

When SEND-TO-SINGLE is truethy and multple endpoints are available, the request is sent to only one of them."
  (let ((endpoints
          (if (listp *backend*)
              (if send-to-single
                  (list (alexandria:random-elt *backend*))
                  *backend*)
              (list *backend*)))
        result)
    (loop for endpoint in endpoints
          do
             (support:with-exponential-backoff-retry
                 (:max-time-spent 60 :max-retries 10 :initial-pause-interval 1 :pause-interval-multiplier 2)
               (handler-case
                   (multiple-value-bind (body code headers)
                       (let ((uri (quri:uri endpoint))
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
                   (format t
                           "~&Encountered error from FAST-HTTP: ~A~&~@[Query leading to failure: ~A~&~]"
                           e (when *log-sparql-query-roundtrip* string))
                   (support:report-exponential-backoff-failure e))
                 (error (e)
                   (format t
                           "~&Encountered general error when executing query: ~A~&~@[Query leading to failure: ~A~&~]"
                           e (when *log-sparql-query-roundtrip* string))
                   (support:report-exponential-backoff-failure e)))))
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
      (client:bindings (client:query query-string)))))

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
      (client:bindings (client:query query-string)))))

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
