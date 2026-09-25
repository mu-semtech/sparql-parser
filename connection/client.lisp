(in-package #:client)

(defparameter *backend*
  (if (find :docker *features*)
      "http://triplestore:8890/sparql"
      "http://localhost:8891/sparql")
  "Backends to talk to.  If this is a list, ")

(defparameter *backends*
  nil
  "If this special variable is set, it will contain objects representing the current backends.  It will replace *backend* over time.")

(defparameter *backend-type* :virtuoso
  "Type of the triplestore backing this instance.  Supported values are :VIRTUOSO (the default) and :QLEVER.

QLever executes SPARQL update queries (INSERT/DELETE) through raw update
requests rather than through the query parameter used by Virtuoso.  It also
requires an access token for updates, see `*QLEVER-ACCESS-TOKEN*'.")

(defparameter *qlever-access-token* "dba"
  "Access token sent in the authorization header when executing SPARQL update queries against a QLever backend.

QLever rejects update queries without a valid access token.")

(defparameter *max-concurrent-connections* 8
  "The maximum amount of concurrent queries sent to a sparql endpoint.")

(defparameter *log-sparql-query-roundtrip* nil
  "When set to non-nil, we log both the outgoing query sent to and response received from the sparql endpoint.")

(defparameter *acquire-db-semaphore-timeout* 55
  "Amount of time (in seconds) to wait to acquire the semaphore (default is now 55).

NIL symbolizes to wait forever.")

;; Overriding the default values here for now.  Ideally these get calculated based on how much time we have left.
(setf dexador.util:*default-connect-timeout* 60)
(setf dexador.util:*default-read-timeout* 60)

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

;; begin max query time
(defparameter *max-query-time-for-retries* 10
  "This is the max amount of time to spend within which we'll retry to send the query.")

(defparameter *max-query-time-for-retries-in-followup-queries* 50
  "This is the max amount of time to spend within which we'll retry to send the query once we've already sent the first query.")

(defparameter *has-increased-max-query-time-for-retries* nil
  "Set to t in `WITH-INCREASED-MAX-QUERY-TIME-FOR-RETRIES' to automatically increase the time after the first request.")

(defmacro with-increased-max-query-time-for-retries (&body body)
  "First query in body has `*max-query-time-for-retries*' and followup get `*max-query-time-for-retries-in-followup-queries*'."
  `(let ((*max-query-time-for-retries* *max-query-time-for-retries*)
         (*has-increased-max-query-time-for-retries* t))
     ,@body))

(defun complete-query-for-max-query-time-retries ()
  "Called when completing a query.
  When in `with-increased-max-query-time-for-retries' macro, will set `*max-query-time-for-retries*' to
  `*max-query-time-for-retries-in-followup-queries*'."
  (when *has-increased-max-query-time-for-retries*
    (setf *max-query-time-for-retries* *max-query-time-for-retries-in-followup-queries*)))
;; end max query time

(defparameter *log-failing-query-tries* t
  "Whether to log queries which fail in exponential backoff retry.")

(defparameter *log-failing-query-tries-with-condition* t
  "Whether to log the condition for queries which fail in exponential backoff retry.")

(defun send-query-to-triplestore (endpoint query headers)
  "Sends QUERY as query to the triplestore at url ENDPOINT with headers HEADERS.

Yields what `DEX:REQUEST' yields, the first three being BODY CODE HEADERS."
  (let ((endpoint-uri (quri:uri endpoint)))
    (if (< (length query) 1000) ;; resources guesses 5k, we guess 1k for Virtuoso
        (progn
          (setf (quri:uri-query-params endpoint-uri)
                `(("query" . ,query)))
          (dex:request endpoint-uri
                       :method :get
                       :use-connection-pool t
                       :keep-alive t
                       :force-string t
                       ;; :verbose t
                       :headers headers))
        (dex:request endpoint-uri
                     :method :post
                     :use-connection-pool nil
                     :keep-alive nil
                     :force-string t
                     :headers headers
                     :content `(("query" . ,query))))))

(defun qlever-backend-p ()
  "Truethy iff the backing triplestore is a QLever instance."
  (eq *backend-type* :qlever))

(defun accept-header ()
  "Accept header sent along with outgoing queries.

QLever does not support serialising CONSTRUCT query results as JSON
and responds with a turtle-family format instead.  For a QLever
backend we request N-Triples as additional accepted format so we
receive a predictable serialisation we can convert, see
`CONSTRUCT-RESULT-AS-JSON'."
  (if (qlever-backend-p)
      "application/sparql-results+json, application/n-triples"
      "application/sparql-results+json"))

(defun qlever-authorization-headers ()
  "Headers needed to authorize against a QLever backend.

Yields an empty list if no access token is configured."
  (when *qlever-access-token*
    `(("authorization" . ,(format nil "Bearer ~A" *qlever-access-token*)))))

(defun send-update-to-triplestore (endpoint update headers)
  "Sends UPDATE as a SPARQL update request to the triplestore at url ENDPOINT with headers HEADERS.

QLever expects SPARQL updates to be sent as a raw body via POST with
content-type application/sparql-update, rather than as a query parameter.
The authorization headers reported by `QLEVER-AUTHORIZATION-HEADERS' are
added as QLever requires an access token for updates.

Yields what `DEX:REQUEST' yields, the first three being BODY CODE HEADERS."
  (dex:request endpoint
               :method :post
               :use-connection-pool nil
               :keep-alive nil
               :force-string t
               :headers (append (qlever-authorization-headers)
                                '(("content-type" . "application/sparql-update"))
                                headers)
               :content update))

;; begin CONSTRUCT conversion for QLever

(defun turtle-family-content-type-p (content-type)
  "Truethy iff CONTENT-TYPE indicates a turtle-family response.

QLever responds to CONSTRUCT queries with turtle or N-Triples rather
than with the JSON responses Virtuoso yields."
  (and content-type
       (cl-ppcre:scan "^(text/turtle|application/n-triples)" content-type)))

(defun decode-turtle-escapes (string)
  "Decodes the ECHAR escape sequences in STRING.

The ttl parser keeps escaped characters like quotes and backslashes in
their escaped form.  Virtuoso reports the unescaped values, hence we
decode them to yield equivalent results."
  (cl-ppcre:regex-replace-all
   "\\\\([tbnrf\"'\\\\])"
   string
   (lambda (string start end match-start match-end reg-starts reg-ends)
     (declare (ignore start end match-start match-end reg-ends))
     (string (case (char string (aref reg-starts 0))
               (#\t #\Tab)
               (#\b (code-char 8))
               (#\n #\Newline)
               (#\r #\Return)
               (#\f (code-char 12))
               (#\" #\")
               (#\' #\')
               (#\\ #\\))))))

(defun construct-term-as-binding (term)
  "Converts a TERM of a parsed turtle triple into its JSON binding value.

Mimics the values Virtuoso yields for CONSTRUCT queries: URIs are
reported as \"uri\", blank nodes as \"bnode\", plain and language
tagged literals as \"literal\" and typed literals as
\"typed-literal\".  Language tags are reported under the \"lang\" key
as Virtuoso does for CONSTRUCT queries."
  (cond
    ((quri:uri-p term)
     (jsown:new-js ("type" "uri") ("value" (quri:render-uri term))))
    ((cl-ttl-parser:blank-node-p term)
     (jsown:new-js ("type" "bnode") ("value" (cl-ttl-parser:blank-node-label term))))
    ((cl-ttl-parser:rdf-literal-p term)
     (let* ((lang (cl-ttl-parser:rdf-literal-lang term))
            (datatype (cl-ttl-parser:rdf-literal-datatype term))
            (datatype-string (when datatype (quri:render-uri datatype)))
            ;; a plain literal and an xsd:string literal are the same
            ;; thing in RDF 1.1, Virtuoso reports both as plain literal
            (plain-p (or (not datatype)
                         (string= datatype-string "http://www.w3.org/2001/XMLSchema#string")))
            (binding (jsown:new-js
                       ("type" (if (and datatype (not plain-p)) "typed-literal" "literal"))
                       ("value" (decode-turtle-escapes (cl-ttl-parser:rdf-literal-value term))))))
       (when lang
         (setf (jsown:val binding "lang") lang))
       (when (and datatype (not plain-p))
         (setf (jsown:val binding "datatype") datatype-string))
       binding))
    (t (error "Cannot convert ~A into a CONSTRUCT binding" term))))

(defun construct-result-as-json (body)
  "Converts the turtle-family BODY of a QLever CONSTRUCT response into the sparql-results+json format Virtuoso yields.

QLever does not support serialising CONSTRUCT query results as JSON,
so we parse the N-Triples response and construct the JSON bindings
ourselves.  The result contains one binding per triple, reporting the
subject under the \"s\" key, the predicate under \"p\" and the object
under \"o\"."
  (let ((bindings
          (loop for (subject predicate object) in (cl-ttl-parser:parse-ttl body)
                collect (jsown:new-js
                          ("s" (construct-term-as-binding subject))
                          ("p" (construct-term-as-binding predicate))
                          ("o" (construct-term-as-binding object))))))
    (jsown:to-json
     (jsown:new-js
       ("head" (jsown:new-js ("link" '()) ("vars" '("s" "p" "o"))))
       ("results" (jsown:new-js ("distinct" :false)
                                ("ordered" :true)
                                ("bindings" bindings)))))))

(defun maybe-construct-result-as-json (body headers)
  "Converts the response BODY to the JSON bindings format if it is a turtle-family CONSTRUCT result from a QLever backend.

Other responses are returned as is."
  (if (and (qlever-backend-p)
           headers
           (turtle-family-content-type-p (gethash "content-type" headers)))
      (construct-result-as-json body)
      body))

;; end CONSTRUCT conversion for QLever

(defun ensure-backends-variable ()
  "Users can set the backends using the `*BACKEND*' variable in simple string form.  We now have a more complex structure
which is stored in the `*BACKENDS*' variable.  This function handles the upgrade from one format to the other."
  (unless *backends*
    (if (listp *backend*)
        (apply #'set-backend-urls *backend*)
        (set-backend-urls *backend*))))

(defun ensure-endpoints-available (&key (verbose nil))
  "Waits for all endpoints to become available and for them to respond to at least one query.

When the VERBOSE keyword is truethy, output is written to STDOUT."
  (ensure-backends-variable)
  (when verbose
    (format t "~&Ensuring ~A backends are up: ~{~%- ~A~}~%"
            (length *backends*)
            (mapcar #'sparql-endpoint-url *backends*)))
  (loop for backend in *backends*
        for url = (sparql-endpoint-url backend)
        do
           (loop until (handler-case
                           (progn
                             (jsown:val
                              (jsown:parse
                               (send-query-to-triplestore url
                                                          "ASK { GRAPH ?g { ?s ?p ?o. } }"
                                                          '(("accept" . "application/sparql-results+json"))))
                              "boolean")
                             (when verbose
                               (format t "~&Endpoint ~A is up~%" url))
                             t)
                         (error (e)
                           (when verbose
                             (format t "~&Could not access endpoint ~A, signaled ~A, will retry.~%" url e))
                           (sleep 1))))))

(defun query (string &key (send-to-single nil) (update-p nil))
  "Sends a query to the backend and responds with the response body.

When SEND-TO-SINGLE is truethy and multiple endpoints are available, the request is sent to only one of them.

When UPDATE-P is truethy, the request is treated as a SPARQL update query
(INSERT/DELETE).  For a QLever backend the update is then sent as a raw
body via POST with content-type application/sparql-update as expected by
QLever.  Other backends receive updates the same way as regular queries."
  (ensure-backends-variable)
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
    (support:with-multiple-semaphores ((mapcar #'sparql-endpoint-semaphore selected-endpoints) :timeout *acquire-db-semaphore-timeout*)
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
                         (let ((headers `(("accept" . ,(accept-header))

                                          ("mu-call-id" . ,(mu-call-id))
                                          ("mu-session-id" . ,(mu-session-id)))))
                           (if (and update-p (qlever-backend-p))
                               (send-update-to-triplestore (sparql-endpoint-url endpoint) string headers)
                               (send-query-to-triplestore (sparql-endpoint-url endpoint) string headers)))
                       (declare (ignore code))
                       (when *log-sparql-query-roundtrip*
                         (format t "~&Requested:~%~A~%and received~%~A~%"
                                 string body))
                       ;; QLever responds to CONSTRUCT queries with a
                       ;; turtle-family format which we convert to the
                       ;; JSON bindings Virtuoso yields.
                       (setf result (maybe-construct-result-as-json body headers)))
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
    (complete-query-for-max-query-time-retries)
    result))

(defun clean-up-sparql-results (sparql-results)
  "Cleans sparql results based on parsing or triplestore issues.

Currently translates jsown's ratio's to floats."
  (let ((bindings (jsown:val-safe (jsown:val-safe sparql-results "results")
                                  "bindings")))
    (dolist (variable-bindings bindings)
      (jsown:do-json-keys (binding-key variable-binding) variable-bindings
        (let ((value (jsown:val variable-binding "value")))
          (when (typep value 'number)
            (setf (jsown:val variable-binding "value")
                  (typecase value
                    (ratio (format nil "~G"  (coerce value 'float)))
                    (float (format nil "~G"  (coerce value 'float)))
                    (otherwise (write-to-string value :readably nil)))))))))
  sparql-results)

(defun expand-bindings (bindings &key virtuoso-p construct-p)
  "Expands bindings for URIs which actually represent a string.  May modify bindings in place.

VIRTUOSO-P and CONSTRUCT-P are only taken into account for cases where the a URI is expanded to its literal value.  This may change in the future.

CONSTRUCT-P indicates the expansion should happen for a Virtuoso CONSTRUCT query which uses different json key for lang
and a different value for type."
  (loop for binding in bindings
        do
           (jsown:do-json-keys (key val) binding
             (when (string= (jsown:val val "type") "uri")
               (multiple-value-bind (string-replacement string-replacement-p lang datatype)
                   (support:maybe-uri-to-string (jsown:val val "value"))
                 (when string-replacement-p
                   (let ((new-jsown-object
                           (jsown:new-js
                             ("value" string-replacement)
                             ("type" (if (and virtuoso-p (not construct-p) datatype)
                                        "typed-literal"
                                        "literal")))))
                     (when lang
                       (let ((key (if (and virtuoso-p construct-p)
                                      "lang"
                                      "xml:lang")))
                         (setf (jsown:val new-jsown-object key) lang)))
                     (when datatype
                       (setf (jsown:val new-jsown-object "datatype") datatype))
                     (setf (jsown:val binding key)
                           new-jsown-object)))))))
  bindings)

(defun standardize-construct-bindings (bindings)
  "Destructively converts bindings into a more standardized form.

Could be integrated into expand-bindings but this could just provide a standard form.

Virtuoso yields various forms depending on how you ask it something and it's not always the same as the SPARQL
standard (which may not exist).  This function converts the forms we know of today to forms which closely match the
SPARQL SELECT standard.  Rules may be extended as we encounter more forms."
  ;; We only care about the object portion because the select part seems to be right at all times.

  ;; We may expand into multiple forms even when it's not intended.
  (dolist (binding bindings)
    ;; cases
    ;; 1. key xml:lang vs lang (cast to "xml:lang")
    ;; 2. type typed-literal vs literal (cast to "uri" or "literal")
    ;; 3. number value being a raw value instead of a string
    (let ((object (jsown:val binding "o")))
      (when (jsown:keyp object "lang")
        (setf (jsown:val object "xml:lang")
                    (jsown:val object "lang"))
        (jsown:remkey object "lang"))
      (when (string= (jsown:val object "type")
                     "typed-literal")
        (setf (jsown:val object "type") "literal"))
      (unless (stringp (jsown:val object "value"))
        (setf (jsown:val object "value")
              (format nil "~A" (jsown:val object "value"))))))
  bindings)

(defun bindings (query-result &key (convert-string-uris t) virtuoso-p construct-p)
  "Converts the string representation of the SPARQL query result into a set
of JSOWN compatible BINDINGS.

If CONVERT-STRING-URIS is truethy, any URI which actually represents a
string, will be expanded into its string representation for further
comparison."
  ;; TODO: introduce a database error type and expand it if the result was nil?
  (let ((bindings (jsown:filter (clean-up-sparql-results
                                 (jsown:parse query-result))
                                "results" "bindings")))
    (if convert-string-uris
        (expand-bindings bindings :virtuoso-p virtuoso-p :construct-p construct-p)
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

(defun batch-create-full-solution-for-construct-query (query &rest args &key (for :read) batch-size usage)
  "Executes a SPARQL CONSTRUCT query, like `BATCH-CREATE-FULL-SOLUTION-FOR-SELECT-QUERY'."
  (declare (ignore for batch-size usage))
  (apply #'batch-create-full-solution-for-select-query query args))

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
