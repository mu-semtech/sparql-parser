(in-package #:client)

(defparameter *backend* "http://localhost:8890/sparql")

(defun query (string)
  "Sends a query to the backend and responds with the response body."
  (multiple-value-bind (body code headers)
      (let ((uri (quri:uri *backend*)))
        (setf (quri:uri-query-params uri)
              `(("query" . ,string)))
        (dex:request uri
                     :method :get
                     :use-connection-pool t
                     :keep-alive t
                     :force-string t
                     ;; :verbose t
                     :headers `(("accept" . "application/sparql-results+json")
                                ("mu-call-id" . ,(mu-call-id))
                                ("mu-session-id" . ,(mu-session-id)))))
    (declare (ignore code headers))
    body))

(defun bindings (query-result)
  "Converts the string representation of the SPARQL query result into a set
of JSOWN compatible BINDINGS."
  (jsown:filter (jsown:parse query-result)
                "results" "bindings"))
