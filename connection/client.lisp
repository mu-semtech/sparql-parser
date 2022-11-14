(in-package #:client)

(defparameter *backend* "http://localhost:8890/sparql")

(defun query (string)
  "Sends a query to the backend and responds with the response body."
  (multiple-value-bind (body code headers)
      (dex:request *backend*
                   :method :post
                   :content string
                   :use-connection-pool nil
                   :keep-alive nil
                   :force-string t
                   :headers `(("content-type" . "application/sparql-update")
                              ("accept" . "application/sparql-results+json")
                              ("mu-call-id" . ,(mu-call-id))
                              ("mu-session-id" . ,(mu-session-id))))
    (declare (ignore code headers))
    body))
