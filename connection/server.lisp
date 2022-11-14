(in-package #:server)

(defparameter *woo-env* nil)

(defun extract-query-string (env content-type)
  (let ((arr (make-array (getf env :content-length) :element-type 'flex:octet)))
    (read-sequence arr (getf env :raw-body))
    (let ((body-string (flex:octets-to-string arr)))
      (if (equal content-type "application/sparql-update")
          (coerce body-string 'base-string)
          (let ((params (quri:url-decode-params body-string)))
            (coerce (string-trim "\n \t" (cdr (or (assoc "query" params :test #'equal)
                                                  (assoc "update" params :test #'equal))))
                    'base-string))))))

(defun parse-query (query)
  (sparql-parser:parse-sparql-string query))

(defun manipulate-query (match)
  (sparql-manipulation::add-from-graphs
   (sparql-manipulation::remove-graph-graph-patterns
    (sparql-manipulation::remove-dataset-clauses match))
   (list "<http://mu.semte.ch/graphs/public>")))

(defun generate-query (match)
  (sparql-generator:is-valid match)
  (sparql-generator:write-valid match))

(defun acceptor (env)
  ;; (declare (ignore env))
  ;; '(200 (:content-type "application/sparql-results+json") ("HELLO HELLO HELLO"))
  (let ((sparql-parser::*stack* nil)
        (sparql-parser::*scanning-string* nil)
        (sparql-parser::*match-tree* nil)
        (sparql-parser::*current-token* nil)
        (sparql-parser::*next-char-idx* 0)
        (headers (getf env :headers)))
    (with-call-context (:mu-call-id (gethash "mu-call-id" headers)
                        :mu-session-id (gethash "mu-session-id" headers)
                        :mu-auth-allowed-groups (gethash "mu-auth-allowed-groups" headers))
      (let* ((query (extract-query-string env (gethash "content-type" headers)))
             (updated-query (funcall (alexandria:compose #'generate-query
                                                         #'manipulate-query
                                                         #'parse-query)
                                     query))
             (response (client::query updated-query)))
        `(200
          (:content-type "application/sparql-results+json" :mu-auth-allowed-groups ,(mu-auth-allowed-groups))
          (,response))))))

(defun boot (&key (port 8080) (worker-count 32))
  (bordeaux-threads:make-thread
   (lambda ()
     (woo:run (lambda (env) (acceptor env))
              :address "0.0.0.0"
              :port port
              :worker-num worker-count))))
