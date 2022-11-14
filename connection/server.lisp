(in-package #:server)

(defun extract-query-string (env content-type)
  "Extracts query string from the request when content-type is given."
  (let ((arr (make-array (getf env :content-length) :element-type 'flex:octet)))
    (read-sequence arr (getf env :raw-body))
    (let ((body-string (flex:octets-to-string arr)))
      (if (equal content-type "application/sparql-update")
          (coerce body-string 'base-string)
          (let ((params (quri:url-decode-params body-string)))
            (coerce (string-trim "\n \t" (cdr (or (assoc "query" params :test #'equal)
                                                  (assoc "update" params :test #'equal))))
                    'base-string))))))

(defun manipulate-query (match)
  "Manipulates the requested query for current access rights."
  (-> match
    (remove-dataset-clauses)
    (remove-graph-graph-patterns)
    (add-from-graphs (list "<http://mu.semte.ch/graphs/public>"))))

(defun generate-query (match)
  "Generates the query string from the updated match."
  (if (sparql-generator:is-valid match)
      (sparql-generator:write-valid match)
      (error "Match is invalid ~A" match)))

(defun acceptor (env)
  ;; (declare (ignore env))
  ;; '(200 (:content-type "application/sparql-results+json") ("HELLO HELLO HELLO"))
  (let ((headers (getf env :headers)))
    (with-call-context (:mu-call-id (gethash "mu-call-id" headers)
                        :mu-session-id (gethash "mu-session-id" headers)
                        :mu-auth-allowed-groups (gethash "mu-auth-allowed-groups" headers))
      (with-parser-setup
        (let ((response
                (client::query
                 (generate-query
                  (manipulate-query
                   (parse-sparql-string (extract-query-string env (gethash "content-type" headers))))))))
          `(200
            (:content-type "application/sparql-results+json" :mu-auth-allowed-groups ,(mu-auth-allowed-groups))
            (,response)))))))

(defun boot (&key (port 8080) (worker-count 32))
  (bordeaux-threads:make-thread
   (lambda ()
     (woo:run (lambda (env) (acceptor env))
              :address "0.0.0.0"
              :port port
              :worker-num worker-count))))
