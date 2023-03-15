(in-package #:server)

(defparameter *request-counter-lock*
  (bt:make-lock "REQUEST-COUNTER"))

(defparameter *request-count* 0)

(defun extract-query-string (env content-type)
  "Extracts query string from the request when content-type is given."
  (if (eq (getf env :request-method) :post)
      ;; post should have query in query body or in form body or form update
      (let ((arr (make-array (getf env :content-length) :element-type 'flex:octet)))
        (read-sequence arr (getf env :raw-body))
        (let ((body-string (flex:octets-to-string arr)))
          (if (equal content-type "application/sparql-update")
              (coerce body-string 'base-string)
              (let ((params (quri:url-decode-params body-string)))
                (coerce (string-trim "\n \t" (cdr (or (assoc "query" params :test #'equal)
                                                      (assoc "update" params :test #'equal))))
                        'base-string)))))
      ;; get should have query in query parameter
      (when-let ((query-assoc (assoc "query" (quri:url-decode-params (getf env :query-string ""))
                                     :test #'string=)))
        (coerce (cdr query-assoc) 'base-string))))

(defun manipulate-query (ast)
  "Manipulates the requested query for current access rights."
  (acl:apply-access-rights ast))

(defun generate-query (match)
  "Generates the query string from the updated match."
  (if (sparql-generator:is-valid match)
      (sparql-generator:write-valid match)
      (error "Match is invalid ~A" match)))

(defun execute-query-for-context (query)
  "Executes the string QUERY in the current context, applying al relevant access rights and shipping off the query."
  (with-parser-setup
    (let ((ast (parse-sparql-string (coerce query 'base-string))))
      (if (sparql-parser:match-term-p (sparql-parser:sparql-ast-top-node ast) 'ebnf::|UpdateUnit|)
          ;; update
          (let ((detect-quads::*info* (detect-quads::make-info)))
            (handle-update-unit::handle-sparql-update-unit
             (sparql-parser:sparql-ast-top-node ast)))
          ;; query
          (client::query
           (generate-query
            (manipulate-query ast)))))))

(defun acceptor (env)
  ;; (declare (ignore env))
  ;; '(200 (:content-type "application/sparql-results+json") ("HELLO HELLO HELLO"))
  (bt:with-lock-held (*request-counter-lock*)
    (incf *request-count*))
  (let ((headers (getf env :headers)))
    (with-call-context (:mu-call-id (gethash "mu-call-id" headers)
                        :mu-session-id (gethash "mu-session-id" headers)
                        :mu-auth-allowed-groups (gethash "mu-auth-allowed-groups" headers)
                        :mu-call-scope (gethash "mu-call-scope" headers))
      (with-parser-setup
        (let* ((query-string (extract-query-string env (gethash "content-type" headers)))
               (response (execute-query-for-context query-string)))
          `(200
            (:content-type "application/sparql-results+json" :mu-auth-allowed-groups ,(mu-auth-allowed-groups))
            (,response)))))))

(defun boot (&key (port 8890) (worker-count 32))
  (bordeaux-threads:make-thread
   (lambda ()
     (woo:run (lambda (env) (acceptor env))
              :address "0.0.0.0"
              :port port
              :worker-num worker-count))))
