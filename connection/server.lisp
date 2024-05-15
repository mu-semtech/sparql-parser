(in-package #:server)

(defparameter *request-counter-lock*
  (bt:make-lock "REQUEST-COUNTER"))

(defparameter *request-count* 0)

(defparameter *log-incoming-requests-p* nil
  "When set to non-nil, we log incoming requests and access rights for them.")

(defun extract-query-string (env content-type)
  "Extracts query string from the request when content-type is given."
  (if (eq (getf env :request-method) :post)
      ;; post should have query in query body or in form body or form update
      (let ((arr (make-array (getf env :content-length) :element-type 'flex:octet)))
        (read-sequence arr (getf env :raw-body))
        (let ((body-string (flex:octets-to-string arr)))
          (if (equal content-type "application/sparql-update")
              (coerce body-string #-be-cautious 'base-string #+be-cautious 'string)
              (let ((params (quri:url-decode-params body-string)))
                (coerce (string-trim (list #\Newline #\Space #\Tab #\Return)
                                     (cdr (or (assoc "query" params :test #'equal)
                                              (assoc "update" params :test #'equal))))
                        #-be-cautious 'base-string #+be-cautious 'string)))))
      ;; get should have query in query parameter
      (when-let ((query-assoc (assoc "query" (quri:url-decode-params (getf env :query-string ""))
                                     :test #'string=)))
        (coerce (string-trim (list #\Newline #\Space #\Tab #\Return) (cdr query-assoc)) #-be-cautious 'base-string #+be-cautious 'string))))

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
    (let ((ast (parse-sparql-string (coerce query #-be-cautious 'base-string #+be-cautious 'string))))
      (if (sparql-parser:match-term-p (sparql-parser:sparql-ast-top-node ast) 'ebnf::|UpdateUnit|)
          ;; update
          (let ((detect-quads::*info* (detect-quads::make-info))) ;; TODO: convert this setup into a macro and document it
            (handle-update-unit::handle-sparql-update-unit
             (sparql-parser:sparql-ast-top-node ast))
            ;; Following is subject to change.  Nothing should depend
            ;; on the resulting output, sending out a json body could
            ;; be assumed, might as well send something sensible.
            "{ \"head\": { \"link\": [], \"vars\": [\"callret-0\"] }, \"results\": { \"distinct\": false, \"ordered\": true, \"bindings\": [ { \"callret-0\": { \"type\": \"literal\", \"value\": \"Executed update query\" }} ] } }")
          ;; query
          (let ((jsown-result
                  (jsown:with-injective-reader
                    (jsown:parse
                     (client::query
                      (generate-query
                       (manipulate-query ast))
                      :send-to-single t)))))
            (when (jsown:keyp jsown-result "results")
              ;; expand bindings if they exist
              (setf (jsown:val (jsown:val jsown-result "results") "bindings")
                    (expand-bindings (jsown:filter jsown-result "results" "bindings"))))
            (jsown:to-json jsown-result))))))

(defun parse-mu-call-scope-header (header)
  "Parses the mu-call-scope header and converts it into the correct instance."
  (cond ((null header) acl:_)
        ((string= "" header) acl:_)
        (t header)))

(defun acceptor (env)
  ;; (declare (ignore env))
  ;; '(200 (:content-type "application/sparql-results+json") ("HELLO HELLO HELLO"))
  (bt:with-lock-held (*request-counter-lock*)
    (incf *request-count*))
  (handler-case
      (let ((headers (getf env :headers)))
        (with-call-context (:mu-call-id (gethash "mu-call-id" headers)
                            :mu-session-id (gethash "mu-session-id" headers)
                            :mu-auth-sudo (and (gethash "mu-auth-sudo" headers nil) t)
                            :mu-auth-allowed-groups (gethash "mu-auth-allowed-groups" headers)
                            :mu-call-id-trail (jsown:parse (gethash "mu-call-id-trail" headers "[]"))
                            :mu-call-scope (parse-mu-call-scope-header (gethash "mu-auth-scope" headers)))
          (with-parser-setup
            (let* ((query-string (let ((str (extract-query-string env (gethash "content-type" headers))))
                                   (when *log-incoming-requests-p*
                                     (format t "Requested query as string:~%~A~%With access rights:~{~A: ~A~&~}"
                                             str
                                             (list :mu-call-id (mu-call-id)
                                                   :mu-session-id (mu-session-id)
                                                   :mu-auth-sudo (mu-auth-sudo)
                                                   :mu-auth-allowed-groups (mu-auth-allowed-groups)
                                                   :mu-call-scope (mu-call-scope)
                                                   :mu-call-id-trail (mu-call-id-trail))))
                                   str))
                   (response (execute-query-for-context query-string)))
              `(200
                (:content-type "application/sparql-results+json" :mu-auth-allowed-groups ,(mu-auth-allowed-groups))
                (,response))))))
    (error (e)
      (format t "Could not process query, yielding 500.  ~%~A~%" e)
      `(500 (:content-type "text/plain") (,(format nil "An error occurred ~A" e))))))

(defun boot (&key (port 8890) (worker-count 32))
  (format t "Booting server on port ~A with ~A workers" port worker-count)

  (bordeaux-threads:make-thread
   (lambda ()
     (woo:run (lambda (env) (acceptor env))
              :address "0.0.0.0"
              :port port
              :worker-num worker-count)
     (let ((sb-ext:*exit-timeout* 2))
       ;; TODO: ensure all open update queries are handled before exiting
       (format t "~&Giving two seconds notice to quit.~%")
       (force-output)
       (sb-ext:quit)))))

(setf woo.specials:*debug* t)

(when (find :docker *features*)
  (boot))

