(in-package #:administration)

(defun sparql-escape-uri (uri)
  "Escapes URI for use in a SPARQL query."
  (sparql-manipulation:uri-wrap-marks
   (coerce
    (loop for char across uri
          if (find char "\\<>\"" :test #'char=)
            append (list #\\ char)
          else
            collect char)
    #+be-cautious 'string #-be-cautious 'base-string)))

(defun sparql-escape-string (string &key lang datatype)
  "Escapes STRING for use in a SPARQL query.
LANG and DATATYPE may be supplied, but only one of them may be non-nil.  LANG is expected to exclude the @ (eg: nl)."
  (format nil "\"\"\"~A\"\"\"~@[@~A~]~@[^^~A~] "
          (coerce (loop for char across string
                        if (find char (list #\\ #\") :test #'char=)
                          append (list #\\ char)
                        else
                          collect char)
                  #+be-cautious 'string #-be-cautious 'base-string)
          lang
          (and datatype (sparql-escape-uri datatype))))

(defparameter *long-db-strings-to-move-per-batch* 10
  "How many long strings in the DB are moved to files per batch.")

(defun update-database-long-strings-to-string-files ()
  "Updates long strings in the triplestore to become string files."
  (let ((select-query
          (format
           nil
           "SELECT ?g ?s ?p ?o
           WHERE {
             GRAPH ?g {
               ?s ?p ?o
             }
             FILTER( isLiteral(?o) && strlen(str(?o)) > ~A)
           } LIMIT ~A" support:*string-max-size* *long-db-strings-to-move-per-batch*)))
    (loop for bindings = (client:bindings (client:query select-query :send-to-single t)
                                          :convert-string-uris nil)
          while bindings
          for batch from 1
          do
             (format t "DB TO FILE BATCH ~A has ~A bindings (max ~A)" batch (length bindings) *long-db-strings-to-move-per-batch*)
             (let
                 ((replacements
                    (loop for binding in bindings
                          for object = (jsown:val binding "o")
                          for datatype = (jsown:val-safe object "datatype")
                          for lang = (or (jsown:val-safe object "lang")
                                         (jsown:val-safe object "xml:lang"))
                          for value = (jsown:val object "value")
                          collect
                          (multiple-value-bind (replacement replacement-p)
                              (support:maybe-string-to-uri value :lang lang :datatype datatype)
                            (unless replacement-p
                              (error 'simple-error
                                     :format-control "Something went wrong converting quad~%~A"
                                     :format-arguments (list object)))
                            (list binding replacement lang datatype)))))
               ;; first insert the alternatives (we may need to group these
               (client:query
                (format nil "INSERT DATA {~{~%  GRAPH ~{~A { ~A ~A ~A }~}~}~%}"
                        (loop for (binding replacement) in replacements
                              collect
                              (list
                               (sparql-escape-uri (jsown:filter binding "g" "value"))
                               (sparql-escape-uri (jsown:filter binding "s" "value"))
                               (sparql-escape-uri (jsown:filter binding "p" "value"))
                               (sparql-escape-uri replacement)))))
               ;; then delete the old values
               (client:query
                (format nil "DELETE DATA {~{~% GRAPH ~{~A {~A ~A ~A}~}~}~%}"
                        (loop for (binding replacement lang datatype) in replacements
                              for value = (jsown:filter binding "o" "value")
                              collect
                              (list
                               (sparql-escape-uri (jsown:filter binding "g" "value"))
                               (sparql-escape-uri (jsown:filter binding "s" "value"))
                               (sparql-escape-uri (jsown:filter binding "p" "value"))
                               (sparql-escape-string value :lang lang :datatype datatype)))))))))

(defun all-string-files ()
  "Lists all string files."
  (directory (make-pathname
              :name :wild :type :wild
              :defaults support:*sha-file-directory*)))

(defparameter *short-string-files-to-move-per-batch* 10
  "How many short string-files are moved per batch for discovered short string files.")

(defun move-short-string-files-into-database ()
  "Searches for string-files which are too short and which should be inlined in the triplestore again."
  (loop for file in (all-string-files)
        ;; TODO: constrain on N * the max length of the file, we don't need to read in strings when we know we will not
        ;; need the data.
        for string = (support:read-path-as-string file)
        for sha = (pathname-name file)
        if (<= (length string) support:*string-max-size*)
          do ;; inline the string
             (loop for bindings = (client:bindings
                                   (client:query
                                    (format nil "SELECT ?g ?s ?p ?o { GRAPH ?g { ?s ?p ?o. } FILTER ( isIri(?o) && STRSTARTS(STR(?o), ~A ) ) } LIMIT ~A"
                                            (sparql-escape-string (support:string-file-uri sha))
                                            *short-string-files-to-move-per-batch*))
                                   :convert-string-uris nil)
                   for i from 1
                   while bindings
                   do
                      (format t "~&FILE TO DB BATCH ~A for ~A :: Updating ~A quads (max ~A)~%" i sha (length bindings) *short-string-files-to-move-per-batch*)
                      (client:query
                       (format nil
                               (concatenate 'string
                                            "INSERT DATA {~{~% GRAPH ~{~A { ~A ~A ~A  }~}~}~% };"
                                            "DELETE DATA {~{~% GRAPH ~{~A { ~A ~A ~A }~}~}~% };")
                               (loop for quad in bindings
                                     collect
                                     (list
                                      (sparql-escape-uri (jsown:filter quad "g" "value"))
                                      (sparql-escape-uri (jsown:filter quad "s" "value"))
                                      (sparql-escape-uri (jsown:filter quad "p" "value"))
                                      (multiple-value-bind (string uri-string-p lang datatype)
                                          (support:maybe-uri-to-string (jsown:filter quad "o" "value"))
                                        (declare (ignore uri-string-p))
                                        (sparql-escape-string
                                         string
                                         :lang lang
                                         :datatype datatype))))
                               (loop for quad in bindings
                                     collect
                                     (list
                                      (sparql-escape-uri (jsown:filter quad "g" "value"))
                                      (sparql-escape-uri (jsown:filter quad "s" "value"))
                                      (sparql-escape-uri (jsown:filter quad "p" "value"))
                                      (sparql-escape-uri (jsown:filter quad "o" "value")))))))
        else
          do (format t "~&FILE TO DB string-file ~A is ok~%" sha)))

(defun update-database-string-files (&key (database-p t) (files-p t))
  "Upgrades the database string files to match the current `SUPPORT::*STRING-MAX-SIZE*'."
  (client:ensure-endpoints-available :verbose t)
  (when database-p (update-database-long-strings-to-string-files))
  (when files-p (move-short-string-files-into-database)))
