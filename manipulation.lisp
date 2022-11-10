(in-package #:sparql-manipulation)

(defmacro update-submatches ((thing submatches-var) &body body)
  "Set submatches of THING when it's a MATCH."
  (let ((match-var (gensym "match")))
    `(let ((,match-var ,thing))
       (when (sparql-parser:match-p ,match-var)
         (setf (sparql-parser:match-submatches ,match-var)
               (let ((,submatches-var (sparql-parser:match-submatches ,match-var)))
                 ,@body))))))

(defun remove-dataset-clauses (match)
  "Removes the DatasetClause statements from MATCH, destructively updating it."
  (when (sparql-parser:match-p match)
    (setf (sparql-parser:match-submatches match)
          (delete-if (lambda (submatch)
                       (and (sparql-parser:match-p submatch)
                            (eq 'sparql-bnf::|DatasetClause|
                                (sparql-parser:match-term submatch))))
                     (sparql-parser:match-submatches match)))
    (mapcar #'remove-dataset-clauses (sparql-parser:match-submatches match)))
  match)

(defun add-from-graphs (match graphs)
  "Adds a series of graphs as the FROM graphs for MATCH."
  (let ((dataset-clauses
          (loop for graph-string in graphs
                collect ;; (construct-match `(|DatasetClause| "FROM"
                        ;;                                    (|DefaultGraphClause|
                        ;;                                     (|SourceSelector|
                        ;;                                      (|iri| ,(iriref graph))))))
                (sparql-parser::make-match
                 :term 'sparql-bnf::|DatasetClause|
                 :submatches (list (sparql-parser::make-match
                                    :term "FROM"
                                    :submatches (list (sparql-parser::make-scanned-token :start 0 :end 0 :token "FROM")))
                                   (sparql-parser::make-match
                                    :term 'sparql-bnf::|DefaultGraphClause|
                                    :submatches (list (sparql-parser::make-match
                                                       :term 'sparql-bnf::|SourceSelector|
                                                       :submatches (list (sparql-parser::make-match
                                                                          :term 'sparql-bnf::|iri|
                                                                          :submatches (list (sparql-parser::make-match
                                                                                             :term 'sparql-bnf::|IRIREF|
                                                                                             :submatches
                                                                                             (list (sparql-parser::make-scanned-token
                                                                                                    :start 0 :end 5 :token ; TODO: utter nonsense
                                                                                                    'sparql-bnf::|IRIREF|))))))))))))))
    (labels ((traverse (match)
               (when (sparql-parser:match-p match)
                 (when (eq (sparql-parser:match-term match) 'sparql-bnf::|SelectQuery|)
                   (update-submatches (match submatches)
                     (destructuring-bind (select-clause &rest other-clauses)
                         submatches
                       `(,select-clause ,@dataset-clauses ,@other-clauses))))
                 (mapcar #'traverse (sparql-parser:match-submatches match)))))
      (traverse match)
      match)))
