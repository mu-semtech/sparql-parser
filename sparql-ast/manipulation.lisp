(in-package #:sparql-manipulation)

(defun mk-match (content)
  "Constructs a match object from the abbreviated match specification."
  (destructuring-bind (term &rest submatches)
      content
    (flet ((transform-submatch (submatch)
             (typecase submatch
               (sparql-parser:match submatch)
               (string (sparql-parser::make-match
                        :term submatch
                        :submatches (list (sparql-parser::make-scanned-token :start 0 :end 0 :token submatch))))
               (list (mk-match submatch)))))
      (sparql-parser::make-match :term term
                                 :submatches (mapcar #'transform-submatch submatches)))))

(defun iriref (graph-string)
  "Constructs an IRIREF for GRAPH-STRING."
  (sparql-parser::make-match
   :term 'ebnf::|IRIREF|
   :submatches
   (list (sparql-parser::make-scanned-token
          :start 0 :end 0
          :string graph-string
          :token 'ebnf::|IRIREF|))))

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
                            (eq 'ebnf::|DatasetClause|
                                (sparql-parser:match-term submatch))))
                     (sparql-parser:match-submatches match)))
    (mapcar #'remove-dataset-clauses (sparql-parser:match-submatches match)))
  match)

(defun remove-graph-graph-patterns (match)
  "Converts QuadsNotTriples into TriplesTemplate."
  (when (and (sparql-parser:match-p match))
    (when (eq (sparql-parser:match-term match) 'ebnf::|GraphGraphPattern|)
     ;; GraphGraphPattern ::= 'GRAPH' VarOrIri GroupGraphPattern
     ;; GroupOrUnionGraphPatterrn ::= GroupGraphPattern ( 'UNION' GroupGraphPattern )*
      (setf (sparql-parser:match-term match) 'ebnf::|GroupOrUnionGraphPattern|)
      (setf (sparql-parser:match-submatches match)
            (cddr (sparql-parser:match-submatches match))))
    (mapcar #'remove-graph-graph-patterns (sparql-parser:match-submatches match)))
  match)

(defun add-from-graphs (match graphs)
  "Adds a series of graphs as the FROM graphs for MATCH."
  (let ((dataset-clauses
          (loop for graph-string in graphs
                collect (mk-match `(ebnf::|DatasetClause|
                                    "FROM" (ebnf::|DefaultGraphClause|
                                            (ebnf::|SourceSelector|
                                             (ebnf::|iri| ,(iriref graph-string)))))))))
    (labels ((traverse (match)
               (when (sparql-parser:match-p match)
                 (when (eq (sparql-parser:match-term match) 'ebnf::|SelectQuery|)
                   (update-submatches (match submatches)
                     (destructuring-bind (select-clause &rest other-clauses)
                         submatches
                       `(,select-clause ,@dataset-clauses ,@other-clauses))))
                 (mapcar #'traverse (sparql-parser:match-submatches match)))))
      (traverse match)
      match)))
