(in-package #:sparql-manipulation)

;;;; Manipulation primitives
;;;;
;;;; Easier ways to manipulate matches

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

(defun map-matches* (match functor)
  "Maps over each submatch of MATCH with FUNCTOR, replacing it with the
list of matches yielded by the function."
  (setf (sparql-parser:match-submatches match)
        (loop for submatch in (sparql-parser:match-submatches match)
              if (sparql-parser:match-p submatch)
                append (prog1 (funcall functor submatch)
                         (map-matches* submatch functor))
              else
                collect submatch))
  match)

(defmacro map-matches (match (var) &body body)
  "Macro variant of MAP-MATCHES*."
  ;; This variant may allow for further optimizations down the line.
  `(map-matches* ,match (lambda (,var) ,@body)))

(defun remove-clause (sparql-ast term)
  "Removes any match with MATCH-TERM TERM from MATCH."
  ;; Moving this into a separate function we give ourselves the leeway
  ;; to optimize later on.  An obvious optimization is to detect where
  ;; the clause may exist and home in on it.
  (prog1 sparql-ast
    (map-matches (sparql-parser:sparql-ast-top-node sparql-ast) (match)
      (unless (eq term (sparql-parser:match-term match))
        (list match)))))


;;;; High level manipulations
;;;;
;;;; These are the manipulations we may ask on the match as a whole as a
;;;; logical unit.

(defun remove-dataset-clauses (sparql-ast)
  "Removes the DatasetClause statements from MATCH, destructively updating it."
  (remove-clause sparql-ast 'ebnf::|DatasetClause|))

(defun remove-graph-graph-patterns (sparql-ast)
  "Converts QuadsNotTriples into TriplesTemplate."
  (labels ((traverse (match)
             (when (and (sparql-parser:match-p match))
               (when (eq (sparql-parser:match-term match) 'ebnf::|GraphGraphPattern|)
                 ;; GraphGraphPattern ::= 'GRAPH' VarOrIri GroupGraphPattern
                 ;; GroupOrUnionGraphPatterrn ::= GroupGraphPattern ( 'UNION' GroupGraphPattern )*
                 (setf (sparql-parser:match-term match) 'ebnf::|GroupOrUnionGraphPattern|)
                 (setf (sparql-parser:match-submatches match)
                       (cddr (sparql-parser:match-submatches match))))
               (mapcar #'traverse (sparql-parser:match-submatches match)))))
    (traverse (sparql-parser:sparql-ast-top-node sparql-ast)))
  sparql-ast)

(defun add-from-graphs (sparql-ast graphs)
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
      (traverse (sparql-parser:sparql-ast-top-node sparql-ast))
      sparql-ast)))
