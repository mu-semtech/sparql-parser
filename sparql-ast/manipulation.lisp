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

(defun iriref (uri)
  "Constructs an IRIREF for GRAPH-STRING."
  (sparql-parser::make-match
   :term 'ebnf::|IRIREF|
   :submatches
   (list (sparql-parser::make-scanned-token
          :start 0 :end 0
          :string (concatenate 'string "<" uri ">")
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
                append (prog1 (let ((result (funcall functor submatch)))
                                (if (listp result)
                                    result
                                    (list result)))
                         (map-matches* submatch functor))
              else
                collect submatch))
  match)

(defmacro map-matches ((var) match &body body)
  "Macro variant of MAP-MATCHES*."
  ;; This variant may allow for further optimizations down the line.
  `(map-matches* ,match (lambda (,var) ,@body)))

(defun remove-clause (sparql-ast term)
  "Removes any match with MATCH-TERM TERM from MATCH."
  ;; Moving this into a separate function we give ourselves the leeway
  ;; to optimize later on.  An obvious optimization is to detect where
  ;; the clause may exist and home in on it.
  (prog1 sparql-ast
    (map-matches (match)
        (sparql-parser:sparql-ast-top-node sparql-ast)
      (unless (eq term (sparql-parser:match-term match))
        (list match)))))

(defmacro process-sparql-ast-match ((var) sparql-ast &body body)
  "Processes a SPARQL-AST MATCH within the right context and yielding back
the SPARQL-AST."
  (let ((sparql-ast-var (gensym "sparql-ast")))
    `(let* ((,sparql-ast-var ,sparql-ast)
            (,var (sparql-parser:sparql-ast-top-node ,sparql-ast-var)))
       (sparql-parser:with-sparql-ast ,sparql-ast-var
         (prog1 ,sparql-ast-var
           ,@body)))))

(defmacro update-matches-symbol-case ((var) sparql-ast &body body)
  "Maps over each match of SPARQL-AST deeply, binding VAR to match.  BODY is a
list of forms like CASE in which the CAR is the SYMBOL to be matched
as MATCH-NAME and the CDR a list of FORMS to be excuted with MATCH
bound.  The found MATCH is replaced by the list of matches returned."
  (let ((top-match (gensym "top-match")))
    `(process-sparql-ast-match (,top-match) ,sparql-ast
       (map-matches (,var) ,top-match
         (case (sparql-parser:match-term ,var)
           ,@body
           (otherwise (list ,var)))))))

(defun loop-matches* (match functor &key (filter #'sparql-parser:match-p))
  "Deeply loops over each SUBMATCH of MATCH with FUNCTOR for which FILTER holds.

Returns no values."
  (loop for submatch in (sparql-parser:match-submatches match)
        when (funcall filter submatch)
          do (funcall functor submatch)
             (loop-matches* submatch functor :filter filter))
  (values))

(defmacro loop-matches ((var) sparql-ast &body body)
  "Loops over the matches of SPARQL-AST deeply, binding VAR to each MATCH.

Executes the forms in BODY for each and discards the result, leaving
SPARQL-AST in tact."
  (let ((top-match (gensym "top-match")))
    `(process-sparql-ast-match (,top-match) ,sparql-ast
       (loop-matches* ,top-match (lambda (,var) ,@body)))))

(defmacro loop-matches-symbol-case ((var) sparql-ast &body clauses)
  "Loops over each match of SPARQL-AST with a CASE running for the
solutions."
  `(loop-matches (,var) ,sparql-ast
     (case (typecase ,var
             (sparql-parser:match (sparql-parser:match-term ,var))
             (sparql-parser:scanned-token (sparql-parser:scanned-token-token ,var)))
       ,@clauses)))

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
    (update-matches-symbol-case (match) sparql-ast
      (ebnf::|SelectQuery|
             (destructuring-bind (select-clause &rest other-clauses)
                 (sparql-parser:match-submatches match)
               (setf (sparql-parser:match-submatches match)
                     `(,select-clause ,@dataset-clauses ,@other-clauses))
               match)))))

(defun add-default-base-decl-to-prologue (sparql-ast &optional (base-uri "http://mu.semte.ch/prefix/local/"))
  "Adds a default base decl as the first element of PROLOGUE."
  (update-matches-symbol-case (match) sparql-ast
    (ebnf::|Prologue|
           (setf (sparql-parser:match-submatches match)
                 (cons (mk-match `(ebnf::|BaseDecl| "BASE" ,(iriref base-uri)))
                       (sparql-parser:match-submatches match)))
           match)))

(defun replace-iriref (sparql-ast &key from to)
  "Replaces each occurence of IRIREF FROM to TO in SPARQL-AST.

FROM and TO are both expected to be strings.

Used to replace <SESSION_URI> in access calculation."
  ;; from: source-iriref-string
  ;; to: target-iriref-string
  (let ((from (coerce (concatenate 'string "<" from ">") 'base-string))
        (to (iriref (coerce to 'base-string))))
    (update-matches-symbol-case (match) sparql-ast
      (ebnf::|IRIREF|
             (if (string= from (sparql-parser:terminal-match-string match))
                 to
                 match)))))
