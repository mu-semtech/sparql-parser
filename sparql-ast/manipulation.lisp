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

(defun make-match-up-to-scanned-token (&key string match-list)
  (if (rest match-list)
      (sparql-parser:make-match
       :term (first match-list)
       :submatches (list (make-match-up-to-scanned-token
                          :string string
                          :match-list (rest match-list))))
      (sparql-parser:make-match
       :term (first match-list)
       :submatches (list (sparql-parser:make-scanned-token
                          :start 0 :end 0
                          :string string
                          :token (first match-list))))))

(defun make-iri (uri)
  "Constructs an IRI with IRIREF in it."
  (sparql-parser:make-match
   :term 'ebnf::|iri|
   :submatches (list (iriref uri))))

(defun iriref (uri)
  "Constructs an IRIREF for GRAPH-STRING."
  (sparql-parser:make-match
   :term 'ebnf::|IRIREF|
   :submatches
   (list (sparql-parser:make-scanned-token
          :start 0 :end 0
          :string (uri-wrap-marks uri)
          :token 'ebnf::|IRIREF|))))

(defun make-var (name)
  "Constructs a variable, supply varname including ? or $."
  (sparql-parser:make-match
   :term 'ebnf::|Var|
   :submatches
   (list (sparql-parser:make-match
          :term 'ebnf::|VAR1|
          :submatches
          (list (sparql-parser:make-scanned-token
                 :start 0 :end 0
                 :string name
                 :token 'ebnf::|VAR1|))))))

(defun uri-wrap-marks (uri-string)
  "Wraps a URI in < and > marks.

Assumes URI is not wrapped."
  (coerce (concatenate 'string "<" uri-string ">")
          #-be-cautious 'base-string
          #+be-cautious 'string))

(defun uri-unwrap-marks (uri-string)
  "Unwraps a URI's < and > marks.

Assumes URI-STRING is wrapped."
  (subseq uri-string 1 (1- (length uri-string))))

(defun sparql-escape-string (string)
  "Generate an escaped SPARQL string for triple double quotes."
  (concatenate 'string
               "\"\"\""
               (cl-ppcre:regex-replace-all "([\"\\\\])" string "\\\\\\1")
               "\"\"\""))

(defun make-token-match (term string)
  "Makes a token match for the given string.  This is a MATCH which has a SCANNED-TOKEN as a match."
  `(,term
    ,(sparql-parser:make-scanned-token
      :start 0 :end 0
      :string string
      :token term)))

(defun make-rdfliteral (string &key datatype-match)
  "This constructs an ebnf::|RDFLiteral| from STRING.

Currently supports only string, but could be extended with datatype and lang keywords when necessary."
  (handle-update-unit::make-nested-match
   `(ebnf::|RDFLiteral|
           ,(make-string-literal string)
           ,@(and datatype-match
                  (list "^^" datatype-match)))))

(defun make-string-literal (string)
  "Constructs a string literal for string, escaping as necessary."
  (handle-update-unit::make-nested-match
   `(ebnf::|String|
           ,(make-token-match 'ebnf::|STRING_LITERAL_LONG2|
                              (sparql-escape-string string)))))

(defun string-literal-string (ebnf-string)
  "Gets the original string for an 'ebnf::|String|, unescaping as necessary.

  Escapes quoting but not special characters such as \\t and \\n."
  (let* ((specific-match (first (match-submatches ebnf-string)))
         (term (sparql-parser:match-term specific-match))
         (string (sparql-parser:terminal-match-string specific-match)))
    (case term
      (ebnf::|STRING_LITERAL_LONG1|
       (cl-ppcre:regex-replace-all "\\\\(['\\\\])"
                                   (subseq string 3 (- (length string) 3))
                                   "\\1"))
      (ebnf::|STRING_LITERAL_LONG2|
       (cl-ppcre:regex-replace-all "\\\\([\"\\\\])"
                                   (subseq string 3 (- (length string) 3))
                                   "\\1"))
      (ebnf::|STRING_LITERAL1|
       (cl-ppcre:regex-replace-all "\\\\([\'\\\\])"
                                   (subseq string 1 (1- (length string)))
                                   "\\1"))
      (ebnf::|STRING_LITERAL2|
       (cl-ppcre:regex-replace-all "\\\\([\"\\\\])"
                                   (subseq string 1 (1- (length string)))
                                   "\\1")))))

(defun make-word-match (string)
  "Constructs a match for fixed content in the EBNF.

  Eg: the string \"GRAPH\" or \"INSERT DATA\"."
  (sparql-parser:make-match
   :term string :rule nil
   :submatches (list (sparql-parser::make-scanned-token
                      :start 0 :end 0
                      :string string :token string))))

(defmacro update-submatches ((thing submatches-var) &body body)
  "Set submatches of THING when it's a MATCH."
  (let ((match-var (gensym "match")))
    `(let ((,match-var ,thing))
       (when (match-p ,match-var)
         (setf (sparql-parser:match-submatches ,match-var)
               (let ((,submatches-var (sparql-parser:match-submatches ,match-var)))
                 ,@body))))))

(defun map-matches* (match functor)
  "Maps over each submatch of MATCH with FUNCTOR, replacing it with the
list of matches yielded by the function."
  (setf (sparql-parser:match-submatches match)
        (loop for submatch in (sparql-parser:match-submatches match)
              if (match-p submatch)
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
      (unless (eq term (match-term match))
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

(defmacro expanded-term-case (term &body clauses)
  "Constructs a CASE for TERM, expanding CLAUSES by
ABSTRACT-TOKEN-EXPANSION and duplicating terms for clauses which are
lists."
  (flet ((expand-clause (clause)
           (destructuring-bind (term-spec &rest body)
               clause
             (cons
              (loop for term in (if (listp term-spec) term-spec (list term-spec))
                    append (or (ebnf:abstract-token-expansion term) (list term)))
              body)))
         (dedup-clause (clause)
           ;; TODO: case accepts a list as its first argument which
           ;; would allow us not to repeat body
           (destructuring-bind (term-spec &rest body)
               clause
             (if (listp term-spec)
                 (loop for term in (alexandria:flatten term-spec)
                       collect `(,term ,@body))
                 `((,term ,@body))))))
    `(case ,term
       ,@(loop for clause in clauses
               append (dedup-clause (expand-clause clause))))))

(defmacro update-matches-symbol-case ((var) sparql-ast &body body)
  "Maps over each match of SPARQL-AST deeply, binding VAR to match.  BODY is a
list of forms like CASE in which the CAR is the SYMBOL to be matched
as MATCH-NAME and the CDR a list of FORMS to be excuted with MATCH
bound.  The found MATCH is replaced by the list of matches returned.

Allows case expansions through EXPANDED-TERM-CASE."
  (let ((top-match (gensym "top-match")))
    `(process-sparql-ast-match (,top-match) ,sparql-ast
       (map-matches (,var) ,top-match
         (expanded-term-case (match-term ,var)
           ,@body
           (otherwise (list ,var)))))))

(defun loop-matches* (match functor &key (filter #'match-p))
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
solutions.

Allows case expansions through EXPANDED-TERM-CASE."
  `(loop-matches (,var) ,sparql-ast
     (expanded-term-case (typecase ,var
                           (sparql-parser:match (match-term ,var))
                           (sparql-parser:scanned-token (sparql-parser:scanned-token-token ,var)))
       ,@clauses)))

(defmacro match-symbol-case (match &body clauses)
  "Matches MATCH using CASE for its SYMBOL.

Allows case expansions through EXPANDED-TERM-CASE."
  `(when (match-p ,match)
     (expanded-term-case (match-term ,match)
       ,@clauses)))

(defmacro with-named-child ((var) (match term) &body body)
  "Executes BODY in a context where VAR is bound to the first submatch of
MATCH that has symbol TERM.  If no solution is found BODY is not
executed and NIL is returned."
  `(alexandria:when-let ((,var (find ',term (sparql-parser:match-submatches ,match) :test (lambda (term match) (and (sparql-parser:match-p match) (eq term (sparql-parser:match-term match)))))))
     ,@body))

(defun follow-path (match path)
  "Follow PATH  in  MATCH yielding a list of solutions.

PATH is a tree of properties to follow.  Multiple paths can be followed
with :OR.

  (:or a b c)

is interpreted as following either a, b or c.  This path itself can
contain lists to be interpreted as a further depth.

  (:or a (b c))

is interpreted as either a, or c embedded in b."
  (cond ((symbolp path)
         ;; verify the current match and return it
         (when (eq path (match-term match))
           (list match)))
        ((eq (first path) :or)
         ;; combine all sub solutions
         (loop for subpath in (rest path)
               append (follow-path match subpath)))
        ((eq (first path)
             (match-term match))
         ;; walk over the rest paths and collect the last ones
         (let ((submatches (sparql-parser:match-submatches match)))
           (loop for subselection in (rest path)
                 do
                    (setf submatches
                          (loop for submatch in submatches
                                append (follow-path submatch subselection))))
           submatches))))

(defun match-child-term-list (match &optional term)
  "Yields a list of all children with the given type, not recursive.

This is an interesting construct when the items in the list can repeat
themselves, like with an ObjectList."
  (flet ((matches-constraint (submatch)
           (let ((match-term (match-term submatch)))
             (and (symbolp match-term)
                  (or (not term) (eq term match-term))))))
   (loop for submatch in (sparql-parser:match-submatches match)
         when (matches-constraint submatch)
           collect submatch)))

(defun scan-deep-term-case* (match functor)
  "Deeply scans match for a term, calling functor on the first term and yielding the result.

Also see REASONER-TREE-MIRROR:TREE-SCAN-DEEP-TERM-CASE."
  (labels ((descend (match)
             (loop for sub-match in (sparql-parser:match-submatches match)
                   if (match-p sub-match)
                     do
                        (let ((term (match-term sub-match)))
                          (cond ((stringp term) nil) ; should not match constants
                                ((sparql-parser:terminalp term)
                                 (return-from scan-deep-term-case* (funcall functor sub-match)))
                                (t (descend sub-match)))))))
    (descend match)))

(defmacro scan-deep-term-case (var (match &optional start-term) &body clauses)
  "Deeply matches the term until a terminal match was found.  Accepts any clause of clauses.

Allows case expansions through EXPANDED-TERM-CASE.

TODO: when start-term is supplied, verify all options of start-term are
taken into account and no loop paths are available."
  (declare (ignore start-term))
  `(scan-deep-term-case*
    ,match
    (lambda (,var)
      (expanded-term-case (match-term ,var)
        ,@clauses))))

(defun self-recursive-list (match)
  "A match which looks like Foo ::= A Foo B will be expanded into a list of
Foo matches."
  (let ((solutions nil)
        (target-term (match-term match))
        (next (list match)))
    (loop
      while next
      do
         (alexandria:appendf solutions next)
         (setf next
               (loop for current in next
                     append (loop for sub in (match-submatches current)
                                  when (and (match-p sub)
                                            (eq (match-term sub) target-term))
                                    collect sub))))
    solutions))

(defun group-children* (match &key (amount 1) filter-terms error-on-incomplete-amount-p)
  "Collects all children in MATCH filtering them by FILTER-TERMS and
returning them in lists of AMOUNT solutions.

FILTER-TERMS should be a list of terminals."
  (let ((matching-submatches (loop for match in (match-submatches match)
                                   when (or (not filter-terms)
                                            (and (match-p match)
                                                 (find (match-term match) filter-terms :test #'eq)))
                                     collect match))
        (solutions nil))
    (loop while matching-submatches
          for solution = nil
          do
             (loop for i from 0 below amount
                   for next = (pop matching-submatches)
                   if (or next (not error-on-incomplete-amount-p))
                     do (push next solution)
                   else
                     do (error "Missing submatch in MATCH ~A when filtering by ~A and collecting per ~A."
                               match amount filter-terms))
             (push (reverse solution) solutions))
    (reverse solutions)))

(defmacro do-grouped-children (lambda-list (match &key (amount 1) filter-terms error-on-incomplete-amount-p) &body body)
  "Loops over the children of MATCH grouped as per GROUP-CHILDREN* with
destructured bindings of LAMBDA-LIST as per DESTRUCTURING-BIND."
  (let ((bindable-result (gensym "bindable-submatches")))
    `(dolist (,bindable-result (group-children* ,match :amount ,amount :filter-terms ',filter-terms :error-on-incomplete-amount-p ,error-on-incomplete-amount-p))
       (destructuring-bind ,lambda-list
           ,bindable-result
         ,@body))))

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
             (when (and (match-p match))
               (when (eq (match-term match) 'ebnf::|GraphGraphPattern|)
                 ;; GraphGraphPattern ::= 'GRAPH' VarOrIri GroupGraphPattern
                 ;; GroupOrUnionGraphPatterrn ::= GroupGraphPattern ( 'UNION' GroupGraphPattern )*
                 (setf (match-term match) 'ebnf::|GroupOrUnionGraphPattern|)
                 (setf (sparql-parser:match-submatches match)
                       (cddr (sparql-parser:match-submatches match))))
               (mapcar #'traverse (sparql-parser:match-submatches match)))))
    (traverse (sparql-parser:sparql-ast-top-node sparql-ast)))
  sparql-ast)

(defun fold-and-remove-quads-not-triples (sparql-ast)
  "Removes QuadsNotTriples statements and folds into TriplesTemplate."
  ;; The relevant manipulations are:
  ;; [50] Quads ::= TriplesTemplate? ( QuadsNotTriples '.'? TriplesTemplate? )*
  ;; [51] QuadsNotTriples ::= 'GRAPH' VarOrIri '{' TriplesTemplate? '}'
  ;; [52] TriplesTemplate := TriplesSameSubject ( '.' TriplesTemplate? )?
  ;;
  ;; We will therefore search for QuadsNotTriples inside of Quads (the only place they can live), pull ourselves higher
  ;; into QuadsNotTriples and then fold the corresponding TriplesTemplate statements.
  ;;
  ;; Note that QuadsNotTriples can't be nested in itself, it can only be part of Quads.  Lacking loops we don't have to
  ;; consider further nesting and only need to join TriplesTemplate from Quads onwards and know all of the
  ;; TriplesTemplate starting points from Quads and QuadsNotTriples.
  (labels ((traverse (match)
             (when (match-p match)
               (when (eq (match-term match) 'ebnf::|Quads|)
                 (let ((triples-templates
                         (loop for submatch in (sparql-parser:match-submatches match)
                               if (match-p submatch)
                                 collect (case (match-term submatch)
                                           (ebnf::|TriplesTemplate| submatch)
                                           (ebnf::|QuadsNotTriples| (when (match-p (fourth (match-submatches submatch)))
                                                                      (fourth (match-submatches submatch))))))))
                   (when triples-templates
                     (join-triples-templates (first triples-templates) (rest triples-templates))
                     (setf (match-submatches match) (list (first triples-templates))))))
               (mapcar #'traverse (sparql-parser:match-submatches match))))
           (join-triples-templates (main-template other-templates)
             ;; Inlines the triple-templates, a bit more complex because they are defined as a recursive list.  Always
             ;; search for the first TriplesTemplate which has less than 2 elements and then reconstruct its children.  May destroy the supplied ebnf:|TriplesTemplate|s
             (when other-templates
               (if (< (length (sparql-parser:match-submatches main-template)) 3)
                   ;; this is a last element
                   (let ((new-children
                           (list (first (sparql-parser:match-submatches main-template))
                                 (or (second (sparql-parser:match-submatches main-template))
                                     (make-word-match ".")
                                     (first other-templates)))))
                     (setf (sparql-parser:match-submatches main-template) new-children)
                     ;; everything set up, recurse over other-templates
                     (join-triples-templates (first other-templates) (rest other-templates)))
                   ;; not a last element, recurse
                   (join-triples-templates (third (sparql-parser:match-submatches main-template))
                                           other-templates)))))
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
               match))
      (ebnf::|ConstructQuery|
             ;; ALways starts with "CONSTRUCT" then either:
             ;; - a ConstructTemplate followed by a DatasetClause*
             ;; - a DatasetClause* followed by "WHERE"
             (let* ((submatches (match-submatches match))
                    (second-submatch (second  submatches)))
               (if (match-p second-submatch)
                   (case (match-term second-submatch)
                     (ebnf::|ConstructTemplate|
                      ;; insert in the third spot
                      (setf (sparql-parser:match-submatches match)
                            `(,(first submatches)
                              ,(second submatches)
                              ,@dataset-clauses
                              ,@(cddr submatches))))
                     (ebnf::|DatasetClause|
                      ;; insert in the second spot
                      (setf (sparql-parser:match-submatches match)
                            `(,(first submatches)
                              ,@dataset-clauses
                              ,@(cdr submatches))))
                     (t (error "ConstructQuery has is not ConstructTemplate or DatasetClause as second submatch but is a match-p: ~A"
                               second-submatch)))
                   ;; must be the word "WHERE", insert in the second spot
                   (setf (sparql-parser:match-submatches match)
                         `(,(first submatches)
                           ,@dataset-clauses
                           ,@(cdr submatches))))
               match))
      (ebnf::|DescribeQuery|
             ;; Always starts with DESCRIBE then either multiple VarOrIri elements or "*".
             ;; Map over (rest submatches) and collect into the right bin:
             (let ((describe-word (first (sparql-parser:match-submatches match)))
                   (vars (list (second (sparql-parser:match-submatches match))))
                   where-list
                   solution-modifier)
               ;; collect submatches by type
               (dolist (submatch (cddr (sparql-parser:match-submatches match)))
                 (match-symbol-case submatch
                   (ebnf::|VarOrIri| (push submatch vars))
                   (ebnf::|WhereClause| (push submatch where-list))
                   (ebnf::|SolutionModifier| (setf solution-modifier submatch))
                   (otherwise (error "Unknown match received in DescribeQuery"))))
               ;; reconstruct submatches
               (setf (match-submatches match)
                     `(,describe-word
                       ,@(reverse vars)
                       ,@dataset-clauses
                       ,@where-list
                       ,solution-modifier))
               match))
      (ebnf::|AskQuery|
             ;; Only need to replace all dataset-clauses or inject them in the right spot.
             (let ((ask-word (first (match-submatches match))))
               (destructuring-bind (solution-modifier where-clause &rest args)
                   (reverse (match-submatches match))
                 (declare (ignore args))
                 (setf (match-submatches match)
                       `(,ask-word ,@dataset-clauses ,where-clause ,solution-modifier))
                 match))))))

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
  (let ((from (uri-wrap-marks from))
        (to (iriref (coerce to #-be-cautious 'base-string #+be-cautious 'string))))
    (update-matches-symbol-case (match) sparql-ast
      (ebnf::|IRIREF|
             (if (string= from (sparql-parser:terminal-match-string match))
                 to
                 match)))))
