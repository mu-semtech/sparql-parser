(in-package #:reasoner-tree-mirror)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defstruct reasoner-ast
  "Replication of MATCH for TERM-INFO which can be used to walk around solutions.

  The tree is a mirror of the SPARQL-AST but does not contain any
  information on direct string matches."
  (node (error "Must supply self") :type match)
  (parent nil :type (or null reasoner-ast))
  (children nil :type (or null cons))
  ;; local accounting
  (term-info-list nil :type (or null cons)) ; known info at this level
  (dirty-p nil :type (or null t))) ; helps in more efficient dirty tracking

(defmethod print-object ((object reasoner-ast) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (sparql-parser:match-term (reasoner-ast-node object)))))

(defun construct-reasoner-ast (sparql-ast)
  "Constructs a REASONER-AST for a SPARQL-AST"
  (labels ((make-ast (match &optional reasoner-ast-parent)
             (let ((self (make-reasoner-ast :node match
                                            :parent reasoner-ast-parent)))
               (setf (reasoner-ast-children self)
                     (mapcar (rcurry #'make-ast self)
                             (match-match-submatches match)))
               self)))
    (make-ast (sparql-ast-top-node sparql-ast))))

(defmacro tree-scan-deep-term-case (var (match &optional start-term) &body clauses)
  "Like SPARQL-MANIPULATION:SCAN-DEEP-TERM-CASE, but for ast-tree."
  (declare (ignore start-term)) ; TODO: expand valid paths based on search term
  `(tree-scan-deep-term-case*
    ,match
    (lambda (,var)
      (sparql-manipulation:expanded-term-case (match-term (reasoner-ast-node ,var))
        ,@clauses))))

(defun tree-scan-deep-term-case* (tree functor)
  "Like SPARQL-MANIPULATION:SCAN-DEEP-TERM-CASE*"
  (labels ((descend (tree)
             (loop for child in (reasoner-ast-children tree)
                   if (match-p (reasoner-ast-node child))
                     do
                        (let ((term (match-term (reasoner-ast-node child))))
                          (cond ((sparql-parser:terminalp term)
                                 (return-from tree-scan-deep-term-case* (funcall functor child)))
                                (t (descend child)))))))
    (descend tree)))

(defmacro with-named-child-tree ((var) (tree term) &body body)
  "Like SPARQL-MANIPULATION:WITH-TREE-NAMED-CHILD."
  `(when-let ((,var (find ',term
                          (reasoner-ast-children ,tree)
                          :key (compose #'match-term #'reasoner-ast-node)
                          :test #'eq)))
     ,@body))

(defmacro do-grouped-tree-children (lambda-list (match &key (amount 1) filter-terms error-on-incomplete-amount-p) &body body)
  "Like SPARQL-MANIPULATION:DO-GROUPED-TREE-CHILDREN."
  (let ((bindable-result (gensym "bindable-submatches")))
    `(dolist (,bindable-result (group-tree-children* ,match :amount ,amount :filter-terms ',filter-terms :error-on-incomplete-amount-p ,error-on-incomplete-amount-p))
       (destructuring-bind ,lambda-list
           ,bindable-result
         ,@body))))

(defun group-tree-children* (tree &key (amount 1) filter-terms error-on-incomplete-amount-p)
  "Like SPARQL-MANIPULATION:GROUP-TREE-CHILDREN*

Collects all children in MATCH filtering them by FILTER-TERMS and
returning them in lists of AMOUNT solutions.

FILTER-TERMS should be a list of terminals."
  (let ((matching-submatches (loop for tree in (reasoner-ast-children tree)
                                   when (or (not filter-terms)
                                            (find (match-term (reasoner-ast-node tree)) filter-terms :test #'eq))
                                     collect tree))
        (solutions nil))
    (loop while matching-submatches
          for solution = nil
          do
             (loop for i from 0 below amount
                   for next = (pop matching-submatches)
                   if (or next (not error-on-incomplete-amount-p))
                     do (push next solution)
                   else
                     do (error "Missing submatch in TREE ~A when filtering by ~A and collecting per ~A."
                               tree amount filter-terms))
             (push (reverse solution) solutions))
    (reverse solutions)))

(defmacro tree-match-symbol-case (tree &body clauses)
  "Like SPARQL-MANIPULATION:MATCH-SYMBOL-CASE."
  `(sparql-manipulation:expanded-term-case (match-term (reasoner-ast-node ,tree))
     ,@clauses))

;;;;;;;;;;;;;;;;;;;
;;;; loop over tree
(defun loop-tree-matches* (tree functor &key (filter (lambda (x) (declare (ignore x)) t)))
  "Deeply loops over each SUBMATCH of MATCH with FUNCTOR for which FILTER holds.

Returns no values."
  (loop for subtree in (reasoner-ast-children tree)
        when (funcall filter subtree)
          do (funcall functor subtree)
             (loop-tree-matches* subtree functor :filter filter))
  (values))

(defmacro loop-tree-matches ((var) sparql-ast &body body)
  "Loops over the matches of SPARQL-AST deeply, binding VAR to each MATCH.

Executes the forms in BODY for each and discards the result, leaving
SPARQL-AST in tact."
  `(loop-tree-matches* ,sparql-ast (lambda (,var) ,@body)))

(defmacro loop-tree-matches-symbol-case ((var) sparql-ast &body clauses)
  "Loops over each match of SPARQL-AST with a CASE running for the
solutions.

Allows case expansions through EXPANDED-TERM-CASE."
  `(loop-tree-matches (,var) ,sparql-ast
     (sparql-manipulation:expanded-term-case (match-term (reasoner-ast-node ,var))
       ,@clauses)))
