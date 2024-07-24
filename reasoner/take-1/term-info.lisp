(in-package #:reasoner-term-info)

;; (declaim (optimize (speed 0) (safety 3) (debug 3)))
(declaim (optimize (speed 3) (safety 0) (debug 0)))
;; (declaim (inline primitive-term-equal))

;;;; What does a constraint look like?
;;;;
;;;; a full constraint is a hash-table with URI-OR-VAR as the HASH-KEY
;;;; and an `(:or ,@info)
;;;;
;;;; an info block will differ between variables and primitives due to
;;;; their position in the constraint.  we currently only support
;;;; specific values.  limitations from other constraints (for example
;;;; ?foo > 1000) are not supported.
;;;;
;;;; variable
;;;;   :forward-predicates ((uri-or-var . values))
;;;;   :backward-predicates ((uri-or-var . values))
;;;; primitive
;;;;   :backward-predicates ((uri-or-var . values))
;;;;
;;;; these constraints will later be dispatched to the predicates where
;;;; they'll be used for graph reasoning.
;;;;
;;;; a full constraint is a list of these constraints wrapped in an
;;;; `(:or) block.
;;;;
;;;; We represent alterantives (for instance, introduced through UNION)
;;;; but also need to represent an empty union.  As such, there must be
;;;; a difference between not having any constraints, and the
;;;; alternative of having no constraints.
;;;;
;;;; no constraints :: (:or nil)
;;;;
;;;; an option with no constraints and another with a constraint
;;;; (through a union) :: (:or nil ((var-a (:forward-predicates (uri-a
;;;; . nil)))))
;;;;
;;;; when joining constraints we add to the current constraints.  the
;;;; rules for merging constraint limitations:
;;;;
;;;; fixpoint limitation :: nil
;;;;
;;;; merging non-overlapping constraints :: append the constraints
;;;; (union '(:forward-predicates ((uri-a . nil))) '(:forward-predicates
;;;; ((uri-b . nil)))) would result in '(forward-predicates ((uri-a
;;;; . nil) (uri-b . nil)))
;;;;
;;;; merging overlapping constraints :: make a list of any of the
;;;; values, as each of the values must be present.
;;;;
;;;; (union '(:forward-predicates ((uri-a uri-x))) '(:forward-predicates
;;;; ((uri-a uri-y)))) would result in '(:forward-predicates ((uri-a
;;;; uri-x uri-y)))
;;;;
;;;; NOTE: that all values will be sorted based on PRIMITIVE-TERM-< and
;;;; hence URI-X must be before URI-Y.
;;;;
;;;;
;;;; Joining :or constraints
;;;;
;;;; This is the most common case of constraints.  We either join up, or
;;;; we join sideways, but we join nothing.  Joins tend to come from
;;;; multiple angles and it should be distributative (join (join a b) c)
;;;; should be the same as (join a (join b c)).
;;;;
;;;; joining empty matches :: (join (:or nil) (:or nil)) this is the
;;;; fixpoint and hence it must be (:or nil).
;;;;
;;;; joining a constraint with an empty match :: the fixpoint must be
;;;; ignorable, hence it must join to the other value: (join '(:or nil)
;;;; '(:or (uri-a (:forward-predicates ((uri-b . nil)))))) '(:or (uri-a
;;;; (:forward-predicates ((uri-b . nil)))))
;;;;
;;;; joining constraints with multiple alternatives in the :or :: for
;;;; this case the cross product must be made of each :or option, each
;;;; of these can be joined by the aforementioned strategies. (join
;;;; '(:or A B) '(:or C D)) must result in `(:or ,(join A C) ,(join A D)
;;;; ,(join B C) ,(join B D))
;;;;
;;;; simplification :: at any stage we may create multiple clauses that
;;;; express the same information.  any :or constraints that have the
;;;; same content can be folded.

(defparameter *match-term-info* (make-hash-table :test 'eq)
  "Term info options at clause MATCH.

The term info options are collections of constraints that hold at the key MATCH.")

(defmacro with-match-term-info (&body body)
  "Executes code-block with a scoped match-term-info block."
  `(let ((*match-term-info* (make-hash-table :test 'eq)))
     ,@body))

(defparameter *term-info-update-tracker* nil
  "Container for the changed matches.

When this exists, change-tracking for TERM-INFO of matches.")

(defmacro with-term-info-change-tracking (&body body)
  `(let ((*term-info-update-tracker* (make-hash-table :test 'eq)))
     ,@body))

(defun term-info-tracking-enabled ()
  "Whether term-info-tracking is enabled."
  (and *term-info-update-tracker* t))

(defun term-info-tracking-add (match &optional (tracker *term-info-update-tracker*))
  "Adds match to the items for which term-info-tracking is enabled."
  (setf (gethash match tracker) t))

(defun term-info-tracking-contains (match &optional (tracker *term-info-update-tracker*))
  "Adds match to the items for which term-info-tracking is enabled."
  (gethash match tracker))

(defun term-info-tracking-tracked-matches (&optional (tracker *term-info-update-tracker*))
  "Lists all items currently tracked."
  (and tracker (hash-table-keys tracker)))

(defun term-info-tracking-tracked-amount (&optional (tracker *term-info-update-tracker*))
  "Returns the amount of items currently being tracked."
  (length (term-info-tracking-tracked-matches tracker)))

(defun term-info-tracking-empty-p (&optional (tracker *term-info-update-tracker*))
  "Yields truethy iff the tracking table is empty."
  (loop for k being the hash-keys of tracker
        do (return nil)
        finally (return t)))

(defun term-info-tracking-get-current-tracker ()
  *term-info-update-tracker*)


(declaim (ftype (function (primitive-expression primitive-expression) (or null t)) primitive-term-equal primitive-term-<))
(defun primitive-term-equal (left right)
  "Compares two primitive terms as used by the reasoner."
  ;; this currently assumes a string as a value.  However, these could
  ;; have a different representation in the future instead.  String
  ;; comparison is sufficient for now.
  (string= left right))

(defun primitive-term-< (left right)
  "Compares two primitive terms as used by the reasoner."
  ;; like primitive-term-equal, this assumes both left and right are
  ;; strings.
  (string< left right))



(declaim (ftype (function (&rest reasoner-ast) (values term-info-rule &optional)) union-term-info))
(defun union-reasoner-asts (&rest reasoner-asts)
  "For each of the matches, ensure the constraints are pushed down into each match."
  ;; If there are multiple alternatives, they should be multiplied

  ;; In order to combine the knowledge, we have to multiply each of the
  ;; constraints.  This may explode.

  ;; TODO: Only pass on relevant knowledge by analyzing the intersting
  ;; bits early on, making this easier to analyze and substantially
  ;; easier to process with multiple UNIONs
  (loop for rast in (rest reasoner-asts)
        for current = (and (first reasoner-asts)
                           (reasoner-ast-term-info-list (first reasoner-asts)))
          then (join-term-info (and rast (reasoner-ast-term-info-list rast)) current)
        finally (return current)))

(declaim (ftype (function (reasoner-ast reasoner-ast reasoner-ast reasoner-ast)
                          (values))
                add-subject-predicate-object))
(defun add-subject-predicate-object (reasoner-ast subject predicate object)
  "Adds the SUBJECT PREDICATE OBJECT combination to the known knowledge of MATCH."
  ;; types of predicates:
  ;; - :forward-predicates
  ;; - :backward-predicates
  ;;
  ;; NOTE: the solution with also-set-backward and predicate-type is not
  ;; the cleanest approach.  we could refactor this someday.
  (let ((subject-string (or ; needs or because failure may be scanned-token instead
                         (reasoner-tree-mirror:tree-match-symbol-case subject
                           (ebnf::|ABSTRACT-IRI| (cached-expanded-uri (reasoner-tree-mirror:reasoner-ast-node subject))))
                         (sparql-generator:write-valid-match (reasoner-tree-mirror:reasoner-ast-node subject))))
        (predicate-string (sparql-generator:write-valid-match (reasoner-tree-mirror:reasoner-ast-node predicate)))
        (object-string (or
                        (reasoner-tree-mirror:tree-match-symbol-case object
                          (ebnf::|ABSTRACT-IRI| (cached-expanded-uri (reasoner-tree-mirror:reasoner-ast-node object))))
                        (sparql-generator:write-valid-match (reasoner-tree-mirror:reasoner-ast-node object)))))
    (add-primitive-subject-predicate-object reasoner-ast subject-string predicate-string object-string)
    (add-primitive-subject-predicate-object reasoner-ast subject-string predicate-string object-string t)))

(defun add-primitive-subject-predicate-object (reasoner-ast subject predicate object &optional inverse-direction)
  ;; (format t "~%Adding ~A ~:[~;(inverse)~])~% to ~A:~&" (list subject predicate object) inverse-direction (term-info reasoner-ast nil))
  (let ((source (if inverse-direction object subject))
        (target (if inverse-direction subject object))
        (direction-keyword (if inverse-direction :backward-predicates :forward-predicates)))
    (let ((term-info-directional (ensure-term-info-list-directional-cell reasoner-ast source))
          term-info-predicate-info)     ; set later
      (setf (term-info-directional-direction term-info-directional direction-keyword)
            (loop for predicate-info in (term-info-directional-direction term-info-directional direction-keyword)
                  if (primitive-term-equal (term-info-predicate-predicate predicate-info) predicate)
                    do
                       ;; pushing the target and sorting the values happens later
                       (setf term-info-predicate-info predicate-info)
                       (return (term-info-directional-direction term-info-directional direction-keyword))
                  finally (let ((predicate-info (make-term-info-predicate-info)))
                            (setf (term-info-predicate-predicate predicate-info) predicate)
                            (setf term-info-predicate-info predicate-info)
                            (return (cons predicate-info (term-info-directional-direction term-info-directional direction-keyword))))))
      ;; cope with sorting
      (loop for object in (term-info-predicate-targets term-info-predicate-info)
            when (primitive-term-equal object target)
              do (return)
            finally
               (push target (term-info-predicate-targets term-info-predicate-info))
               (mark-dirty reasoner-ast))))
  ;; (format t "~&  ~A~&" (term-info reasoner-ast nil))
  )

(defun mark-dirty (reasoner-ast)
  "Marks the reasoner-ast dirty and updates whatever state is needed to
reflect that."
  (unless (reasoner-ast-dirty-p reasoner-ast)
    (setf (reasoner-ast-dirty-p reasoner-ast) t)
    (when (term-info-tracking-enabled)
      (term-info-tracking-add reasoner-ast))))


;;;; term-info-directional
(defun make-term-info-directional (&optional forward-predicates backward-predicates)
  (cons forward-predicates backward-predicates))

(defun term-info-directional-forward (term-info-directional)
  (car term-info-directional))
(defun (setf term-info-directional-forward) (value term-info-directional)
  (setf (car term-info-directional) value))

(defun term-info-directional-backward (term-info-directional)
  (cdr term-info-directional))
(defun (setf term-info-directional-backward) (value term-info-directional)
  (setf (cdr term-info-directional) value))

(defun term-info-directional-direction (term-info-directional direction-keyword)
  (case direction-keyword
    (:forward-predicates (term-info-directional-forward term-info-directional))
    (:backward-predicates (term-info-directional-backward term-info-directional))
    (otherwise (error "direction-keyword must be one of :forward-predicates or :backward-predicates"))))
(defun (setf term-info-directional-direction) (value term-info-directional direction-keyword)
  (case direction-keyword
    (:forward-predicates (setf (term-info-directional-forward term-info-directional) value))
    (:backward-predicates (setf (term-info-directional-backward term-info-directional) value))
    (otherwise (error "direction-keyword must be one of :forward-predicates or :backward-predicates"))))

(defun ensure-term-info-list-directional-cell (reasoner-ast term)
  "Ensures the term-info-list cell exists for term exists in reasoner-ast and returns it.

Ensures a single :OR solution exists with at least a cell for the
predicate TERM.  Returns the on which :forward-predicates and
:backward-predicates can be set for the predicate TERM."

  ;; TODO: put predicate cell at the right spot by sorting
  ; rest to skip over first :OR key.
  (let ((current-condition (second (reasoner-ast-term-info-list reasoner-ast))))
    ;; the current condition is either nil, or is the only option of the
    ;; :OR clause because this function only understands a single
    ;; element.
    (loop for (source cell) on current-condition
            by #'cddr
          when (primitive-term-equal source term)
            do (return cell)
          finally (let ((cell (make-term-info-directional)))
                    (setf (reasoner-ast-term-info-list reasoner-ast)
                          `(:or (,term ,cell ,@current-condition)))
                    (return cell)))))

;; ;;;; term-info-predicate-cell
;; (defun make-term-info-predicate-cell (&optional predicate term-info-list)
;;   (cons predicate term-info-list))
;; (defun term-info-predicate-predicate (term-info-predicate-cell)
;;   (car term-info-predicate-cell))
;; (defun term-info-predicate-info-list (term-info-predicate-cell)
;;   (cdr term-info-predicate-cell))

;;;; term-info-predicates-info
(defun make-term-info-predicate-info (&key source targets)
  (cons source targets))

(defun term-info-predicate-predicate (term-info-predicate-info)
  (car term-info-predicate-info))
(defun (setf term-info-predicate-predicate) (value term-info-predicate-info)
  (setf (car term-info-predicate-info) value))

(defun term-info-predicate-targets (term-info-predicate-info)
  (cdr term-info-predicate-info))
(defun (setf term-info-predicate-targets) (value term-info-predicate-info)
  (setf (cdr term-info-predicate-info) value))


(defun cross-product* (left-list right-list functor)
  "Constructs the cross-product of LEFT-LIST and RIGHT-LIST by calling
FUNCTOR with a permutation of the elements in both."
  (loop for left in left-list
        append (loop for right in right-list
                     collect (funcall functor left right))))

(defmacro make-cross-product ((left-var right-var) (left-list right-list) &body body)
  "Creates the cross-product of LEFT-LIST and RIGHT-LIST calling BODY with LEFT-VAR bound to each permutation of an element of LEFT-LIST bound to LEFT-VAR and an element of RIGHT-LIST bound to RIGHT-VAR."
  `(cross-product* ,left-list ,right-list (lambda (,left-var ,right-var) ,@body)))

(declaim (ftype (function (&rest term-info-rule) (values term-info-rule)) join-or-term-info-statements))
(defun join-or-term-info-statements (&rest term-info-rules)
  "Joins a set of :OR term-info statements by combining them all into a big
OR."
  (let ((current (first term-info-rules)))
    (loop for rule in (rest term-info-rules)
          unless (and (eq (first rule) :or)
                      (eq (first current) :or))
            do
               (error "~A or ~A does not start with :or" current rule)
          do (setf current
                   (make-cross-product (left right)
                       ((rest current) (rest rule))
                     (pick-lists left right
                                 :pick (lambda (left right)
                                         (cond ((primitive-term-equal (first left) (first right))
                                                :both)
                                               ((primitive-term-< (first left) (first right))
                                                :left)
                                               (t :right)))
                                 :single (lambda (x) x)
                                 :double (lambda (a b) `(,(first a) ,@(merge-predicate-infos (rest a) (rest b))))))))
    current))

(defun merge-predicate-infos (left-term-info-directional right-term-info-directional)
  ;; {DIRECTIONAL ((source . targets) (source . targets)) ((source . targets) (source . targets))}
  (flet ((merge-direction (lefts rights)
           (pick-lists lefts rights
                       :pick (lambda (left right)
                               (cond ((primitive-term-< (car left) (car right))
                                      :left)
                                     ((primitive-term-equal (car left) (car right))
                                      :both)
                                     (t :right)))
                       :single (lambda (x) x)
                       :double (lambda (x y) (cons (first x) (merge-primitive-lists (rest x) (rest y)))))))
    (let ((forward (merge-direction (term-info-directional-forward left-term-info-directional)
                                    (term-info-directional-forward right-term-info-directional)))
          (backward (merge-direction (term-info-directional-backward left-term-info-directional)
                                     (term-info-directional-backward right-term-info-directional))))
      (make-term-info-directional forward backward))))

(defun merge-primitive-lists (left right)
  "Merges primitive lists in the right order."
  (pick-lists left right
              :pick (lambda (x y)
                      (cond ((primitive-term-< x y) :left)
                            ((primitive-term-< y x) :right)
                            (t :both)))
              :single (lambda (x) x)
              :double (lambda (x y) (declare (ignore y)) x)))













(declaim (ftype (function (reasoner-tree-mirror:reasoner-ast &optional term-info-collection) term-info-rule) term-info-rule))
(defun term-info (ast &optional (default (list :or nil)))
  "Yields known term information at MATCH.

These are options of constraints that we know of at MATCH.  They are
distributed amongst matches.

Returns two values, the first being the term-info and the second
indicating whether term info was available."
  (or (reasoner-tree-mirror:reasoner-ast-term-info-list ast) default)
  ;; (gethash match *match-term-info* default)
  )

;; (declaim (ftype (function (term-info-rule sparql-parser:match &optional term-info-rule) (values)) (setf term-info)))
(declaim (ftype (function (t reasoner-tree-mirror:reasoner-ast &optional t) (values)) (setf term-info)))
(defun (setf term-info) (value ast &optional (default (list :or nil)))
  "Sets the term-info for VALUE"
  (declare (ignore default))
  (when (term-info-tracking-enabled)
    (unless (term-info-rule-equal value (term-info ast))
      (term-info-tracking-add ast)))
  (setf (reasoner-tree-mirror:reasoner-ast-term-info-list ast) value)
  ;; (setf (gethash match *match-term-info* default) value)
  (values))

(declaim (ftype (function (term-info-rule term-info-rule) (member nil t))))
(defun term-info-rule-equal (left-term-info-rule right-term-info-rule)
  "Compares the term-info for MATCH with NEW-VALUE, yielding truethy iff
the information differs."
  ;; TODO: make more performant by using sorted tables
  (if (or (not (eq (first left-term-info-rule) :or))
          (not (eq (first right-term-info-rule) :or)))
      (error "Only :or combinations understood")
      (set-equal
       (rest left-term-info-rule) (rest right-term-info-rule)
       :test (lambda (left-table right-table)
               (let ((left-primitives (term-info-primitives left-table))
                     (right-primitives (term-info-primitives right-table)))
                 (and (set-equal left-primitives right-primitives
                                 :test #'primitive-term-equal)
                      (loop for primitive
                              in (union left-primitives right-primitives :test #'primitive-term-equal)
                            for left-directional = (term-info-primitive-directional left-table primitive)
                            for right-directional = (term-info-primitive-directional right-table primitive)
                            unless (flet 
                                       ((compare-directional-set (left-set right-set)
                                          (set-equal left-set right-set
                                           :test (lambda (left-predicate right-predicate)
                                                   (and (primitive-term-equal (first left-predicate)
                                                                              (first right-predicate))
                                                        (set-equal (rest left-predicate) (rest right-predicate)
                                                                   :test #'primitive-term-equal))))))
                                     (and (compare-directional-set (term-info-directional-forward left-directional)
                                                                   (term-info-directional-forward right-directional))
                                          (compare-directional-set (term-info-directional-backward left-directional)
                                                                   (term-info-directional-backward right-directional))))
                              do (return nil)
                            finally (return t))))))))

(declaim (ftype (function (reasoner-tree-mirror:reasoner-ast) term-info-rule) ensure-term-info))
(defun ensure-term-info (ast)
  "Ensures term-info has a setting for MATCH and returns it."
  (multiple-value-bind (value foundp)
      (term-info ast)
    (unless foundp
      (setf (term-info ast) value))
    value))

(defun print-term-info (ast &optional stream)
  "Prints TERM-INFO for MATCH on STREAM"
  (format stream ":or~{|~{~{~& ~A~{~&  -> ~{~A~,^ ~}~}~{~&  <- ~{~A~,^ ~}~}~}~}~,^~&~}"
          (loop for info in (rest (term-info ast))
                collect
                (loop for k in (term-info-primitives info)
                      for directional-info = (term-info-primitive-directional info k)
                      collect (list k
                                    (term-info-directional-forward directional-info)
                                    (term-info-directional-backward directional-info))))))

(declaim (ftype (function (term-info) (values primitive-predicate-list &optional)) term-info-primitives))
(defun term-info-primitives (term-info)
  "Yields all primitives known in term-info."
  (loop for k in term-info by #'cddr collect k))

(declaim (ftype (function (term-info primitive-source) (values (or null term-info-directional) &optional)) term-info-primitive-directional))
(defun term-info-primitive-directional (term-info primitive)
  "Yields the TERM-INFO-DIRECTIONAL belonging to TERM-INFO for PRIMITIVE."
  (loop for (k v) on term-info by #'cddr
        if (primitive-term-equal k primitive)
          return v))

;; (declaim (ftype (function (term-info-directional) (values term-info-predicates-info &optional)) term-info-forward-predicates term-info-backward-predicates))
(defun term-info-forward-predicates (term-info-directional)
  "Forward predicate info for TERM-INFO"
  (term-info-directional-predicates term-info-directional :forward-predicates))

;; (defun term-info-backward-predicates (term-info-directional)
;;   "Forward predicate info for TERM-INFO"
;;   (term-info-directional-predicates term-info-directional :backward-predicates))

(defmacro update-term-info ((reasoner-ast ;; match
                                &key var wrap-in-or) &body body)
  "Update term-info of MATCH to the result of BODY.

Optionally assigns the current value to VAR within the scope of BODY.
The resulting list is prefixed with :OR when WRAP-IN-OR is non-nil."
  (let ((reasoner-ast-sym (gensym "MATCH")))
    `(let ((,reasoner-ast-sym ,reasoner-ast))
       (setf (term-info ,reasoner-ast-sym)
             (append ,(when wrap-in-or '(list :or))
                     ,@(if var
                           `((let ((,var (term-info ,reasoner-ast-sym))) ,@body))
                           body))))))

(defmacro with-term-info-source ((term-info primitive-string &key var) &body body)
  "Executes body with  the given source, storing the current binding in var."
  `(let ((,var (term-info-primitive-directional ,term-info ,primitive-string)))
     ,@body))

(defmacro update-term-info-source ((term-info primitive-string &key var) &body body)
  "Executes body with  the given source, storing the current binding in var."
  `(let ((,var (term-info-primitive-directional ,term-info ,primitive-string)))
     ,@body))


;; (declaim (ftype (function (reasoner-ast reasoner-ast reasoner-ast reasoner-ast
;;                            &optional
;;                            (or null t)
;;                            predicate-direction-key)
;;                           (values))
;;                 add-subject-predicate-object))
;; (defun add-subject-predicate-object (tree subject predicate object &optional (also-set-backward t) (predicate-type :forward-predicates))
;;   "Adds the SUBJECT PREDICATE OBJECT combination to the known knowledge of TREE."
;;   ;; types of predicates:
;;   ;; - :forward-predicates
;;   ;; - :backward-predicates
;;   ;;
;;   ;; NOTE: the solution with also-set-backward and predicate-type is not
;;   ;; the cleanest approach.  we could refactor this someday.
;;   (let ((subject-string (or ; needs or because failure may be scanned-token instead
;;                          (sparql-manipulation:match-symbol-case subject 
;;                            (ebnf::|ABSTRACT-IRI| (cached-expanded-uri (reasoner-tree-mirror:reasoner-ast-node subject))))
;;                          (sparql-generator:write-valid-match (reasoner-tree-mirror:reasoner-ast-node subject))))
;;         (predicate-string (sparql-generator:write-valid-match (reasoner-tree-mirror:reasoner-ast-node predicate)))
;;         (object-string (or
;;                         (sparql-manipulation:match-symbol-case object
;;                           (ebnf::|ABSTRACT-IRI| (cached-expanded-uri (reasoner-tree-mirror:reasoner-ast-node object))))
;;                         (sparql-generator:write-valid-match (reasoner-tree-mirror:reasoner-ast-node object)))))
;;     ;; ;; information must be added to each term-info and we will construct a new term-info-rule for this
;;     ;; (update-term-info (tree :var term-info-rule :wrap-in-or t)
;;     ;;   (loop for term-info in (rest term-info-rule) ;; assume first is :or
;;     ;;         collect
;;     ;;         (update-term-info-source
;;     ;;          (term-info subject-string :var primitive-directional)
;;     ;;          )
;;     ;;         )))

;;     ;; subject exists with :forward-perdicates
;;     (unless (assoc predicate-string
;;                    (getf (gethash subject-string
;;                                   (second (term-info tree)))
;;                          predicate-type)
;;                    :test #'primitive-term-equal)
;;       ;; subject exists but predicate is not known
;;       (push (list predicate-string)
;;             (getf (gethash subject-string
;;                            (second (ensure-term-info tree)))
;;                   predicate-type)))
;;     ;; we now know the subject-predicate combination exists
;;     (let ((predicate-cell (assoc predicate-string
;;                                  (getf (gethash subject-string
;;                                                 (second (term-info tree)))
;;                                        predicate-type)
;;                                  :test #'primitive-term-equal)))
;;       (if predicate-cell
;;           (unless (find object-string (rest predicate-cell) :test #'primitive-term-equal)
;;             (setf (cdr (last predicate-cell)) (list object-string)))
;;           (push (list predicate object-string)
;;                 (getf (gethash subject-string
;;                                (second (ensure-term-info tree)))
;;                       predicate-type))))
;;     ;; if object represents an iri or a variable, we must set the backward-predicates too
;;     ;; TODO: support RDFLiteral
;;     (when also-set-backward
;;       (sparql-manipulation:match-symbol-case object
;;         (ebnf::|ABSTRACT-IRI| (add-subject-predicate-object tree object predicate subject nil :backward-predicates))
;;         (ebnf::|ABSTRACT-VAR| (add-subject-predicate-object tree object predicate subject nil :backward-predicates))
;;         (ebnf::|ABSTRACT-PRIMITIVE| nil
;;                ;; (format t "~&Not defining inverse predicate for primitive ~A~%"
;;                ;;         object-string)
;;                )
;;         (t (warn "Received an unknown type of value in REASONER-TERM-INFO:ADD-SUBJECT-PREDICATE-OBJECT ~A" object))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; uninioning term infos
(declaim (ftype (function (term-info-predicates-info term-info-predicates-info) (values term-info-predicates-info &optional)) join-constraint-primitive-predicates-constraint))
(defun join-constraint-primitive-predicates-constraint (left-predicates right-predicates)
  "Joins :forward-predicates or :backward-predicates."
  ;; We know both have the form ((str str str ...) (str) ...)
  (let* ((predicate-groups (group-by (append left-predicates right-predicates)
                                     #'primitive-term-equal
                                     :key #'first)))
    (loop for predicates in predicate-groups
          for key = (caar predicates)
          for objects = (remove-duplicates
                         (loop for (pred . values) in predicates
                               append values)
                         :test #'primitive-term-equal)
          collect (cons key objects))))

(declaim (ftype (function ((or null term-info-directional) (or null term-info-directional)) (values term-info-directional &optional)) join-constraint-primitives))
(defun join-constraint-primitives (left right)
  "Joins the constraint primitives of two primitves which have the same subject."
  ;; Both have a form like 
  ;; (:forward-predicates ((str str) (str))
  ;;  :backward-predicates ((str) (str str str)))
  (make-term-info-directional
   (cond ((and left right)
          (join-constraint-primitive-predicates-constraint
           (term-info-directional-forward left)
           (term-info-directional-forward right)))
         (left (term-info-directional-forward left))
         (right (term-info-directional-forward right))
         (t nil))
   (cond ((and left right)
          (join-constraint-primitive-predicates-constraint
           (term-info-directional-backward left)
           (term-info-directional-backward right)))
         (left (term-info-directional-backward left))
         (right (term-info-directional-backward right))
         (t nil))))

(declaim (ftype (function (term-info term-info) (values term-info &optional)) join-or-constraints-2))
(defun join-or-constraints-2 (left right)
  "Joins two term constraints."
  ;; Both have a plist form like:
  ;;
  ;; { uri-a => (:forward-predicates ((uri-q uri-r))
  ;;             :backward-predicates ((uri-s)))
  ;;   uri-b => (:forward-predicates ((uri-t)))) }
  ;;
  ;; This function searches to combine the solutions of each uri from
  ;; left into right and vice-versa
  (loop for key in (union (term-info-primitives left)
                          (term-info-primitives right)
                          :test #'primitive-term-equal)
        for left-value = (term-info-primitive-directional left key)
        for right-value = (term-info-primitive-directional right key)
        for combined-value = (join-constraint-primitives left-value right-value)
        append (list key combined-value)))

(declaim (ftype (function (term-info-rule term-info-rule) term-info-rule)))
(defun join-or-constraints (left right)
  "Joins two or constraints by creating a data structure which combines the
knowledge of both in a single :or constraint."
  ;; Both have a form like:
  ;; (:or { uri-a => (:forward-predicates ((uri-q uri-r))
  ;;                  :backward-predicates ((uri-s)))
  ;;        uri-b => (:forward-predicates ((uri-t))) }
  ;;      { uri-a => (:forward-predicates ((uri-u))) })
  (let ((left-alternatives (rest left))
        (right-alternatives (rest right)))
    (fold-or-constraint
     `(:or ,@(loop for left-alternative in left-alternatives
                   append (loop for right-alternative in right-alternatives
                                collect (join-or-constraints-2 left-alternative right-alternative)))))))

(declaim (ftype (function (term-info-rule) term-info-rule)))
(defun fold-or-constraint (constraint)
  "Folds an or constraint which may have multiple patterns into the
simplest form that represents the same contsraints."
  ;; TODO: implement constraint folding
  constraint)

(declaim (ftype (function (term-info-rule-list) term-info-rule) union-term-info-for-or-constraints))
(defun union-term-info-for-or-constraints (or-constraints)
  "unions a set of or-constraints, combining them be moving knowledge across."
  ;; (format t "~&Calculating u-t-i-f-o-c for ~A ..." or-constraints)
  (let ((result
          (loop for new-constraint in or-constraints
                for constraint = new-constraint
                  then (join-or-constraints constraint new-constraint)
                finally (return constraint))))
    ;; (format t " DONE~&")
    ;; (format t " yielded ~A~%" result)
    result))

(declaim (ftype (function (&rest reasoner-tree-mirror:reasoner-ast) (values term-info-rule &optional)) union-term-info))
(defun union-term-info (&rest trees)
  "For each of the matches, ensure the constraints are pushed down into each match."
  ;; If there are multiple alternatives, they should be multiplied

  ;; In order to combine the knowledge, we have to multiply each of the
  ;; constraints.  This may explode.

  ;; TODO: Only pass on relevant knowledge by analyzing the intersting
  ;; bits early on, making this easier to analyze and substantially
  ;; easier to process with multiple UNIONs
  (union-term-info-for-or-constraints
   (mapcar (lambda (tree)
             (term-info tree (list :or nil)))
           trees)))

;; (declaim (ftype (function (&rest term-info-rule) (values term-info-rule)) join-or-term-info-statements))
;; (defun join-or-term-info-statements (&rest term-info-statements)
;;   "Joins a set of :OR term-info statements by combining them all into a big
;; OR."
;;   `(:or ,@(loop
;;             for (or . sub-statements) in term-info-statements
;;             if (eq or :or)
;;               append sub-statements
;;             else
;;               do (error "Can only join :or info statements"))))
