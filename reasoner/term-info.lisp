(in-package #:reasoner-term-info)

;; (declaim (optimize (speed 0) (debug 3)))
(declaim (optimize (speed 3) (safety 0) (debug 0)))

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

;;;; Effective storage model
;;;;
;;;; Top-level there is a hash-table to contain the information of each
;;;; MATCH.  This is called a term-info-collection.
;;;;
;;;; term-info-collection :: HASH-TABLE match -> term-info-rule
;;;; term-info-rule :: '(:or term-info-1 term-info-2)
;;;; term-info :: HASH-TABLE primitive-subject ->
;;;;                           :forward-predicates ((pred-1 obj*) (pred-2 obj*))
;;;;                           :backward-predicates ((pred-3 obj*) (pred-4 obj*))


;; high-level to low-level
(with-derived-types ((term-info-collection-table (typed-hash-table match term-info-rule)))
  (deftype term-info-collection ()
    "Whole collection of information on all MATCH statements."
    term-info-collection-table))

(with-derived-types ((term-info-list (typed-list term-info)))
  (deftype term-info-rule ()
    "A single :or rule, currently one exists for a MATCH statement."
    `(cons (member :or) ,term-info-list)))

(with-derived-types ((term-info-hash-table (typed-hash-table primitive-expression term-info-directional)))
 (deftype term-info ()
   "A set of constraints holding within one :or statement."
   term-info-hash-table))

(with-derived-types ((term-info-directional-plist (typed-plist predicate-direction-key term-info-predicates-info :expand-length 2)))
 (deftype term-info-directional ()
   "Directional term info is the place that stores the links to forward and backward predicates."
   term-info-directional-plist))

(with-derived-types ((term-info-predicate-info-list (typed-list term-info-predicate-info)))
  (deftype term-info-predicates-info ()
    "List of predicates and their corresponding values.  This is stored as an alist."
    term-info-predicate-info-list))

(deftype term-info-predicate-info ()
  "Information about an individual predicate."
  '(cons primitive-predicate term-info-target-info))

(with-derived-types ((primitive-target-list (typed-list primitive-target)))
  (deftype term-info-target-info ()
    "Information we have on the target of term-info."
    primitive-target-list))

;; basics
(deftype primitive-expression ()
  'string)

(deftype primitive-source ()
  "How we represent the source of a term-info."
  'primitive-expression)

(deftype primitive-predicate ()
  "How we represent the predicate of a term-info."
  'primitive-expression)

(deftype primitive-target ()
  "How we represent the target of a term-info."
  'primitive-expression)

(deftype predicate-direction-key ()
  "Keys used to indicate the key of type of info we have on a subject."
  '(member :forward-predicates :backward-predicates))


;; (deftype term-info-collection ()
;;   '(typed-hash-table match ))

;; (defun hash-table-list-p (list)
;;   "Truethy iff LIST contains only elements of type HASH-TABLE."
;;   (every #'hash-table-p list))

;; (defun term-info-collection-match-hash-table-p (hash-table)
;;   (hash-table-has-types-p hash-table 'match 'term-info))

;; (defun term-info-collection-match-hash-table-list-p (hash-table-list)
;;   (every (lambda (hash-table)
;;            (term-info-collection-match-hash-table-p hash-table))
;;          hash-table-list))

;; (deftype term-info-collection ()
;;   '(cons
;;     (member :or)
;;     (satisfies term-info-collection-match-hash-table-list
;;      ;; hash-table-list-p
;;      )))

(with-derived-types ((primitive-expression-list (typed-list primitive-target)))
  (deftype term-info-collection-predicate-list ()
    "List of predicates known by term-info-collection."
    primitive-expression-list)

  (deftype term-info-predicate-values ()
    "Values describing the predicate information."
    primitive-expression-list))

(with-derived-types ((primitive-predicate-list (typed-list primitive-predicate)))
  (deftype primitive-predicate-list ()
    primitive-predicate-list))

;; (defun string-list-p (list)
;;   "Truethy iff LIST contains only strings."
;;   (every #'stringp list))

;; (deftype string-list ()
;;   "List of strings."
;;   '(satisfies string-list-p))

;; (deftype term-info-predicate-values ()
;;   "Values describing the predicate information."
;;   'string-list)

(deftype term-info-predicate-direction-key ()
  "Keys used for predicate direction.
Either forward-predicates or backward-predicates."
  '(member :forward-predicates :backward-predicates))

;; (deftype term-info-predicate ()
;;   "Information we have to describe predicates."
;;   '(cons string term-info-predicate-values))

;; (deftype term-info ()
;;   '(member nil
;;     (cons term-info-predicate-direction-key
;;      (cons term-info-predicate
;;       (member nil
;;        (cons term-info-predicate-direction-key
;;         (cons term-info-predicate nil)))))))

(defparameter *match-term-info* (make-hash-table :test 'eq)
  "Term info options at clause MATCH.

The term info options are collections of constraints that hold at the key MATCH.")

(defmacro with-match-term-info (&body body)
  "Executes code-block with a scoped match-term-info block."
  `(let ((*match-term-info* (make-hash-table :test 'eq)))
     ,@body))

(declaim (ftype (function (match &optional term-info-collection) term-info-rule) term-info-rule))
(defun term-info (match &optional (default (list :or (make-hash-table :test 'equal))))
  "Yields known term information at MATCH.

These are options of constraints that we know of at MATCH.  They are
distributed amongst matches.

Returns two values, the first being the term-info and the second
indicating whether term info was available."
  (gethash match *match-term-info* default))

;; (declaim (ftype (function (term-info-rule sparql-parser:match &optional term-info-rule) (values)) (setf term-info)))
(declaim (ftype (function (t sparql-parser:match &optional t) (values)) (setf term-info)))
(defun (setf term-info) (value match &optional (default (list :or (make-hash-table :test 'equal))))
  "Sets the term-info for VALUE"
  (setf (gethash match *match-term-info* default) value)
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
                                          (set-equal
                                           (term-info-forward-predicates left-set)
                                           (term-info-forward-predicates right-set)
                                           :test (lambda (left-predicate right-predicate)
                                                   (and (primitive-term-equal (first left-predicate)
                                                                              (first right-predicate))
                                                        (set-equal (rest left-predicate) (rest right-predicate)
                                                                   :test #'primitive-term-equal))))))
                                     (and (compare-directional-set (term-info-forward-predicates left-directional)
                                                                   (term-info-forward-predicates right-directional))
                                          (compare-directional-set (term-info-backward-predicates left-directional)
                                                                   (term-info-backward-predicates right-directional))))
                              do (return nil)
                            finally (return t))))))))

(declaim (ftype (function (match) term-info-rule) ensure-term-info))
(defun ensure-term-info (match)
  "Ensures term-info has a setting for MATCH and returns it."
  (multiple-value-bind (value foundp)
      (term-info match)
    (unless foundp
      (setf (term-info match) value))
    value))

(defun print-term-info (match &optional stream)
  "Prints TERM-INFO for MATCH on STREAM"
  (format stream ":or~{|~{~{~& ~A~{~&  -> ~{~A~,^ ~}~}~{~&  <- ~{~A~,^ ~}~}~}~}~,^~&~}"
          (loop for info in (rest (term-info match))
                collect
                (loop for k being the hash-keys of info
                      collect (list k
                                    (getf (gethash k info) :forward-predicates)
                                    (getf (gethash k info) :backward-predicates))))))

(declaim (ftype (function (term-info) (values primitive-predicate-list &optional)) term-info-primitives))
(defun term-info-primitives (term-info)
  "Yields all primitives known in term-info."
  (hash-table-keys term-info))

(declaim (ftype (function (term-info primitive-source) (values term-info-directional &optional)) term-info-primitive-directional))
(defun term-info-primitive-directional (term-info primitive)
  "Yields the TERM-INFO-DIRECTIONAL belonging to TERM-INFO for PRIMITIVE."
  (values (gethash primitive term-info)))

(declaim (ftype (function (term-info-directional term-info-predicate-direction-key)
                          (values term-info-collection-predicate-list &optional))
                term-info-predicates))
(defun term-info-directional-predicates (term-info-directional predicate-key)
  "Predicates info for TERM-INFO."
  (getf term-info-directional predicate-key))

(declaim (ftype (function (term-info-directional) (values term-info-predicate-values &optional)) term-info-forward-predicates term-info-backward-predicates))
(defun term-info-forward-predicates (term-info-directional)
  "Forward predicate info for TERM-INFO"
  (term-info-predicates term-info-directional :forward-predicates))

(defun term-info-backward-predicates (term-info-directional)
  "Forward predicate info for TERM-INFO"
  (term-info-predicates term-info-directional :backward-predicates))

(declaim (ftype (function (match (or match scanned-token) (or match scanned-token) (or match scanned-token) &optional (or null t) predicate-direction-key)
                          (values))
                add-subject-predicate-object))
(defun add-subject-predicate-object (match subject predicate object &optional (also-set-backward t) (predicate-type :forward-predicates))
  "Adds the SUBJECT PREDICATE OBJECT combination to the known knowledge of MATCH."
  ;; types of predicates:
  ;; - :forward-predicates
  ;; - :backward-predicates
  ;;
  ;; NOTE: the solution with also-set-backward and predicate-type is not
  ;; the cleanest approach.  we could refactor this someday.
  (let ((subject-string (or ; needs or because failure may be scanned-token instead
                         (sparql-manipulation:match-symbol-case subject 
                           (ebnf::|ABSTRACT-IRI| (cached-expanded-uri subject)))
                         (sparql-generator:write-valid-match subject)))
        (predicate-string (sparql-generator:write-valid-match predicate))
        (object-string (or
                        (sparql-manipulation:match-symbol-case object
                          (ebnf::|ABSTRACT-IRI| (cached-expanded-uri object)))
                        (sparql-generator:write-valid-match object))))
    ;; subject exists with :forward-perdicates
    (unless (assoc predicate-string
                   (getf (gethash subject-string
                                  (second (term-info match)))
                         predicate-type)
                   :test #'primitive-term-equal)
      ;; subject exists but predicate is not known
      (push (list predicate-string)
            (getf (gethash subject-string
                           (second (ensure-term-info match)))
                  predicate-type)))
    ;; we now know the subject-predicate combination exists
    (let ((predicate-cell (assoc predicate-string
                                 (getf (gethash subject-string
                                                (second (term-info match)))
                                       predicate-type)
                                 :test #'primitive-term-equal)))
      (if predicate-cell
          (unless (find object-string (rest predicate-cell) :test #'primitive-term-equal)
            (setf (cdr (last predicate-cell)) (list object-string)))
          (push (list predicate object-string)
                (getf (gethash subject-string
                               (second (ensure-term-info match)))
                      predicate-type))))
    ;; if object represents an iri or a variable, we must set the backward-predicates too
    ;; TODO: support RDFLiteral
    (when also-set-backward
      (sparql-manipulation:match-symbol-case object
        (ebnf::|ABSTRACT-IRI| (add-subject-predicate-object match object predicate subject nil :backward-predicates))
        (ebnf::|ABSTRACT-VAR| (add-subject-predicate-object match object predicate subject nil :backward-predicates))
        (ebnf::|ABSTRACT-PRIMITIVE| nil
               ;; (format t "~&Not defining inverse predicate for primitive ~A~%"
               ;;         object-string)
               )
        (t (warn "Received an unknown type of value in REASONER-TERM-INFO:ADD-SUBJECT-PREDICATE-OBJECT ~A" object))))))

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

(declaim (ftype (function (term-info-directional term-info-directional) (values term-info-directional &optional)) join-constraint-primitives))
(defun join-constraint-primitives (left right)
  "Joins the constraint primitives of two primitves which have the same subject."
  ;; Both have a form like 
  ;; (:forward-predicates ((str str) (str))
  ;;  :backward-predicates ((str) (str str str)))
  (list :forward-predicates
        (join-constraint-primitive-predicates-constraint
         (getf left :forward-predicates)
         (getf right :forward-predicates))
        :backward-predicates
        (join-constraint-primitive-predicates-constraint
         (getf left :backward-predicates)
         (getf right :backward-predicates))))

(declaim (ftype (function (term-info term-info) (values term-info &optional)) join-or-constraints-2))
(defun join-or-constraints-2 (left right)
  "Joins two term constraints."
  ;; Both have a hash-table form like:
  ;;
  ;; { uri-a => (:forward-predicates ((uri-q uri-r))
  ;;             :backward-predicates ((uri-s)))
  ;;   uri-b => (:forward-predicates ((uri-t)))) }
  ;;
  ;; This function searches to combine the solutions of each uri from
  ;; left into right and vice-versa
  (let ((result (make-hash-table :test 'equal)))
    (loop for key in (union (alexandria:hash-table-keys left)
                            (alexandria:hash-table-keys right)
                            :test #'primitive-term-equal)
          for left-value = (gethash key left)
          for right-value = (gethash key right)
          for combined-value = (join-constraint-primitives left-value right-value)
          do
             (setf (gethash key result) combined-value))
    result))

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

(with-derived-types ((term-info-rule-list (typed-list term-info-rule)))
  (deftype term-info-rule-list ()
    term-info-rule-list))

(declaim (ftype (function (term-info-rule-list) term-info-rule) union-term-info-for-or-constraints))
(defun union-term-info-for-or-constraints (or-constraints)
  "unions a set of or-constraints, combining them be moving knowledge across."
  (loop for new-constraint in or-constraints
        for constraint = new-constraint
          then (join-or-constraints constraint new-constraint)
        finally (return constraint)))

(declaim (ftype (function (&rest match) (values term-info-rule &optional)) union-term-info))
(defun union-term-info (&rest matches)
  "For each of the matches, ensure the constraints are pushed down into each match."
  ;; If there are multiple alternatives, they should be multiplied

  ;; In order to combine the knowledge, we have to multiply each of the
  ;; constraints.  This may explode.

  ;; TODO: Only pass on relevant knowledge by analyzing the intersting
  ;; bits early on, making this easier to analyze and substantially
  ;; easier to process with multiple UNIONs
  (union-term-info-for-or-constraints
   (mapcar (lambda (match)
             (term-info match (list :or (make-hash-table :test 'equal))))
           matches)))

(declaim (ftype (function (&rest term-info-rule) (values term-info-rule)) join-or-term-info-statements))
(defun join-or-term-info-statements (&rest term-info-statements)
  "Joins a set of :OR term-info statements by combining them all into a big
OR."
  `(:or ,@(loop
            for (or . sub-statements) in term-info-statements
            if (eq or :or)
              append sub-statements
            else
              do (error "Can only join :or info statements"))))
