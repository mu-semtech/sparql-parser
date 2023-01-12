(in-package #:reasoner-term-info)

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

;; (with-derived-types ((term-info-hash-table (typed-hash-table primitive-expression term-info-directional)))
;;  (deftype term-info ()
;;    "A set of constraints holding within one :or statement."
;;    term-info-hash-table))

(with-derived-types ((term-info-directional-plist (typed-plist primitive-expression term-info-directional :expand-length 4)))
  (deftype term-info ()
   "A set of constraints holding within one :or statement."
   term-info-directional-plist))

;; (with-derived-types ((term-info-directional-plist (typed-plist predicate-direction-key term-info-predicates-info :expand-length 2)))
;;  (deftype term-info-directional ()
;;    "Directional term info is the place that stores the links to forward and backward predicates."
;;    term-info-directional-plist))
(deftype term-info-directional ()
  "Directional term info is the place that stores the links to forward and backward predicates."
  '(cons (or null term-info-predicates-info)
         (or null term-info-predicates-info)))


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

(deftype term-info-predicate-direction-key ()
  "Keys used for predicate direction.
Either forward-predicates or backward-predicates."
  '(member :forward-predicates :backward-predicates))

(with-derived-types ((term-info-rule-list (typed-list term-info-rule)))
  (deftype term-info-rule-list ()
    term-info-rule-list))
