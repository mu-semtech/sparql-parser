(in-package #:match-equality)

;; given the API of define-kind-matcher and define-comparator, we can later make this more efficient by using numbers instead.  We will define the comparison system separately from the comparators.

;; define-kind-matcher will always yield t or nil

(defparameter *matchers* nil
  "List of matchers against which a type can be matched.  First match is chosen.")

(defun add-kind-matcher (functor)
  "Adds a kind-matcher to the known matchers."
  (push functor *matchers*))

(defmacro define-kind-matcher (type (match-var) &body body)
  "Defines a new kind matcher which will receive MATCH-VAR with a match to determine if it is of type TYPE or not.  Must yield nil or t"
  (let ((function-name (intern (format nil "MATCH-~A--P" (symbol-name type)))))
    `(progn
       (defun ,function-name (,match-var)
         ,(format nil "Matches to see if ~A matches type ~A" (symbol-name match-var) (symbol-name type))
         (when (progn ,@body)
           ',type))
       (add-kind-matcher #',function-name))))

(defun matcher-kind (match)
  "What kind is this match?  Yields the symbol or T if none were found."
  (or
   (loop for matcher in *matchers*
         for kind = (funcall matcher match)
         when kind
           return kind)
   t))

;; matching across different types is rare so we don't have to create fast paths for those

(defparameter *comparators* nil
  "Lists all known comparators as (TYPE-LEFT TYPE-RIGHT MATCHER)")



(defun compare (left right)
  "Runs the comparison for left and right.  Comparators are assumed to be symmetric."
  (let ((left-kind (matcher-kind left))
        (right-kind (matcher-kind right)))
    (support:multi-value-or
     ;; `SUPPORT:MULTI-VALUE-OR'' jumps to the next if the answer is a single-valued nil and otherwise returns the result
     (loop for (matcher-left-kind matcher-right-kind matcher)
             in *comparators*
           if (and (eq matcher-left-kind left-kind)
                   (eq matcher-right-kind right-kind))
             return (funcall matcher left right)
           if (and (eq matcher-right-kind left-kind)
                   (eq matcher-left-kind right-kind))
             return (funcall matcher right left))
     (when (kinds-certainly-different-p left-kind right-kind)
       (values nil t))
     (generic-kind-comparison left right))))

(defun find-shared-value-space (left-kind right-kind classes)
  "Yields T when LEFT-KIND and RIGHT-KIND appear together in one class of
CLASSES."
  (some (lambda (c) (and (member left-kind c) (member right-kind c)))
        classes))

(defparameter *matcher-kind-groups*
  ;; Virtuoso
  '((boolean number)
    (untyped-string)
    (xsd-string)
    (lang-string)
    (date-time)
    (iri))
  ;; XSD
  ;; '((untyped-string xsd-string)
  ;;   (number)
  ;;   (boolean)
  ;;   (lang-string)
  ;;   (date-time)
  ;;   (iri))
  "Groups which are considered to potentially overlap.  If kinds are in different groups they can't have overlapping
values.")

(defun kinds-certainly-different-p (left-kind right-kind)
  "Yields non-nil iff two kinds must represent a different value.

Uses *MATCHER-KIND-GROUPS* to indicate which groups could have overlapping values.  T is the top-level \"unknown\" class
so it's ignored."
  (when (and
         ;; this common-case can be short-circuited
         (not (eq left-kind right-kind))
         ;; t means we don't know the kind
         (not (eq left-kind t))
         (not (eq right-kind t)))
    ;; if there's no group which contains both of them, then they must be different
    (notany (lambda (group)
              (and (member left-kind group)
                   (member right-kind group)))
            *matcher-kind-groups*)))

(defun add-comparator (kind-left kind-right functor)
  "Adds the comparator for KIND -LEFT against KIND-RIGHT executed by FUNCTOR to the known comparators."
  (push (list kind-left kind-right functor)
        *comparators*))

(defmacro define-comparator (((var-left type-left) (var-right type-right)) &body body)
  "Defines a new comparator.  Comparators are assumed to be symmetric."
  `(add-comparator ',type-left ',type-right
                   (lambda (,var-left ,var-right) ,@body)))

(defun generic-kind-comparison (a b)
  "Default comparator which is only certain when the two matches have the exact same contents, otherwise it's uncertain fails.

Yields (VALUES SAME-P CERTAIN-P)."
  (labels
      ((identical-p (x y)
         (cond
           ((and (match-p x) (match-p y))
            (and (equal (match-term x) (match-term y))
                 (= (length (match-submatches x))
                    (length (match-submatches y)))
                 (every #'identical-p
                        (match-submatches x) (match-submatches y))))
           ((and (scanned-token-p x) (scanned-token-p y))
            (and (equal (scanned-token-token x) (scanned-token-token y))
                 (string= (scanned-token-effective-string x)
                          (scanned-token-effective-string y))))
           (t nil))))
    (if (identical-p a b)
        (values t t)
        (values nil nil))))
