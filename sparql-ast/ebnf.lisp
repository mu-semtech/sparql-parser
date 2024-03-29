(in-package :ebnf)

;;;; SPARQL EBNF symbols
;;;;
;;;; This package contains symbols yielded when reading the EBNF
;;;; s-expression syntax.  Symbols are interned in this package.

(defstruct rule
  (name nil :type symbol)
  (type nil :type (member rule terminal))
  (first nil :type list)
  (follow nil :type list)
  (expansion nil :type list)
  (index nil :type (or null fixnum)))

(defun read-bnfsexp-from-file (path)
  "Reads a bnf sxp file file frrom PATH."
  (let ((*package* (find-package :ebnf))
        (*readtable* (let ((rt (copy-readtable)))
                       (set-dispatch-macro-character
                        #\# #\t
                        (lambda (s c n)
                          (declare (ignore s c n))
                          t)
                        rt)
                       (setf (readtable-case rt) :preserve)
                       rt))
        (mapping '(|rule| rule
                   |terminal| terminal
                   |first| first
                   |follow| follow
                   |seq| seq
                   |alt| alt
                   |opt| opt
                   |plus| plus
                   |star| star)))
    (labels ((map-tree (tree)
               (if (and tree (listp tree))
                   ;; we now know tree is a tree and not nil
                   (let ((first (or (getf mapping (first tree))
                                    (first tree))))
                     `(,first ,@(mapcar #'map-tree (rest tree))))
                   tree))
             (as-ebnf-rule (rule)
               (make-rule :type (first rule)
                          :name (second rule)
                          :first (rule-values-for rule 'first)
                          :follow (rule-values-for rule 'follow)
                          :expansion   (find-if (lambda (name) (find name '(seq alt opt plus star)))
                                                rule
                                                :key (lambda (x) (and (consp x) (car x))))
                          :index (handler-case (parse-integer (third rule))
                                   (error nil)))))
      (mapcar (alexandria:compose #'as-ebnf-rule #'map-tree)
              (with-open-file (input path :direction :input)
                (read input))))))

(defun rule-terminal-p (rule)
  "Returns truethy iff the rule is a terminal specification."
  (eq (rule-type rule) 'terminal))

(defun rule-values-for (rule key)
  "Gets values for KEY in RULE."
  (loop for props in (rest rule)
        when (and (listp props)
                  (eq (first props) key))
          do (return-from rule-values-for
               (values (mapcar (lambda (thing)
                                 (if (typep thing 'string)
                                     (coerce thing #-be-cautious 'base-string #+be-cautious 'string)
                                     thing))
                               (rest props))
                       t)))
  (values nil nil))

(defparameter *abstract-tokens* nil
  "List of known abstract token expansions.")

(defmacro define-abstract-token (token &rest elements)
  "Used as an abstraction layer, must be known at compiletime.  These
define names that could be used as an alternative for many other names."
  `(setf (getf *abstract-tokens* ',token) ',elements))

(defun abstract-token-expansion (token)
  "Yields the abstract token set for the supplied token."
  (getf *abstract-tokens* token))

(define-abstract-token ebnf::|ABSTRACT-IRI| ebnf::|IRIREF| ebnf::|PNAME_LN| ebnf::|PNAME_NS|)
(define-abstract-token ebnf::|ABSTRACT-VAR| ebnf::|VAR1| ebnf::|VAR2|)
;; NOTE: does not contain RDFLiteral, which does contain String so must be checked first
(define-abstract-token ebnf::|ABSTRACT-PRIMITIVE|
  ebnf::|STRING_LITERAL1| ebnf::|STRING_LITERAL2| ebnf::|STRING_LITERAL_LONG1| ebnf::|STRING_LITERAL_LONG2| ebnf::|INTEGER_NEGATIVE| ebnf::|DECIMAL_NEGATIVE| ebnf::|DOUBLE_NEGATIVE| ebnf::|INTEGER_POSITIVE| ebnf::|DECIMAL_POSITIVE| ebnf::|DOUBLE_POSITIVE| ebnf::|INTEGER| ebnf::|DECIMAL| ebnf::|DOUBLE|)
