(in-package :ebnf)

;;;; SPARQL EBNF symbols
;;;;
;;;; This package contains symbols yielded when reading the EBNF
;;;; s-expression syntax.  Symbols are interned in this package.

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
                   tree)))
      (mapcar #'map-tree
              (with-open-file (input path :direction :input)
                (read input))))))

(defun rule-name (rule)
  "Name of the EBNF rule."
  (second rule))

(defun rule-type (rule)
  "Type of the ebnf rule."
  (first rule))

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
                                     (coerce thing 'base-string)
                                     thing))
                               (rest props))
                       t)))
  (values nil nil))

(defun rule-first (rule)
  "Get first set of RULE."
  (rule-values-for rule 'first))

(defun rule-follow (rule)
  "Get first set of RULE."
  (rule-values-for rule 'follow))

(defun rule-expansion (rule)
  "Returns the rule expansion for RULE."
  (find-if (lambda (name) (find name '(seq alt opt plus star)))
           rule
           :key (lambda (x) (and (consp x) (car x)))))

(defun rule-index (rule)
  "Yields a number indicating the index for the EBNF rule."
  (parse-integer (third rule)))
