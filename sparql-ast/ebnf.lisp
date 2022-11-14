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
