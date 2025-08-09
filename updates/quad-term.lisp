(in-package :quad-term)

(defun quad-term-uri (quad-term)
  "Yields the quad-term's URI representation if that exists."
  (cond ((consp quad-term)
         (cdr quad-term))
        ((sparql-parser:match-term-p quad-term 'ebnf::|IRIREF|)
         (let ((str (sparql-parser:terminal-match-string quad-term)))
           (subseq str 1 (1- (length str)))))))

(defun quad-term-uri= (quad-term uri-string)
  "Checks whether uri-string is the same as the quad-term."
  ;; The quad-uri-string is wrapped in < and > but the uri-string is
  ;; not, so we need to unwrap.
  (let* ((quad-uri-string (quad-term-uri quad-term)))
    (if quad-uri-string
        (string= uri-string quad-uri-string))))

(defun quad-term-object-as-match (quad-object)
  ;; NOTE: this was the original implementation

  ;; TODO: Verify a quad-object will always yield a iri or one of the following terms and remove this old commented code
  ;; or support other forms.  'ebnf::|RDFLiteral| 'ebnf::|BooleanLiteral| 'ebnf::|NumericLiteral| 'ebnf::|String|
  ;; 'ebnf::|iri|

  ;; (if (and (sparql-parser:match-p quad-object)
  ;;          (sparql-parser:match-term-p quad-object 'ebnf::|RDFLiteral| 'ebnf::|BooleanLiteral| 'ebnf::|NumericLiteral| 'ebnf::|String|
  ;;                                      ;; 'ebnf::string_literal1 'ebnf::string_literal2 'ebnf::string_literal_long1 'ebnf::string_literal_long2
  ;;                                      ))
  ;;     quad-object
  ;;     (if nil
  ;;         ;; (and (listp quad-object)
  ;;         ;;      (find (car quad-object)
  ;;         ;;            (list 'ebnf::|RDFLiteral| 'ebnf::|BooleanLiteral| 'ebnf::|NumericLiteral| 'ebnf::|String|)))
  ;;         quad-object
  ;;         (sparql-manipulation:make-iri (quad-term-uri quad-object))))
  (if (consp quad-object)
      (sparql-manipulation:make-iri (quad-term-uri quad-object)) ; in case of a prefixed uri
      (case (sparql-parser:match-term quad-object)
        (ebnf::|IRIREF| (sparql-manipulation:make-nested-match `(ebnf::|iri| ,quad-object)))
        (otherwise quad-object))))

;; TODO: deduplicate from quad.lisp
(loop for key in '(quad-term-uri quad-term-uri= quad-term-object-as-match)
      for name = (symbol-name key)
      for short-name = (cl-ppcre:regex-replace (format nil "-?~A-?" (package-name *package*)) name "")
      for short-symbol = (intern short-name *package*)
      do
         (setf (fdefinition short-symbol)
               (fdefinition key))
      when (fboundp `(setf ,key))
        do
           (setf (fdefinition `(setf ,short-symbol))
                 (fdefinition `(setf ,key))))
