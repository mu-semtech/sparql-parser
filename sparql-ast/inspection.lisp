(in-package :sparql-inspection)

(defun first-found-scanned-token (match)
  "Optimistic search for a scanned token in submatches."
  (support:depth-first-search
   :start match
   :condition #'sparql-parser:scanned-token-p
   :descend #'sparql-parser:match-submatches))

(defun first-submatch (match &optional (levels 1))
  "Yields the first submatch of MATCH.  LEVELS is amount of levels deep the
submatch is searched for."
  (if (= levels 0)
      match
      (first-submatch (first (match-submatches match))
                      (1- levels))))

(defun rdf-literal-datatype (ebnf-rdfliteral-match)
  "Yields the URI type of an ebnf::|RDFLiteral|"
  (when (= 3 (length (match-submatches ebnf-rdfliteral-match)))
    (sparql-manipulation:uri-unwrap-marks
     (sparql-parser:scanned-token-effective-string
      (sparql-inspection:first-found-scanned-token
       (third (match-submatches ebnf-rdfliteral-match)))))))

(defun ebnf-simple-string-p (ebnf-match)
  "Yields truthy iff ebnf-match represents a string, thus being ebnf::|String| or ebnf::|RDFLiteral| with type xsd:string or no LANGTAG and no iri."
  (case (match-term ebnf-match)
    (ebnf::|String| t)
    (ebnf::|RDFLiteral|
     (case (length (match-submatches ebnf-match))
       (1 t) ; is just a string
       (3 (string= (rdf-literal-datatype ebnf-match)
                   "http://www.w3.org/2001/XMLSchema#string"))
       (otherwise nil)))
    (otherwise nil)))

(defun ebnf-string-real-string (ebnf-string)
  "Yields the real string for the match term matching ebnf-simple-string-p
or the string part of any ebnf::|RDFLiteral|."
  ;; When strings have been parsed, we should execute their escape
  ;; sequences and then yield the resulting string.  As of <2024-03-11
  ;; Mon> there currently isn't any parsing of \u and \U but we will
  ;; ignore this for now.
  ;;
  ;; TODO: verify whether support for \u or \U is necessary.
  (let* ((child-match
           (ecase (match-term ebnf-string)
             (ebnf::|RDFLiteral| (first-submatch ebnf-string 2))
             (ebnf::|String| (first-submatch ebnf-string 1))))
         (base-string
           (scanned-token-effective-string
            (first-submatch child-match)))
         (string-representation
           (coerce
            (loop for idx = 0 then (1+ idx)
                  while (< idx (length base-string))
                  for char = (elt base-string idx)
                  collect (case char
                            (#\\
                             (incf idx)
                             (case (elt base-string idx)
                               (#\\ #\\)
                               ((#\t #\T) #\Tab)
                               ((#\n #\N) #\Linefeed)
                               ((#\r #\R) #\Return)
                               ((#\b #\B) #\Backspace)
                               ((#\f #\F) #\Formfeed)
                               (#\" #\")
                               (#\' #\')))
                            (otherwise char)))
            'string)))
    (cond ((sparql-parser:match-term-p child-match 'ebnf::|STRING_LITERAL1| 'ebnf::|STRING_LITERAL2|)
           (subseq string-representation 1 (- (length string-representation) 1)))
          ((sparql-parser:match-term-p child-match 'ebnf::|STRING_LITERAL_LONG1| 'ebnf::|STRING_LITERAL_LONG2|)
           (subseq string-representation 3 (- (length string-representation) 3)))
          (t (error "Received a string which is not one of 'ebnf::|STRING_LITERAL1| 'ebnf::|STRING_LITERAL2| 'ebnf::|STRING_LITERAL_LONG1| 'ebnf::|STRING_LITERAL_LONG2|")))))

(defun ebnf-boolean-p (ebnf-match)
  "Yields truethy iff the ebnf-match represents a boolean."
  (or (eq (match-term ebnf-match) 'ebnf::|BooleanLiteral|)
      (and (eq (match-term ebnf-match) 'ebnf::|RDFLiteral|)
           (string= (rdf-literal-datatype ebnf-match)
                    "http://www.w3.org/2001/XMLSchema#boolean"))))

(defun ebnf-boolean-as-real-boolean (ebnf-match)
  "Takes an EBNF boolean for match of type ebnf::|BooleanLiteral| or an ebnf::|RDFLiteral| with type xsd:boolean."
  (let* ((raw-string
           (ecase (match-term ebnf-match)
             (ebnf::|BooleanLiteral|
              (scanned-token-effective-string (first-found-scanned-token ebnf-match)))
             (ebnf::|RDFLiteral|
              (ebnf-string-real-string (first-submatch ebnf-match)))))
         (string (string-downcase raw-string)))
    (cond ((string= "true" string) (values t t))
          ((string= "false" string) (values nil t))
          ((string= "0" string) (values nil t))
          ((string= "1" string) (values t t))
          (t (values nil nil)))))

(defun ebnf-numeric-literal-p (ebnf-match)
  "Yields truethy iff the ebnf-match represents an integer."
  (or (eq (match-term ebnf-match) 'ebnf::|NumericLiteral|)
      (and (eq (match-term ebnf-match) 'ebnf::|RDFLiteral|)
           (find (rdf-literal-datatype ebnf-match)
                 (list "http://www.w3.org/2001/XMLSchema#integer"
                       "http://www.w3.org/2001/XMLSchema#decimal"
                       "http://www.w3.org/2001/XMLSchema#double")
                 :test #'string=))))

(defun ebnf-numeric-literal-extract-info (ebnf-match)
  "Extracts numeric info from a numeric literal.  Yields (cons TYPE STRING-VALUE).

The TYPE is one of :integer, :decimal and :double corresponding with the
xsd type.  The STRING-VALUE is the literal value string in the EBNF-MATCH."
  (flet ((extract-number-type (match)
           (ecase (match-term match)
             (ebnf::|RDFLiteral|
              (support:case+ ((rdf-literal-datatype match) :test #'string=)
                ("http://www.w3.org/2001/XMLSchema#integer" :integer)
                ("http://www.w3.org/2001/XMLSchema#decimal" :decimal)
                ("http://www.w3.org/2001/XMLSchema#double" :double)
                (otherwise (error "Received incorrect datatype for number ~A" (rdf-literal-datatype match)))))
             (ebnf::|NumericLiteral|
              (ecase (scanned-token-token (first-found-scanned-token match))
                ((ebnf::|INTEGER| ebnf::|INTEGER_POSITIVE| ebnf::|INTEGER_NEGATIVE|) :integer)
                ((ebnf::|DECIMAL| ebnf::|DECIMAL_POSITIVE| ebnf::|DECIMAL_NEGATIVE|) :decimal)
                ((ebnf::|DOUBLE| ebnf::|DOUBLE_POSITIVE| ebnf::|DOUBLE_NEGATIVE|) :double)))))
         (extract-number-string (match)
           (ecase (match-term match)
             (ebnf::|RDFLiteral| (ebnf-string-real-string match))
             (ebnf::|NumericLiteral| (sparql-parser:scanned-token-effective-string
                                      (sparql-inspection:first-found-scanned-token
                                       match))))))
    (cons (extract-number-type ebnf-match)
          (extract-number-string ebnf-match))))

(defun ebnf-numeric-literal-equal (left-ebnf-match right-ebnf-match)
  "Yields truethy iff both matches represent the same number by some
sensible equality, assuming both represent a numeric literal."
  (destructuring-bind (left-number-type . left-number-string)
      (ebnf-numeric-literal-extract-info left-ebnf-match)
    (destructuring-bind (right-number-type . right-number-string)
        (ebnf-numeric-literal-extract-info right-ebnf-match)
      ;; TODO: this is a more stringent version of equality than what can
      ;; be expected from current triplestores we use.  Add an
      ;; implementation which is friendlier towards what the triplestore
      ;; expects.
      (and (eq left-number-type right-number-type)
           (equalp left-number-string right-number-string)))))

(defun match-equal-p (a b)
  "Yields truthy if match a and match b are equal.  May provide false
negatives but not false positives."
  (and (eq (type-of a) (type-of b))
       (typecase a
         (match
             (cond ((and (ebnf-simple-string-p a)
                         (ebnf-simple-string-p b))
                    (string= (ebnf-string-real-string a) (ebnf-string-real-string b)))
                   ((and (ebnf-boolean-p a)
                         (ebnf-boolean-p b))
                    (eq (ebnf-boolean-as-real-boolean a)
                        (ebnf-boolean-as-real-boolean b)))
                   ((and (ebnf-numeric-literal-p a)
                         (ebnf-numeric-literal-p b))
                    (ebnf-numeric-literal-equal a b))
                   (t
                    (and (equal (match-term a) (match-term b))
                         (= (length (match-submatches a)) (length (match-submatches b)))
                         (every #'match-equal-p (match-submatches a) (match-submatches b))))))
         (scanned-token (and (equal (scanned-token-token a) (scanned-token-token b))
                             (string= (scanned-token-effective-string a)
                                      (scanned-token-effective-string b)))))))

