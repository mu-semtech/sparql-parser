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
  (sparql-manipulation:uri-unwrap-marks
   (sparql-parser:scanned-token-effective-string
    (sparql-inspection:first-found-scanned-token
     (third (match-submatches ebnf-rdfliteral-match))))))

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
  "Yields the real string for the match term matching ebnf-simple-string-p."
  ;; When strings have been parsed, we should execute their escape
  ;; sequences and then yield the resulting string.  As of <2024-03-11
  ;; Mon> there currently isn't any parsing of \u and \U but we will
  ;; ignore this for now.
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

(defun ebnf-real-number (match)
  ;; yields the number 
  )

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

(defun match-equal-p (a b)
  "Yields truthy iff match a and match b are equal."
  ;; TODO: understand semantically equivalent nodes such as """hello"""
  ;; and "hello".
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
                   ;; TODO: support (ebnf::|NumericLiteral|)
                   (t
                    (and (equal (match-term a) (match-term b))
                         (= (length (match-submatches a)) (length (match-submatches b)))
                         (every #'match-equal-p (match-submatches a) (match-submatches b))))))
         (scanned-token (and (equal (scanned-token-token a) (scanned-token-token b))
                             (string= (scanned-token-effective-string a)
                                      (scanned-token-effective-string b)))))))

