(in-package :sparql-inspection)

(defun ebnf-string-real-string (ebnf-string)
  "Yields the real string for the match term string of type ebnf::|String|."
  ;; When strings have been parsed, we should execute their escape
  ;; sequences and then yield the resulting string.  As of <2024-03-11
  ;; Mon> there currently isn't any parsing of \u and \U but we will
  ;; ignore this for now.
  (flet ((first-submatch (match)
           (first (match-submatches match))))
    (let* ((child-match (first-submatch ebnf-string)) ;; must be a specific string
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
            (t (error "Received a string which is not one of 'ebnf::|STRING_LITERAL1| 'ebnf::|STRING_LITERAL2| 'ebnf::|STRING_LITERAL_LONG1| 'ebnf::|STRING_LITERAL_LONG2|"))))))

(defun match-equal-p (a b)
  "Yields truthy iff match a and match b are equal."
  ;; TODO: understand semantically equivalent nodes such as """hello"""
  ;; and "hello".
  (and (eq (type-of a) (type-of b))
       (typecase a
         (match (and (equal (match-term a) (match-term b))
                     (= (length (match-submatches a)) (length (match-submatches b)))
                     (case (match-term a)
                       (ebnf::|String| (string= (ebnf-string-real-string a) (ebnf-string-real-string b)))
                       (otherwise
                        (every #'match-equal-p (match-submatches a) (match-submatches b))))))
         (scanned-token (and (equal (scanned-token-token a) (scanned-token-token b))
                             (string= (scanned-token-effective-string a)
                                      (scanned-token-effective-string b)))))))

