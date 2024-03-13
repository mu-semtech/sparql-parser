(in-package :sparql-inspection)

(defun ebnf-string-real-string (ebnf-string)
  "Yields the real string for the match term string of type ebnf::|String|."
  ;; When strings have been parsed, we should execute their escape
  ;; sequences and then yield the resulting string.  As of <2024-03-11
  ;; Mon> there currently isn't any parsing of \u and \U but we will
  ;; ignore this for now.
  (flet ((first-submatch (match)
           (first (match-submatches match))))
    (let ((base-string
            (scanned-token-effective-string
             (first-submatch
              (first-submatch
               ebnf-string)))))
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
       'string))))

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

