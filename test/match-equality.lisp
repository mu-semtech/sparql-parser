(defpackage :sparql-parser-test-match-equality
  (:use :common-lisp :fiveam :match-equality)
  (:import-from #:sparql-manipulation
                #:make-rdfliteral
                #:make-string-literal
                #:make-langtag
                #:make-iri)
  (:import-from #:sparql-inspection
                #:*boolean-accept-numeric-string-p*)
  (:import-from #:sparql-parser-test-utils
                #:with-acl-config)
  (:export #:match-equality-tests
           #:match-equality-store-tests))

(in-package :sparql-parser-test-match-equality)

(def-suite match-equality-tests :in sparql-parser-test-integration:integration-tests)
(in-suite match-equality-tests)

(defun make-xsd-string (string)
  (make-rdfliteral string
   :datatype-match (make-iri "http://www.w3.org/2001/XMLSchema#string")))

(defun make-integer (string)
  (make-rdfliteral string
   :datatype-match (make-iri "http://www.w3.org/2001/XMLSchema#integer")))

(defun make-decimal (string)
  (make-rdfliteral string
   :datatype-match (make-iri "http://www.w3.org/2001/XMLSchema#decimal")))

(defun make-double (string)
  (make-rdfliteral string
   :datatype-match (make-iri "http://www.w3.org/2001/XMLSchema#double")))

(defun make-boolean (string)
  (make-rdfliteral string
   :datatype-match (make-iri "http://www.w3.org/2001/XMLSchema#boolean")))

(defun make-datetime (string)
  (make-rdfliteral string
   :datatype-match (make-iri "http://www.w3.org/2001/XMLSchema#dateTime")))

(defun make-date (string)
  (make-rdfliteral string
   :datatype-match (make-iri "http://www.w3.org/2001/XMLSchema#date")))

(defun make-xsd-int (string)
  (make-rdfliteral string
   :datatype-match (make-iri "http://www.w3.org/2001/XMLSchema#int")))

(defun make-custom-typed (string)
  (make-rdfliteral string
   :datatype-match (make-iri "http://example.com/CustomType")))

(defun make-langstring (string langtag)
  (make-rdfliteral string :langtag-match (make-langtag langtag)))

(test integer-equal-certain
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-integer "10") (make-integer "10"))))))

(test decimal-equal-certain
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-decimal "10.0") (make-decimal "10.0"))))))

(test integer-vs-decimal-shared-value
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-integer "10") (make-decimal "10.0"))))))

(test integer-vs-double-shared-value
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-integer "10") (make-double "1.0E1"))))))

(test decimal-leading-dot
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-decimal ".5") (make-decimal "0.5"))))))

(test decimal-trailing-dot
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-decimal "5.") (make-decimal "5.0"))))))

(test decimal-negative-leading-dot
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-decimal "-.5") (make-decimal "-0.5"))))))

(test decimal-zero-signs
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-decimal "0") (make-decimal "-0"))))))

(test double-zero-signs-certain-equal
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-double "0.0E0") (make-double "-0.0E0"))))))

(test double-positive-and-negative-exponent
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-double "1.5e3") (make-double "1.5E3"))))))

(test double-large-certain-equal
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-double "1.0e308") (make-double "1.0e308"))))))

(test plain-string-equal
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-string-literal "hi")
                             (make-string-literal "hi"))))))

(test xsd-string-equal
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-xsd-string "hi") (make-xsd-string "hi"))))))

(test plain-string-empty-equal
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-string-literal "")
                             (make-string-literal ""))))))

(test plain-string-with-quotes-equal
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-string-literal "a\"b")
                             (make-string-literal "a\"b"))))))

(test plain-string-with-backslash-equal
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-string-literal "a\\b")
                             (make-string-literal "a\\b"))))))

(test langstring-equal
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-langstring "hi" "@en")
                             (make-langstring "hi" "@en"))))))

(test langstring-case-insensitive-langtag
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-langstring "hi" "@en")
                             (make-langstring "hi" "@EN"))))))

(test boolean-true-vs-true
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-boolean "true") (make-boolean "true"))))))

(test boolean-lexical-1-vs-true-equal
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-boolean "1") (make-boolean "true"))))))

(test boolean-lexical-0-vs-false-equal
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-boolean "0") (make-boolean "false"))))))

(test boolean-numeric-string-not-accepted-defers
  (let ((*boolean-accept-numeric-string-p* nil))
    (is (equal '(nil nil)
               (multiple-value-list
                (match-equal-p (make-boolean "1") (make-boolean "true")))))
    (is (equal '(t t)
               (multiple-value-list
                (match-equal-p (make-boolean "1") (make-boolean "1")))))
    (is (equal '(t t)
               (multiple-value-list
                (match-equal-p (make-boolean "true") (make-boolean "true")))))))

(test datetime-byte-identical-via-structural
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-datetime "2024-01-02T03:04:05")
                             (make-datetime "2024-01-02T03:04:05"))))))

(test xsd-date-as-datetime-category-byte-identical
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-date "2024-01-02") (make-date "2024-01-02"))))))

(test iri-equal
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-iri "http://example.com/")
                             (make-iri "http://example.com/"))))))

(test iri-equal-complex-uri
  (let ((uri "http://example.com/foo?bar=baz#quux"))
    (is (equal '(t t)
               (multiple-value-list
                (match-equal-p (make-iri uri) (make-iri uri)))))))

(test unknown-typed-literal-byte-identical
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-xsd-int "1") (make-xsd-int "1"))))))

(test custom-typed-literal-byte-identical
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-custom-typed "x") (make-custom-typed "x"))))))

(test integer-not-equal
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-integer "10") (make-integer "11"))))))

(test integer-vs-decimal-different-value
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-integer "10") (make-decimal "11.0"))))))

(test integer-vs-double-different-value
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-integer "10") (make-double "1.5e1"))))))

(test plain-string-not-equal
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-string-literal "hi")
                             (make-string-literal "bye"))))))

(test plain-vs-xsd-string-default-different
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-string-literal "hi")
                             (make-xsd-string "hi"))))))

(test xsd-vs-plain-default-different
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-xsd-string "hi")
                             (make-string-literal "hi"))))))

(test langstring-different-langtag
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-langstring "hi" "@en")
                             (make-langstring "hi" "@fr"))))))

(test langstring-different-lexical
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-langstring "hi" "@en")
                             (make-langstring "bye" "@en"))))))

(test boolean-true-vs-false
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-boolean "true") (make-boolean "false"))))))

(test boolean-1-vs-0
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-boolean "1") (make-boolean "0"))))))

(test datetime-different-lexical-default-deferred
  (is (equal '(nil nil)
             (multiple-value-list
              (match-equal-p (make-datetime "2024-01-02T03:04:05")
                             (make-datetime "2024-01-03T03:04:05"))))))

(test iri-not-equal
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-iri "http://example.com/")
                             (make-iri "http://example.org/"))))))

(test inter-category-string-vs-integer-virtuoso-certain
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-integer "10")
                             (make-string-literal "10"))))))

(test inter-category-integer-vs-boolean-virtuoso-defers
  (is (equal '(nil nil)
             (multiple-value-list
              (match-equal-p (make-integer "10") (make-boolean "true"))))))

(test inter-category-numeric-vs-langstring-virtuoso-certain
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-integer "10")
                             (make-langstring "10" "@en"))))))

(test inter-category-integer-vs-boolean-conforming-certain
  (let ((*matcher-kind-groups*
         '((untyped-string xsd-string)
           (number)
           (boolean)
           (lang-string)
           (date-time)
           (iri))))
    (is (equal '(nil t)
               (multiple-value-list
                (match-equal-p (make-integer "10") (make-boolean "true")))))))

(test inter-category-string-vs-integer-conforming-certain
  (let ((*matcher-kind-groups*
         '((untyped-string xsd-string)
           (number)
           (boolean)
           (lang-string)
           (date-time)
           (iri))))
    (is (equal '(nil t)
               (multiple-value-list
                (match-equal-p (make-integer "10")
                               (make-string-literal "10")))))))

(test inter-category-iri-vs-literal-certain
  (is (equal '(nil t)
             (multiple-value-list
              (match-equal-p (make-iri "http://example.com/")
                             (make-string-literal "http://example.com/"))))))

(test allow-number-comparison-p-off-keeps-byte-identical
  (let ((*allow-number-comparison-p* nil))
    (is (equal '(t t)
               (multiple-value-list
                (match-equal-p (make-integer "10") (make-integer "10")))))
    (is (equal '(nil nil)
               (multiple-value-list
                (match-equal-p (make-integer "10") (make-integer "11")))))
    (is (equal '(nil nil)
               (multiple-value-list
                (match-equal-p (make-integer "10") (make-decimal "10.0")))))))

(test malformed-integer-does-not-crash
  (finishes
   (multiple-value-list
    (match-equal-p (make-integer "one-thousand") (make-integer "one-thousand")))))

(test malformed-decimal-does-not-crash
  (finishes
   (multiple-value-list
    (match-equal-p (make-decimal "l0luRH4x3D") (make-decimal "l0luRH4x3D")))))

(test malformed-integer-vs-malformed-integer-defers
  (is (equal '(nil nil)
             (multiple-value-list
              (match-equal-p (make-integer "one-thousand")
                             (make-integer "l0luRH4x3D"))))))

(test malformed-integer-byte-identical-certain
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-integer "one-thousand")
                             (make-integer "one-thousand"))))))

(test double-nan-byte-identical-certain-via-structural
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-double "nan") (make-double "nan"))))))

(test double-inf-byte-identical-certain-via-structural
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-double "inf") (make-double "inf"))))))

(test double-nan-vs-double-zero-defers
  (is (equal '(nil nil)
             (multiple-value-list
              (match-equal-p (make-double "nan") (make-double "0"))))))

(test double-vs-double-byte-identical-certain
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-double "1.0e1") (make-double "1.0E1"))))))

(test integer-large-byte-identical-certain
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-integer "12345678901234567890")
                             (make-integer "12345678901234567890"))))))

(test integer-large-vs-double-promotion-defers
  (is (equal '(nil nil)
             (multiple-value-list
              (match-equal-p (make-integer "12345678901234567890")
                             (make-double "1.234567890123456789e19"))))))

(test plain-vs-xsd-same-mode-makes-them-equal
  (let ((*plain-vs-xsd-string-treatment* :same))
    (is (equal '(t t)
               (multiple-value-list
                (match-equal-p (make-string-literal "hi")
                               (make-xsd-string "hi")))))))

(test plain-vs-xsd-uncertain-mode-defers
  (let ((*plain-vs-xsd-string-treatment* :uncertain))
    (is (equal '(nil nil)
               (multiple-value-list
                (match-equal-p (make-string-literal "hi")
                               (make-xsd-string "hi")))))))

(test descriptive-nan-default-defers-to-structural
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-double "nan") (make-double "nan"))))))

(test descriptive-nan-vs-zero-default-defers
  (is (equal '(nil nil)
             (multiple-value-list
              (match-equal-p (make-double "nan") (make-double "0"))))))

(test descriptive-inf-default-defers-to-structural
  (is (equal '(t t)
             (multiple-value-list
              (match-equal-p (make-double "inf") (make-double "inf"))))))

(def-suite match-equality-chaos :in match-equality-tests)
(in-suite match-equality-chaos)

(defun chaos-string (random-state length)
  "Builds a deliberately nasty string of LENGTH characters drawn from a
  wide alphabet: every byte 0-255, plus a few SPARQL/numeric-significant
  characters weighted in.  The result is intended to break naive parsers."
  (let ((alphabet (nconc (loop for c from 0 to 255 collect c)
                         (mapcar #'char-code
                                 '(#\\ #\" #\@ #\^ #\# #\. #\+ #\-
                                   #\e #\E #\d #\f #\0 #\9 #\Newline
                                   #\Tab #\Space #\; #\( #\) #\{ #\})))))
    (coerce (loop repeat length
                  collect (code-char (nth (random (length alphabet) random-state)
                                          alphabet)))
            'string)))

(defun all-chaos-fixtures-for (lexical)
  "Yields a list of (label . match) pairs exercising each datatype path
with the same LEXICAL content.  The langtag of the langstring fixture is
LITERAL itself, to feed broken langtags through rdf-literal-lang."
  (let ((fixtures nil))
    (flet ((push! (label make-fn)
             (push (cons label (funcall make-fn lexical)) fixtures)))
      (push! "plain" #'make-string-literal)
      (push! "xsd:string" #'make-xsd-string)
      (push! "xsd:integer" #'make-integer)
      (push! "xsd:decimal" #'make-decimal)
      (push! "xsd:double" #'make-double)
      (push! "xsd:boolean" #'make-boolean)
      (push! "xsd:dateTime" #'make-datetime)
      (push! "xsd:date" #'make-date)
      (push! "xsd:int" #'make-xsd-int)
      (push! "custom" #'make-custom-typed)
      (push! "langstring-self-langtag"
             (lambda (lex) (make-langstring lex lex))))
    (nreverse fixtures)))

(defun call-must-finish (a b)
  (match-equal-p a b))

(test chaos-random-plain-strings-never-crash
  (let ((*random-state* (make-random-state t)))
    (dotimes (i 200 t)
      (let ((a (make-string-literal (chaos-string *random-state*
                                                  (random 64 *random-state*))))
            (b (make-string-literal (chaos-string *random-state*
                                                  (random 64 *random-state*)))))
        (finishes (call-must-finish a b))))))

(test chaos-same-lexical-across-all-datatypes-never-crash
  (let ((*random-state* (make-random-state t)))
    (dotimes (i 100 t)
      (let ((lex (chaos-string *random-state* (random 48 *random-state*))))
        (dolist (fixture (all-chaos-fixtures-for lex))
          (finishes (call-must-finish (cdr fixture) (cdr fixture))))))))

(test chaos-cross-datatype-pairs-never-crash
  (let ((*random-state* (make-random-state t)))
    (dotimes (i 50 t)
      (let* ((lex-a (chaos-string *random-state* (random 24 *random-state*)))
             (lex-b (chaos-string *random-state* (random 24 *random-state*)))
             (fixtures-a (all-chaos-fixtures-for lex-a))
             (fixtures-b (all-chaos-fixtures-for lex-b)))
        (dolist (a fixtures-a)
          (dolist (b fixtures-b)
            (finishes (call-must-finish (cdr a) (cdr b)))))))))

(defparameter *nasty-deterministic-lexicals*
  (list ""
        " "
        (make-string 4 :initial-element #\Space)
        "1" "0" "-0" "+0" "00" "000"
        "1.0" "1.0E1" "1.0e1" "1e" "e1" "1.0E" "1.0E+" "1.0E-"
        "1.0.0" "1.0E1.5" "1.0E1E2" ".E1" "-.E1" "+.E1" "." ".." "..."
        "true" "false" "2" "truefalse" "true true" "fals"
        "one-thousand" "l0luRH4x3D" "1e999999" "-1e999999"
        "@en" "en@" "@" " @" "@@" "@EN-US" "@ZH-Hant"
        "#.(format t \"pwned\")"
        "#.(uiop:quit)"
        "#1#" "#.(progn)" "#.(sb-ext:quit)"
        "a\\b" "a\\u0041b" "a\\Ub" "\\x41" "\\n\\t\\r"
        (make-string 64 :initial-element #\*)
        (make-string 1024 :initial-element #\.)
        (coerce (loop for i below 256 collect (code-char i)) 'string)
        (coerce (loop for i from 255 downto 0 collect (code-char i)) 'string)
        (coerce (loop for i below 1024 collect
                      (code-char (random 256 (make-random-state t))))
                'string)
        "1234567890123456789012345678901234567890"
        "1.234567890123456789012345678901234567890e308"
        "999999999999999999999999999999"
        "-99999999999999999999999999999"
        "<>&" "&&" "||" "!="
        "true" "false" "1" "0"
        "NaN" "nan" "NAN" "+NaN"
        "INF" "inf" "+INF" "-INF" "Infinity"
        (concatenate 'string "a" (string #\Newline) "b")
        (concatenate 'string "a" (string #\Tab) "b")
        (concatenate 'string "a" (string #\Return) "b")
        (concatenate 'string "a" (string (code-char 0)) "b")
        (concatenate 'string "a" (string (code-char 255)) "b"))
  "A list of specifically nasty lexical forms hand-picked to exercise
edges of the parsers: malformed numerics, malformed booleans, malformed
langtags, Lisp reader-macro injection attempts, very long strings, all
256 byte values, embedded control characters and NULL bytes.")

(test chaos-deterministic-nasty-lexicals-never-crash-self
  (dolist (lex *nasty-deterministic-lexicals*)
    (dolist (fixture (all-chaos-fixtures-for lex))
      (finishes (call-must-finish (cdr fixture) (cdr fixture))))))

(test chaos-deterministic-nasty-lexicals-never-crash-cross
  (let ((all (mapcan #'all-chaos-fixtures-for *nasty-deterministic-lexicals*))
        (*random-state* (make-random-state t)))
    (dotimes (i 2000 t)
      (let ((a (elt all (random (length all) *random-state*)))
            (b (elt all (random (length all) *random-state*))))
        (finishes (call-must-finish (cdr a) (cdr b)))))))

(test chaos-malformed-rdfliteral-builds-never-crash
  (let ((*random-state* (make-random-state t)))
    (dotimes (i 200 t)
      (let ((lex (chaos-string *random-state* (random 32 *random-state*)))
            (dt (chaos-string *random-state* (random 64 *random-state*))))
        (let ((fixture (make-rdfliteral lex
                         :datatype-match (make-iri dt))))
          (finishes (call-must-finish fixture fixture)))
        (let ((fixture (make-rdfliteral lex
                         :langtag-match (make-langtag dt))))
          (finishes (call-must-finish fixture fixture)))))))


;;; ---------------------------------------------------------------------------
;;; Store-validation suite: cross-checks match-equal-p against the live
;;; triplestore using the verify-query.rq and verify-cross-kind.rq tables
;;; from triplestore-assumptions.md.  Requires a running Virtuoso at
;;; $SPARQL_TEST_BACKEND (or http://localhost:8891/sparql by default).
;;; ---------------------------------------------------------------------------

(def-suite match-equality-store-tests :in match-equality-tests)
(in-suite match-equality-store-tests)

(defparameter *verify-query-pairs*
  '((0  (:plain "hi")                     (:xsd-string "hi")                    nil)
    (1  (:boolean "true")                 (:boolean "1")                        t)
    (2  (:boolean "true")                 (:boolean "false")                    nil)
    (3  (:integer "10")                  (:decimal "10.0")                     t)
    (4  (:integer "10")                  (:double "1.0E1")                     t)
    (5  (:double "0.0E0")                (:double "-0.0E0")                    t)
    (6  (:lang "hi" "en")                (:lang "hi" "fr")                      nil)
    (7  (:lang "hi" "en")                (:lang "hi" "EN")                      t)
    (8  (:integer "10")                  (:plain "10")                         nil)
    (9  (:datetime "2024-01-02T03:04:05") (:datetime "2024-01-02T03:04:05")    t)
    (10 (:datetime "2024-01-02T03:04:05") (:datetime "2024-01-03T03:04:05")    nil)
    (11 (:int "1")                       (:integer "1")                        t)
    (12 (:plain "hi")                    (:plain "hi")                         t)
    (13 (:integer "10")                  (:integer "11")                       nil))
  "Pairs from verify-query.rq in triplestore-assumptions.md.
Each row is (idx left-spec right-spec expected-store-eq) where
expected-store-eq is T or NIL.")

(defparameter *verify-cross-kind-pairs*
  '((0  (:plain "1")        (:xsd-string "1")    nil)
    (1  (:plain "1")        (:lang "1" "en")      nil)
    (2  (:plain "1")        (:integer "1")        nil)
    (3  (:plain "1")        (:boolean "1")        nil)
    (4  (:xsd-string "1")   (:lang "1" "en")      nil)
    (5  (:xsd-string "1")   (:integer "1")        nil)
    (6  (:xsd-string "1")   (:boolean "1")        nil)
    (7  (:lang "1" "en")    (:integer "1")        nil)
    (8  (:lang "1" "en")    (:boolean "1")        nil)
    (9  (:integer "1")      (:boolean "1")        t)
    (10 (:integer "1")      (:boolean "true")     t)
    (11 (:plain "hi")       (:lang "hi" "en")     nil)
    (12 (:integer "2024")   (:date "2024-01-01")  nil))
  "Pairs from verify-cross-kind.rq in triplestore-assumptions.md.
Rows 9 and 10 expect T because this Virtuoso promotes boolean/integer.")

(defun escape-sparql-string (string)
  "Escapes a Lisp string for embedding in a SPARQL string literal."
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for char across string
          do (case char
               (#\" (write-string "\\\"" stream))
               (#\\ (write-string "\\\\" stream))
               (#\Newline (write-string "\\n" stream))
               (#\Tab (write-string "\\t" stream))
               (#\Return (write-string "\\r" stream))
               (otherwise (write-char char stream))))
    (write-char #\" stream)))

(defun spec->sparql (spec)
  "Renders a spec as a SPARQL term for the VALUES clause."
  (destructuring-bind (tag &rest args) spec
    (case tag
      (:plain (escape-sparql-string (first args)))
      (:xsd-string
       (format nil "~A^^xsd:string" (escape-sparql-string (first args))))
      (:lang
       (format nil "~A@~A" (escape-sparql-string (first args)) (second args)))
      (:boolean
       (format nil "~A^^xsd:boolean" (escape-sparql-string (first args))))
      (:integer
       (format nil "~A^^xsd:integer" (escape-sparql-string (first args))))
      (:decimal
       (format nil "~A^^xsd:decimal" (escape-sparql-string (first args))))
      (:double
       (format nil "~A^^xsd:double" (escape-sparql-string (first args))))
      (:datetime
       (format nil "~A^^xsd:dateTime" (escape-sparql-string (first args))))
      (:date
       (format nil "~A^^xsd:date" (escape-sparql-string (first args))))
      (:int
       (format nil "~A^^xsd:int" (escape-sparql-string (first args))))
      (:iri (format nil "<~A>" (first args)))
      (otherwise (error "Unknown spec tag ~A" tag)))))

(defun spec->match (spec)
  "Builds a match object for the spec using the test helpers."
  (destructuring-bind (tag &rest args) spec
    (case tag
      (:plain (make-string-literal (first args)))
      (:xsd-string (make-xsd-string (first args)))
      (:lang (make-langstring (first args)
                              (format nil "@~A" (second args))))
      (:boolean (make-boolean (first args)))
      (:integer (make-integer (first args)))
      (:decimal (make-decimal (first args)))
      (:double (make-double (first args)))
      (:datetime (make-datetime (first args)))
      (:date (make-date (first args)))
      (:int (make-xsd-int (first args)))
      (:iri (make-iri (first args)))
      (otherwise (error "Unknown spec tag ~A" tag)))))

(defun build-verify-select (pairs)
  "Builds a SELECT query that evaluates ?left = ?right for each row in PAIRS."
  (with-output-to-string (stream)
    (format stream "PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>~%")
    (format stream "SELECT ?idx (?left = ?right AS ?store_eq) WHERE {~%")
    (format stream "  VALUES (?idx ?left ?right) {~%")
    (loop for (idx left-spec right-spec) in pairs
          do (format stream "    (~A ~A ~A)~%"
                     idx
                     (spec->sparql left-spec)
                     (spec->sparql right-spec)))
    (format stream "  }~%")
    (format stream "}~%")
    (format stream "ORDER BY ?idx")))

(defun parse-store-boolean (value)
  "Parses a store boolean value (\"1\"/\"0\" or \"true\"/\"false\") to T or NIL."
  (not (null (member value '("1" "true") :test #'string=))))

(defun query-store-equality (pairs)
  "Issues a single SELECT to the store and returns an alist (idx . store-eq-p).
Requires client::*backend* to be bound to the test endpoint (use with-acl-config)."
  (let* ((query (build-verify-select pairs))
         (response (client:query (coerce query
                                         #-be-cautious 'base-string
                                         #+be-cautious 'string)))
         (bindings (jsown:filter (jsown:parse response)
                                 "results" "bindings")))
    (loop for binding in bindings
          for idx = (parse-integer
                     (jsown:val (jsown:val binding "idx") "value"))
          for store-eq = (parse-store-boolean
                          (jsown:val (jsown:val binding "store_eq") "value"))
          collect (cons idx store-eq))))

(defun validate-pairs-against-store (pairs)
  "For each row in PAIRS, asserts two things:
- store-drift: the store's answer matches the documented expected value,
- soundness: when match-equal-p is certain, its answer matches the store."
  (let ((store-results (query-store-equality pairs)))
    (loop for (idx left-spec right-spec expected) in pairs
          for store-eq = (cdr (assoc idx store-results))
          for (our-eq our-certain) = (multiple-value-list
                                      (match-equal-p (spec->match left-spec)
                                                     (spec->match right-spec)))
          do
             (is (eq store-eq expected)
                 "idx ~A: store says ~A but expected ~A"
                 idx store-eq expected)
             (when our-certain
               (is (eq (not (null our-eq)) store-eq)
                   "idx ~A: we are certain ~A but store says ~A"
                   idx our-eq store-eq)))))

(test store-validates-verify-query-defaults
  "Cross-checks match-equal-p against the store for the verify-query.rq table."
  (with-acl-config
    (validate-pairs-against-store *verify-query-pairs*)))

(test store-validates-verify-cross-kind-defaults
  "Cross-checks match-equal-p against the store for the verify-cross-kind.rq table."
  (with-acl-config
    (validate-pairs-against-store *verify-cross-kind-pairs*)))