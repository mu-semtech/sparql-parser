(in-package #:match-equality)

;; given the API of define-kind-matcher and define-comparator, we can later make this more efficient by using numbers instead.  We will define the comparison system separately from the comparators.

;; define-kind-matcher will always yield t or nil

(define-kind-matcher untyped-string (match)
  (case (match-term match)
    (ebnf::|String| t)
    (ebnf::|RDFLiteral| (= 1 (length (match-submatches match))))
    (otherwise nil)))

(define-kind-matcher xsd-string (match)
  (and (eq (match-term match) 'ebnf::|RDFLiteral|)
       (= 3 (length (match-submatches match)))
       (string= (rdf-literal-datatype match)
                "http://www.w3.org/2001/XMLSchema#string")))

(define-kind-matcher lang-string (match)
  (and (eq (match-term match) 'ebnf::|RDFLiteral|)
       (= 2 (length (match-submatches match)))))

(define-kind-matcher date-time (match)
  (and (eq (match-term match) 'ebnf::|RDFLiteral|)
       (= 3 (length (match-submatches match)))
       (member (rdf-literal-datatype match)
               '("http://www.w3.org/2001/XMLSchema#dateTime"
                 "http://www.w3.org/2001/XMLSchema#date"
                 "http://www.w3.org/2001/XMLSchema#time"
                 "http://www.w3.org/2001/XMLSchema#dateTimeStamp"
                 "http://www.w3.org/2001/XMLSchema#gYear"
                 "http://www.w3.org/2001/XMLSchema#gYearMonth"
                 "http://www.w3.org/2001/XMLSchema#gMonth"
                 "http://www.w3.org/2001/XMLSchema#gMonthDay"
                 "http://www.w3.org/2001/XMLSchema#gDay")
               :test #'string=)))

(define-kind-matcher number (match)
  (ebnf-numeric-literal-p match))

(define-kind-matcher boolean (match)
  (ebnf-boolean-p match))

(define-kind-matcher iri (match)
  (eq (match-term match) 'ebnf::|iri|))

;; define-comparator's implementation will always yield two values:
;; (truthy-p and certain-p) as match-equal-p does.

;; matching across different types is rare so we don't have to create fast paths for those

(define-comparator ((left untyped-string) (right untyped-string))
  (values (string= (ebnf-string-real-string left)
                   (ebnf-string-real-string right))
          t))

(define-comparator ((left xsd-string) (right xsd-string))
  (values (string= (ebnf-string-real-string left)
                   (ebnf-string-real-string right))
          t))

(defparameter *plain-vs-xsd-string-treatment* :different
  "Are plain strings equal to xsd:string (RDF-1.1 interpretation) or should we defer to the triplestore.

- :different means they're treated as unique values.
- :same means they're in one bag and we can ignore xsd:string
- :uncertain means we should just ask the triplestore

Also check *MATCHER-KIND-GROUPS*"
  ;; TODO: derive this from *matcher-kind-groups*
  )

(define-comparator ((left untyped-string) (right xsd-string))
  (case *plain-vs-xsd-string-treatment*
    (:different (values nil t))
    (:same (values (string= (ebnf-string-real-string left)
                            (ebnf-string-real-string right))
                   t))
    (:uncertain (values nil nil))))

(define-comparator ((left lang-string) (right lang-string))
  (let ((left-lang (rdf-literal-lang left :skip-@ t))
        (right-lang (rdf-literal-lang right :skip-@ t)))
    (cond
      ((or (not left-lang) (not right-lang))
       (generic-kind-comparison left right))
      ((not (string-equal left-lang right-lang))
       (values nil t))
      (t
       (values (string= (ebnf-string-real-string left)
                        (ebnf-string-real-string right))
               t)))))

(define-comparator ((left boolean) (right boolean))
  (multiple-value-bind (left-boolean left-certain-p)
      (ebnf-boolean-as-real-boolean left)
    (multiple-value-bind (right-boolean right-certain-p)
        (ebnf-boolean-as-real-boolean right)
      (if (and left-certain-p right-certain-p)
          (values (eq left-boolean right-boolean) t)
          (generic-kind-comparison left right)))))

(defparameter *numeric-certain-equal-epsilon* 0
  "Absolute epsilon for the certain-equal branch of numeric comparison.
Default 0 means exact rational equality is required for certain-equal.
Values inside this tolerance but not exactly equal are still deferred.")

(defparameter *numeric-certain-different-epsilon* 1d-9
  "Relative delta beyond which two numbers are certainly different.
delta = |a-b| / max(1, min(|a|,|b|)).  1d-9 swallows ULP-level rounding
between SBCL and the triplestore while still detecting user-meaningful
deltas.  Values inside the band defer.")

(defparameter *numeric-exact-vs-float-promotion-bound* (expt 2 53)
  "When comparing an exact integer/decimal against a float/double, an
exact side whose magnitude exceeds this defers (uncertain) -- promoting
it would lose precision and could yield a false certain-nil.")

(defun parse-rational-decimal (string)
  "Parses an xsd:decimal lexical form to an exact ratio.  Handles optional
leading +, leading-dot, and leading zeros.  Errors signal malformed input
and are caught by parse-numeric's handler-case."
  (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return) string))
         (neg (and (plusp (length s)) (char= (char s 0) #\-)))
         (pos (and (plusp (length s)) (char= (char s 0) #\+)))
         (digits (subseq s (if (or neg pos) 1 0)))
         (dot (position #\. digits)))
    (if (null dot)
        (if (string= digits "") 0 (parse-integer digits))
        (let* ((int-part (subseq digits 0 dot))
               (frac-part (subseq digits (1+ dot)))
               (frac-len (length frac-part))
               (num (parse-integer
                     (concatenate 'string
                                  (if (string= int-part "") "0" int-part)
                                  frac-part)))
               (den (expt 10 frac-len)))
          (/ (if neg (- num) num) den)))))

(defun float-infinity-p (d)
  "Portable-ish detection of IEEE infinity on double-floats.  Excludes NaN
-- callers must test NaN before this."
  (and (floatp d)
       (not (<= most-negative-double-float d most-positive-double-float))))

(defun float-nan-p (d)
  "Portable-ish detection of NaN: NaN does not equal itself."
  (and (floatp d) (not (= d d))))

(defun parse-signed-integer-strict (string)
  "Parses STRING as an optional-sign decimal integer with no junk,
returning the integer or NIL when malformed.  Uses parse-integer rather
than READ, so reader macros cannot fire."
  (handler-case
      (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return) string))
             (neg (and (plusp (length s)) (char= (char s 0) #\-)))
             (rest (if (and (plusp (length s))
                            (member (char s 0) '(#\+ #\-)))
                       (subseq s 1)
                       s)))
        (if (string= rest "")
            nil
            (let ((n (parse-integer rest)))
              (if neg (- n) n))))
    (error () nil)))

(defun parse-double-lexical (string)
  "Parses an xsd:double lexical form to a double-float without invoking
the Lisp reader.  Returns (VALUES DOUBLE-FLOAT T) on success or
:UNHANDLED on malformed input or numerical overflow.  The descriptive
forms (NaN/INF) contain no digits and are rejected by the mantissa check."
  (let* ((s (string-trim '(#\Space #\Tab #\Newline #\Return)
                         (string-downcase string)))
         (sign (cond
                 ((and (plusp (length s)) (char= (char s 0) #\-)) -1)
                 ((and (plusp (length s)) (char= (char s 0) #\+)) 1)
                 (t 1)))
         (body (if (and (plusp (length s)) (member (char s 0) '(#\+ #\-)))
                   (subseq s 1)
                   s)))
    (flet ((unhandled () (return-from parse-double-lexical :unhandled)))
      (when (string= body "")
        (unhandled))
      (let ((e-pos (position #\e body))
            (mantissa body)
            (exp 0))
        (when e-pos
          (setf mantissa (subseq body 0 e-pos))
          (let ((parsed (parse-signed-integer-strict
                         (subseq body (1+ e-pos)))))
            (unless parsed (unhandled))
            (setf exp parsed)))
        (unless (some #'digit-char-p mantissa) (unhandled))
        (unless (<= (count #\. mantissa) 1) (unhandled))
        (unless (every (lambda (c) (or (digit-char-p c) (char= c #\.)))
                       mantissa)
          (unhandled))
        (let ((rat (handler-case (parse-rational-decimal mantissa)
                     (error () (unhandled)))))
          (handler-case
              (let ((d (coerce (* sign rat (expt 10 exp)) 'double-float)))
                (cond
                  ((float-infinity-p d) :unhandled)
                  ((float-nan-p d) :unhandled)
                  (t (values d t))))
            (floating-point-overflow () :unhandled)
            (floating-point-invalid-operation () :unhandled)))))))

(defun parse-numeric (match)
  "Yields (VALUES VALUE FLOATP) where VALUE is an exact rational or a
double-float, or :UNHANDLED on parse failure / NaN / +-INF / overflow.
FLOATP is T iff VALUE is a double-float."
  (destructuring-bind (type . string)
      (ebnf-numeric-literal-extract-info match)
    (handler-case
        (ecase type
          (:integer (values (parse-integer string) nil))
          (:decimal (values (parse-rational-decimal string) nil))
          (:double (parse-double-lexical string)))
      (error () :unhandled))))

(defun compare-numeric-values (a b a-floatp b-floatp)
  (cond
    ((and (not a-floatp) (not b-floatp))
     (cond
       ((= a b) (values t t))
       ((and (> *numeric-certain-equal-epsilon* 0)
             (<= (abs (- a b)) *numeric-certain-equal-epsilon*))
        (values t t))
       (t (values nil t))))
    (t
     (handler-case
         (let* ((af (if a-floatp a (coerce a 'double-float)))
                (bf (if b-floatp b (coerce b 'double-float)))
                (big (or (and (not a-floatp)
                              (> (abs af) *numeric-exact-vs-float-promotion-bound*))
                         (and (not b-floatp)
                              (> (abs bf) *numeric-exact-vs-float-promotion-bound*)))))
           (cond
             (big (values nil nil))
             ((and (zerop af) (zerop bf)) (values t t))
             ((= af bf) (values t t))
             ((and (> *numeric-certain-equal-epsilon* 0)
                   (<= (abs (- af bf)) *numeric-certain-equal-epsilon*))
              (values t t))
             (t
              (let* ((min-abs (min (abs af) (abs bf) 1d0))
                     (delta (/ (abs (- af bf)) (max 1d0 min-abs))))
                (if (> delta *numeric-certain-different-epsilon*)
                    (values nil t)
                    (values nil nil))))))
       (floating-point-overflow () (values nil nil))
       (floating-point-invalid-operation () (values nil nil))))))

(defparameter *allow-number-comparison-p* t
  "Number comparison is complex.  Toggle this to have all values which are not exactly the same to go through the triplestore.")

(define-comparator ((left number) (right number))
  (if *allow-number-comparison-p*
      (multiple-value-bind (va a-floatp) (parse-numeric left)
        (multiple-value-bind (vb b-floatp) (parse-numeric right)
          (cond
            ((or (eq va :unhandled) (eq vb :unhandled))
             (generic-kind-comparison left right))
            (t
             (multiple-value-bind (num-equal num-certain)
                 (compare-numeric-values va vb a-floatp b-floatp)
               (values num-equal num-certain))))))
      (generic-kind-comparison left right)))

(define-comparator ((left iri) (right iri))
  (multiple-value-bind (equal-p) (generic-kind-comparison left right)
    (values equal-p t)))

;; Here is the public API.  We must not change the signature of this function.

(defun match-equal-p (a b)
  "Compares match a to match b and returns (VALUES TRUTHY CERTAIN-P).
When CERTAIN-P is NIL the TRUTHY value is a guess; when CERTAIN-P is
TRUTHY we are certain about the answer.  Literal matches delegate to the
kind-matcher/comparator registry; unhandled cases fall through to
byte-structural recursion over submatches.  Never yields a false
certain-positive."
  (if (eq (type-of a) (type-of b))
      (typecase a
        (match (compare a b))
        (scanned-token
         (values (and (equal (scanned-token-token a) (scanned-token-token b))
                      (string= (scanned-token-effective-string a)
                               (scanned-token-effective-string b)))
                 t)))
      (values nil t)))
