(in-package :sparql-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; rules and solution structures
(defstruct rule
  "Expresses an expansion rule."
  (name (error "Must supply rule name") :type symbol)
  (expansion nil :type list))

(defstruct (match (:copier nil))
  (term (error "Must supply term when creating a match") :type (or symbol string))
  (rule nil :type (or null rule))        ; nil when rule is not known yet
  (submatches nil :type list))

(defun copy-match (match &optional deep-p)
  "Copies MATCH.

If DEEP-P is non-nil submatches are copied recursively through
submatches provided they are of type MATCH."
  (if (match-p match)
      (make-match :term (match-term match)
                  :rule (match-rule match)
                  :submatches (if deep-p
                                  (mapcar (alexandria:rcurry #'copy-match deep-p)
                                          (match-submatches match))
                                  (match-submatches match)))
      match))

(defstruct sparql-ast
  (top-node nil :type (or null match))
  (string (error "Must supply base-string|string (be-cautious feature) when creating sparql-match") :type #-be-cautious base-string #+be-cautious string))

(defstruct scanned-token
  "Represents a scanned token."
  (start (error "Must supply token start") :type integer)
  (end (error "Must supply token end") :type integer)
                                        ; end is right after the scanned
                                        ; character.  ie: if START is 0,
                                        ; the result 0 would mean no
                                        ; characters are read to match
                                        ; the token.
  (string nil :type (or symbol string))  ; optional known string representation
  (token (error "Must supply matched token") :type (or symbol string)))

(defun terminal-p-scanner (thing)
  (loop for pos from 0 below (length thing)
        for char = (elt thing pos)
        unless (support:match-tree-search (char)
                 (#\A . #\Z)
                 (#\0 . #\9)
                 (#\_ . #\_))
          do (return-from terminal-p-scanner nil))
  t)

(defun terminalp (thing)
  "Returns truethy iff symbol represents a terminal.

We accept strings and uppercase symbols as terminals."
  (typecase thing
    (string t)
    (symbol (terminal-p-scanner (symbol-name thing)))
    (t nil)))

(defun match-term-p (match &rest terms)
  "Yields truthy iff MATCH has MATCH-TERM in TERMS."
  (find (match-term match) terms :test #'eq))

(defun scanned-token-effective-string (scanned-token)
  "Yields the effectively matched string of SCANNED-TOKEN within the current context.

Assumes *scanning-string* is available."
  (declare (special *scanning-string*))
  (or (scanned-token-string scanned-token)
      (when (and *scanning-string*
                 (<= (scanned-token-end scanned-token) (length *scanning-string*)))
        (subseq *scanning-string*
                (scanned-token-start scanned-token)
                (scanned-token-end scanned-token)))))

(defun terminal-match-string (match)
  "Match string for the supplied terminal match.

Assumes MATCH is a MATCH statement with only a TOKEN-MATCH as a child."
  (assert (and (typep (first (sparql-parser:match-submatches match)) 'sparql-parser:scanned-token)
               (= (length (sparql-parser:match-submatches match)) 1)))
  (scanned-token-effective-string (first (sparql-parser:match-submatches match))))

(defun clone-sparql-ast (sparql-ast)
  "Clones SPARQL-AST, including the matches and submatches, but nothing but
those.  Allows for manipulation without destroying the original."
  (labels ((clone-match (match)
             (typecase match
               (match (make-match :term (match-term match)
                                  :rule (match-rule match)
                                  :submatches (mapcar #'clone-match (match-submatches match))))
               (t match))))
    (make-sparql-ast :string (sparql-ast-string sparql-ast)
                     :top-node (clone-match (sparql-ast-top-node sparql-ast)))))

(defun match-match-submatches (match)
  "Yields all submatches of match which are a match themselves as per MATCH-P"
  (when (sparql-parser:match-p match)
    (remove-if-not #'sparql-parser:match-p (sparql-parser:match-submatches match))))

;;;;;;;;;;;;;;
;;;; Constants

(defconstant +END+ 'ebnf:|_eof| ;; :end-eof
             "Last token to be processed.")

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Stub language rules

(defparameter *start-symbol* 'ebnf::|UpdateUnit|
  "The symbol used to start processing.")
;; (defparameter *start-symbol* 'ebnf::|QueryUnit|
;;   "The symbol used to start processing.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Construction of transition table

(defun ebnf-rule-search (rules key)
  "Searches a list of BNF rules for the given rule name."
  (find key rules :key #'ebnf:rule-name))

(defun construct-transition-table-from-parsed-bnf (parsed-bnf)
  "Import EBNF converted through Ruby's EBNF module to BNF and written as s-expressions."
  (flet ((mk-key (key)
           (cons key (sparql-terminals:scanner-for (if (stringp key) (coerce key #-be-cautious 'base-string #+be-cautious 'string) key))))
         (rule-first-includes-empty-p (rule)
           (some (lambda (k) (eq k 'ebnf:|_eps|))
                 (ebnf:rule-first rule))))
    (let ((empty-rule (make-rule :name 'ebnf:|_empty| :expansion nil)))
      (loop
        for rule in parsed-bnf
        for rule-name = (ebnf:rule-name rule)
        for rule-expansion = (ebnf:rule-expansion rule)
        for rule-expansion-type = (first rule-expansion)
        for rule-expansion-options = (rest rule-expansion)
        for rule-first-all = (ebnf:rule-first rule)
        for rule-first-options = (remove-if (lambda (k) (eq k 'ebnf:|_eps|)) rule-first-all)
        for rule-follow = (ebnf:rule-follow rule)
        unless (ebnf:rule-terminal-p rule)
          append
          (list (ebnf:rule-name rule)
                (cond ((eq rule-expansion-type 'ebnf:seq)
                       ;; sequence
                       (let* ((expansion-rule (make-rule :name rule-name :expansion rule-expansion-options))
                              (predicted (loop for first in rule-first-options
                                               append (list (mk-key first) expansion-rule)))
                              (empty-follow (when (rule-first-includes-empty-p rule)
                                              ;; follow will contain _empty deeper down the stack
                                              (loop for follow in rule-follow
                                                    append (list (mk-key follow) expansion-rule)))))
                         (concatenate 'list predicted empty-follow)))
                      ((eq rule-expansion-type 'ebnf:alt)
                       ;; alternatives
                       ;;
                       ;; although this will result in duplicate rules,
                       ;; we are generating rules on the fly here.
                       (loop for option in rule-expansion-options
                             append
                             (cond ((eq option 'ebnf:|_empty|)
                                    (loop for key in (cons 'ebnf:|_eof| rule-follow)
                                          append (list (mk-key key) empty-rule)))
                                   ((terminalp option)
                                    (list (mk-key option) (make-rule :name rule-name
                                                                     :expansion (list option))))
                                   (t ;; a subselection
                                    (let* ((ebnf-child-rule (ebnf-rule-search parsed-bnf option))
                                           (child-expansion-rule (make-rule :name rule-name
                                                                            :expansion (list (ebnf:rule-name ebnf-child-rule)))))
                                      (loop for key in (ebnf:rule-first ebnf-child-rule)
                                            append
                                            (if (eq key 'ebnf:|_eps|)
                                                ;; find which expansion may be empty (having _eps in its first) and pick that one
                                                (when (rule-first-includes-empty-p ebnf-child-rule)
                                                  (list (mk-key (first rule-follow)) child-expansion-rule))
                                                (list (mk-key key) child-expansion-rule))))))))
                      (t (error "Found rule expansion which is neither sequence nor alternative."))))))))

(defparameter *transition-table*
  (let ((bnfsxp-location
          (asdf:system-relative-pathname :sparql-parser
                                         "external/sparql.bnfsxp")))
    (alexandria:plist-hash-table
     (construct-transition-table-from-parsed-bnf (ebnf:read-bnfsexp-from-file bnfsxp-location))
     :synchronized t)))


;;;;;;;;;;;;;;;;;;
;;;; Print helpers

(defmethod print-object ((rule rule) stream)
  (print-unreadable-object (rule stream :type "RULE")
    (format stream "~A => ~{~A~,^ ~}" (rule-name rule) (rule-expansion rule))))

(defmethod print-object ((scanned-token scanned-token) stream)
  (declare (special *scanning-string*))
  (print-unreadable-object (scanned-token stream :type "SCAN")
    (format stream "~A[~A,~A]~A"
            (scanned-token-token scanned-token)
            (scanned-token-start scanned-token)
            (scanned-token-end scanned-token)
            (scanned-token-effective-string scanned-token))))

(defun print-match (match &key (stream *standard-output*) (rulep t) (indentation-width 0))
  "Prints the match tree in a clean manner"
  (let* ((prefix-prepend " ")
         (start-prefix (loop for i below indentation-width collect prefix-prepend)))
    (labels ((print-it (match prefix)
               (if match
                   (let* ((submatches (match-submatches match))
                          (matched-token-p (and (= 1 (length submatches))
                                                (not (match-p (first submatches))))))
                     (cond (matched-token-p
                            (format stream "~&~{~A~}~A :: [~A]~%" prefix (match-term match) (first submatches)))
                           (rulep (format stream "~&~{~A~}~A :: ~A~%" prefix (match-term match) (match-rule match)))
                           (t (format stream "~&~{~A~}~A~%" prefix (match-term match))))
                     (unless matched-token-p
                       (dolist (submatch submatches)
                         (if (match-p submatch)
                             (print-it submatch (cons prefix-prepend prefix))
                             (format stream "~&~{~A~}[~A]~%" (cons prefix-prepend prefix) submatch)))))
                   (format stream "~&~{~A~} has match NIL~%" prefix))))
      (print-it match start-prefix))))

(defmethod print-object ((ast sparql-ast) stream)
  (let ((*scanning-string* (sparql-ast-string ast)))
    (declare (special *scanning-string*))
    (print-unreadable-object (ast stream :type "AST")
      (print-match (sparql-ast-top-node ast) :rulep nil :stream stream))))

;;;;;;;;;;;;;;;;;;;;;
;;;; Detecting tokens

(defparameter *token-parsers*
  (list 'ID (cl-ppcre:create-scanner "^([0-9]+|[A-Z][a-zA-Z0-9]*)"))
  "Listing of all things that can parse a token.")

(defun get-token-parser (token)
  "Returns a token parser for the given token."
  (if (stringp token)
      ;; scan the string
      (cl-ppcre:create-scanner (concatenate 'string "^" token))
      (getf *token-parsers* token)))

;; (defconstant +whitespace-scanner+ (cl-ppcre:create-scanner "^(\\s*(#[^\n]*\n)?)*" :multi-line-mode t)
;;   "Reusable scanner for whitespace between tokens.")

(defparameter *use-dedicated-whitespace-scanner* t)

(let ((scanner (cl-ppcre:create-scanner "^(\\s*(#[^
]*
)?)*"
                                        :multi-line-mode t)))
  (defun scan-whitespace (start string)
    "Scans for any whitespace or comments from START.

The result is the next position to start reading from.  If the string is
only whitespace, this will be one character further than the current
string.

START may be after the length of the current string, in this case START
is returned."
    (if *use-dedicated-whitespace-scanner*
        (sparql-terminals:scan-whitespace string start)
        (if (< start (length string))
            (multiple-value-bind (start end)
                (cl-ppcre:scan scanner string :start start)
              (declare (ignore start))
              end)
            start))))

(defun scan-token (token-specifiers start string)
  "Searches best matching token of TOKEN-SPECIFIERS at START in STRING.

A scanned token is returned if one could be found, otherwise nil.

No error is thrown if start is greater than or equal to the length of
the string, rather nil is returned."
  ;; SPARQL assumes the longest match the right match hence we must try
  ;; to scan all tokens.
  (when (< start (length string))
    (alexandria:when-let
        ((solution
          (alexandria:extremum
           (loop
             for (token . scanner) in token-specifiers
             ;; for scanner = (get-token-parser token)
             ;; for end-position = (second (multiple-value-list
             ;;                             (cl-ppcre:scan scanner string :start start)))
             for end-position = (funcall scanner string start)
             if end-position collect (cons token end-position))
           #'> :key #'cdr)))
      (make-scanned-token :start start
                          :end (cdr solution)
                          :token (car solution)))))

(defun calculate-next-token (token-specifiers start string)
  "Calculates the next token and position assuming TOKENS as options START
as the starting point in STRING."
  (let* ((token (scan-token token-specifiers start string)))
    (cond (token
           (values token
                   (scan-whitespace (scanned-token-end token) string)))
          ((>= start (length string))
           (values
            (make-scanned-token :start start :end start :token +END+)
            start))
          (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Parser implementation

(defvar *stack* nil "Place to store the items to be parsed.")
(defparameter *scanning-string* nil "The string to scan.")
(defvar *match-tree* nil "Place to store the entry point of the matching tree.")
(defvar *current-token* nil "Place to store the current token.")
(defvar *next-char-idx* 0 "Next character to read.")

(defun print-state ()
  "Prints the state."
  (format t "~&STACK: ~{~&~T~A~}~%TOKEN: ~A~%STRING: ~A~%TREE:~&"
          *stack*
          *current-token*
          (subseq *scanning-string* *next-char-idx*))
  (print-match *match-tree* :indentation-width 1))

(defun parse-setup (string)
  "Sets up variables for parsing the given set of tokens."
  ;; (setf *tokens* `(,@tokens ,+END+))
  (setf *scanning-string* string)
  (setf *next-char-idx*  0)
  (setf *current-token* nil)
  (setf *match-tree* (make-match :term *start-symbol*))
  (setf *stack* (list *match-tree* (make-match :term +END+))))

(define-condition ebnf-parse-error (simple-error)
  ((index :initarg :index :initform *next-char-idx*)
   (stack-top :initarg :stack-top))
  (:documentation "General error for use in EBNF parsing.  Should be instantiated for extra information."))

(define-condition no-matching-token (ebnf-parse-error)
  ((possible-tokens :initarg :possible-tokens))
  (:documentation "Error wthrown when next token could not be found during query parsing."))

(define-condition no-rule-found (ebnf-parse-error)
  ((next-token :initarg :next-token))
  (:documentation "Error wthrown when next token could not be found during query parsing."))

(defmethod print-object ((no-matching-token no-matching-token) stream)
  (with-slots (index possible-tokens stack-top) no-matching-token
    (format stream "No matching token found at position ~A~&Valid options: ~A~&Stack top: ~A"
            index
            (mapcar (lambda (thing) (let ((token (car thing)))
                                      (typecase token
                                        (string (format nil "\"~A\"" token))
                                        (t token))))
                    possible-tokens)
            stack-top)))

(defmethod print-object ((no-rule-found no-rule-found) stream)
  (with-slots (index stack-top next-token) no-rule-found
    (format stream
            "No rule found at position ~A~&Stack top: ~A, next token ~A"
            index stack-top next-token)))

(defun ensure-current-token ()
  "Calculates the current token if it is not known."
  (if *current-token*
      *current-token*
      (let* ((stack-top (match-term (car *stack*)))
             (next-token-list
               (if (terminalp stack-top)
                   (list (cons stack-top (sparql-terminals:scanner-for (if (stringp stack-top) (coerce stack-top #-be-cautious 'base-string #+be-cautious 'string) stack-top))))
                   (let ((transition-descriptions (gethash stack-top *transition-table*)))
                     (loop for (term . rest) on transition-descriptions
                             by #'cddr
                           collect term)))))
        (multiple-value-bind (token next-start)
            (calculate-next-token next-token-list *next-char-idx* *scanning-string*)
          (if (and token next-start)
              (progn
                (setf *current-token* token)
                (setf *next-char-idx* next-start)
                token)
              (error 'no-matching-token
                     :possible-tokens next-token-list
                     :stack-top stack-top
                     :index *next-char-idx*)
              ;; (error "Could not calculate token at index ~A for stack-top ~A.~&Searched tokens ~A"
              ;;        *next-char-idx* stack-top next-token-list)
              )))))

(defun get-stack-transition (table thing)
  "Like getf, but understands strings too"
  (loop for (label result) on table by #'cddr
        if (equal thing (car label))
          return result))

(defun parse-step ()
  "Executes one step of the LL1 parser."
  (let* ((stack-top-match (car *stack*))
         (stack-top (match-term stack-top-match))
         (next-token-object (ensure-current-token))
         (next-token (scanned-token-token next-token-object)))
    (cond
      ((and (eq next-token +END+)
            (eq stack-top +END+))
       ;; pop off end symbols and end processing
       (pop *stack*)
       (setf *current-token* nil)
       ;; (format t "~&Parsing succeeded.~%")
       (return-from parse-step t))
      ((terminalp stack-top)
       ;; terminals in the syntax need to be matched verbatim
       (if (equal stack-top next-token)
           (progn
             (setf (match-submatches stack-top-match) (list next-token-object))
             (pop *stack*)
             (setf *current-token* nil))
           (error "Error during parsing: Token ~A does not match ~A." next-token stack-top)))
      (t
       (alexandria:if-let ((stack-transitions (gethash stack-top *transition-table*)))
         (alexandria:if-let ((rule (get-stack-transition stack-transitions next-token)))
           (let ((stack-insertion (mapcar (lambda (term) (make-match :term term))
                                          (rule-expansion rule))))
             (setf (match-submatches stack-top-match)
                   stack-insertion)
             (setf (match-rule stack-top-match) rule)
             (setf *stack* `(,@stack-insertion
                             ,@(rest *stack*))))
           (error 'no-rule-found
                  :stack-top stack-top
                  :next-token next-token))
         (error "No transition rules found for state ~A" stack-top))))
    nil ; indicates another step exists
    ))

(defun ebnf-simplify (match)
  (labels ((list-simplified-match (target)
             (cond ((typep target 'scanned-token)
                    (list target))
                   ((typep (match-term target) 'string)
                    (list target))
                   ((char= #\_ (char (symbol-name (match-term target)) 0))
                    ;; can be expanded
                    (apply #'append (mapcar #'list-simplified-match (match-submatches target))))
                   (t
                    ;; a regular term
                    (list (prog1 target
                            (setf (match-submatches target)
                                  (apply #'append (mapcar #'list-simplified-match (match-submatches target))))))))))
    (setf (match-submatches match)
          (apply #'append (mapcar #'list-simplified-match (match-submatches match))))
    match))

(defun parse-string (string &key (max-steps 100000) (print-intermediate-states nil) (print-solution nil) (as-ebnf t))
  "Parses a set of tokens."
  (when print-intermediate-states
    (format t "~&===STACK START===~%")
    (print-state))
  (parse-setup string)
  (loop for i from 0 below max-steps
        do (when print-intermediate-states
             (format t "~&~%===PARSING STEP ~A===~%" i)
             (print-state))
        until (parse-step))
  (when as-ebnf
    (setf *match-tree* (ebnf-simplify *match-tree*)))
  (when print-solution
    (format t "~&===RESULT===~%")
    (print-match *match-tree* :rulep nil))
  (make-sparql-ast :top-node *match-tree*
                   :string *scanning-string*))

(defun get-parser-setup-state ()
  (list *stack*
        *scanning-string*
        *match-tree*
        *current-token*
        *next-char-idx*))

(defmacro with-reset-parser-setup-state (state &body body)
  "Executes within a state as defined by GET-PARSER-SETUP-STATE."
  `(destructuring-bind (*stack*
                        *scanning-string*
                        *match-tree*
                        *current-token*
                        *next-char-idx*)
       ,state
     ,@body))

(defun parse-sparql-string (string
                            &rest args
                            &key
                              (max-steps 100000)
                              (print-intermediate-states nil)
                              (print-solution nil)
                              (as-ebnf t)
                              (start-symbols (list 'ebnf::|QueryUnit| 'ebnf::|UpdateUnit|)))
  "Parses STRING as a SPARQL string either a QueryUnit or an UpdateUnit.

Types of query can be overridden optionally using :START-SYMBOLS keyword.

The first matching start symbol is returned and no further parsing
occurs.  If no matches could be found, the ebnf-parse-error with the
longest match is thrown."
  (declare (ignore max-steps print-intermediate-states print-solution as-ebnf))
  (setf *scanning-string* string) ; initialize the scanning string early
                                  ; on because it will be shadowed by
                                  ; with-reset-parser-setup-state
  (let ((parser-setup-initial-state (get-parser-setup-state))
        (keyword-args-to-pass (loop for (key value)
                                      on args by #'cddr
                                    unless (eq key :start-symbols)
                                      append (list key value))))
    (flet ((execute-query-parsing (start-symbol)
             (handler-case
                 (let ((sparql-parser::*start-symbol* start-symbol))
                   (cons :ok
                         (with-reset-parser-setup-state parser-setup-initial-state
                           (apply #'parse-string string keyword-args-to-pass))))
               (ebnf-parse-error (parsing-error)
                 (cons :error parsing-error)))))
      (let ((errors-as-list
              (loop for start-symbol in start-symbols
                    for (kind . result) = (execute-query-parsing start-symbol)
                    if (eq kind :ok)
                      ;; NOTE: early return when we found a solution
                      do (return-from parse-sparql-string result)
                    else
                      collect result)))
        (error (alexandria:extremum errors-as-list
                                    #'>
                                    :key (alexandria:rcurry #'slot-value 'index)))))))

(defmacro with-parser-setup (&body body)
  "Executes  body within the parser setup scope."
  ;; This macro could be removed if/when we convert other functions to
  ;; make less use of the special variables and more use of passed on
  ;; properties.  Benchmarks should indicate the validity of said case.
  `(let ((*stack* nil)
         (*scanning-string* nil)
         (*match-tree* nil)
         (*current-token* nil)
         (*next-char-idx* 0))
     ,@body))

(defmacro with-sparql-ast (sparql-ast &body body)
  "Executes BODY within the context of SPARQL-AST.

Used for accessing content."
  `(let ((*scanning-string* (sparql-ast-string ,sparql-ast)))
     ,@body))
