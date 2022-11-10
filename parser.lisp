(in-package :sparql-parser)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; rules and solution structures
(defstruct rule
  "Expresses an expansion rule."
  (name (error "Must supply rule name") :type symbol)
  (expansion nil :type list))

(defstruct match
  (term (error "Must supply term when creating a match") :type (or symbol string))
  (rule nil :type (or null rule))        ; nil when rule is not known yet
  (submatches nil :type list))

(defstruct scanned-token
  "Represents a scanned token."
  (start (error "Must supply token start") :type integer)
  (end (error "Must supply token end") :type integer)
                                        ; end is right after the scanned
                                        ; character.  ie: if START is 0,
                                        ; the result 0 would mean no
                                        ; characters are read to match
                                        ; the token.
  (token (error "Must supply matched token") :type (or symbol string)))

(defun terminalp (thing)
  "Returns truethy iff symbol represents a terminal.

We accept strings and uppercase symbols as terminals."
  (and
   (or (stringp thing)
       (cl-ppcre:scan "^[A-Z_0-9]+$" (symbol-name thing)))
   t))

;;;;;;;;;;;;;;
;;;; Constants

(defconstant +END+ 'sparql-bnf:|_eof| ;; :end-eof
             "Last token to be processed.")
(defconstant +EMPTY+ :empty-statement "Indicates a statement which carries no content")


;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Stub language rules

(defparameter *rules*
  (mapcar (lambda (specification)
            (destructuring-bind (name &rest expansion) specification
              (make-rule :name name :expansion expansion)))
          `((:|statement| "if" :|expression| "then" :|statement| "else" :|statement|)
            (:|statement| "while" :|expression| "do" :|statement|)
            (:|statement| "begin" :|statements| "end")
            (:|statements| :|statement| ";" :|statements|)
            (:|statements|)
            (:|expression| ID)))
  "All known rules")

(defparameter *start-symbol* 'sparql-bnf::|UpdateUnit|
  "The symbol used to start processing.")
;; (defparameter *start-symbol* 'sparql-bnf::|QueryUnit|
;;   "The symbol used to start processing.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Construction of transition table

(defun find-rules (rule-name)
  "Finds all rules with name RULE-NAME."
  (remove-if-not (alexandria:curry #'eq rule-name)
                 *rules*
                 :key #'rule-name))

(defun rule-may-be-empty (rule-name)
  "Yields non-nill iff the syntax rule with name RULE-NAME may have an empty result."
  (some (alexandria:compose #'null #'rule-expansion)
        (find-rules rule-name)))

(defun predict-set-for-symbol (symbol rules)
  "Calculate the predict set for SYMBOL in RULES."
  (loop for rule in rules
        when (eq (rule-name rule) symbol)
          append (predict-set-for-rule rule)))

(defun multi-sorted-keyword-union (&rest lists)
  "Unions multiple lists together."
  (remove-duplicates
   (sort (loop for list in lists
               for result = list then (union result list)
               finally (return result))
         #'string<
         :key (lambda (thing) (if (symbolp thing) (symbol-name thing) thing)))))

(defun group-by (items &key (key #'identity) (test #'eq) (value #'identity))
  "Groups supplied items by the given property."
  (loop for item in items
        for item-key = (funcall key item)
        for foundp = nil
        for item-value = (funcall value item)
        for grouped-list = nil
          then (loop for (list-key . values) in grouped-list
                     if (funcall test item-key list-key)
                       collect (prog1
                                   `(,list-key ,item-value ,@values)
                                 (setf foundp t))
                     else
                       collect (cons list-key values))
        unless foundp
          do (setf grouped-list `((,item-key ,item-value) ,@grouped-list))
        finally (return grouped-list)))

(defun predict-sets (rules)
  "Constructs the predict sets for each non-terminal.

The first key is the state under inspection (IE the name of the rule).
The second property is the next token on the stack.  The third item is
the rule to be selected in said case."
  (let ((predict-sets (tree-db:create)))
    (loop
      for previous-predict-sets = (tree-db:copy predict-sets)
      do
         (loop for rule in rules
               for name = (rule-name rule)
               for expansion = (rule-expansion rule)
               for first-expansion = (first expansion)
               do
                  (cond ((not expansion) nil)
                        ((terminalp first-expansion)
                         ;; if it is a primitive, this is the rule to select for the primitive value
                         (setf (tree-db:val predict-sets (list name first-expansion))
                               rule))
                        (t
                         ;; it is not a terminal, hence all known solutions for
                         ;; the child element should be our solutions
                         (loop for (start-state primitive other-rule)
                                 in (tree-db:keys-at predict-sets (list first-expansion))
                               do
                                  ;; we set the rule detected above to be
                                  ;; selected for this primitive further
                                  ;; down the line.
                                  (setf (tree-db:val predict-sets (list name primitive))
                                        rule)))))
      until (tree-db:equal predict-sets previous-predict-sets))
    predict-sets))

(defun starts-sets (rules)
  "Constructs the next sets for each non-terminal"
  (loop for (rule-name . known-terminals)
          in (group-by (tree-db:all-keys (predict-sets rules)) :key #'car :value #'second)
        append (list rule-name
                     (loop for (terminal) on known-terminals by #'cddr
                           collect terminal))))

(defun next-set (rules)
  "Constructs the next set for each term of RULES.

The NEXT symbols are the symbols which may appear after the current
symbol.  It is important when coping with symbols that may be empty."
  ;; something is in the predict set for rule A if
  ;;
  ;; 1. it is in the start set of the thing following rule,
  ;;
  ;; 2. if something B elementOf predict set of rule A and B may be
  ;; null, then anything in the predict set of B is in the predict set.
  (let ((next-sets nil)
        (previous-next-sets nil)
        (starts-sets (starts-sets rules)))
    (loop
      do
         (setf previous-next-sets (copy-tree next-sets))
         (loop for rule in rules
               for name = (rule-name rule)
               for expansion = (rule-expansion rule)
               do
                  (loop for (first next rest) on expansion
                        do
                           (unless (terminalp first)
                             (let* ((next-assoc-sets-of-first (or (assoc first next-sets)
                                                                  (let ((cell (cons first nil)))
                                                                    (push cell next-sets)
                                                                    cell)))
                                    (next-sets-of-first (cdr next-assoc-sets-of-first))
                                    (starts-sets-of-first (getf starts-sets first)))
                               (cond ((terminalp next)
                                      (setf (cdr next-assoc-sets-of-first)
                                            (multi-sorted-keyword-union
                                             (and next (list next))
                                             next-sets-of-first
                                             starts-sets-of-first)))
                                     ((rule-may-be-empty first)
                                      (let ((next-sets-of-next (cdr (or (assoc (rule-name next) next-sets) (cons nil nil)))))
                                        (setf (cdr next-assoc-sets-of-first)
                                              (multi-sorted-keyword-union
                                               starts-sets-of-first
                                               next-sets-of-first
                                               next-sets-of-next))))
                                     (t ; rule not empty and next not terminal
                                      (let ((next-sets-of-next (cdr (or (assoc (rule-name next) next-sets) (cons nil nil)))))
                                        (setf (cdr next-assoc-sets-of-first)
                                              (multi-sorted-keyword-union
                                               next-sets-of-first
                                               starts-sets-of-first
                                               next-sets-of-next)))))))))
      until (alexandria:set-equal next-sets previous-next-sets
                                  :test (lambda (a b)
                                          (and (eq (car a) (car b))
                                               (alexandria:set-equal (cdr a) (cdr b))))))
    (loop for (statement . properties) in next-sets
          append (list statement properties))))

(defun empty-rule-for-rule-name (rule-name rules)
  "Returns the empty rule for RULE-NAME in RULES or NIL if not found."
  (find-if (lambda (rule) (and (eq (rule-name rule) rule-name)
                               (null (rule-expansion rule))))
           rules))

(defun ebnf-rule-name (rule)
  "Name of the EBNF rule."
  (second rule))

(defun ebnf-rule-type (rule)
  "Type of the ebnf rule."
  (first rule))

(defun ebnf-rule-terminal-p (rule)
  "Returns truethy iff the rule is a terminal specification."
  (eq (ebnf-rule-type rule) 'sparql-bnf:|terminal|))

(defun ebnf-rule-values-for (rule key)
  "Gets values for KEY in RULE."
  (loop for props in (rest rule)
        when (and (listp props)
                  (eq (first props) key))
          do (return-from ebnf-rule-values-for
               (values (mapcar (lambda (thing)
                                 (if (typep thing 'string)
                                     (coerce thing 'base-string)
                                     thing))
                               (rest props))
                       t)))
  (values nil nil))

(defun ebnf-rule-first (rule)
  "Get first set of RULE."
  (ebnf-rule-values-for rule 'sparql-bnf:|first|))

(defun ebnf-rule-follow (rule)
  "Get first set of RULE."
  (ebnf-rule-values-for rule 'sparql-bnf:|follow|))

(defun ebnf-rule-expansion (rule)
  "Returns the rule expansion for RULE."
  (multiple-value-bind (seq seqp)
      (ebnf-rule-values-for rule 'sparql-bnf:|seq|)
    (if seqp
        (cons 'sparql-bnf:|seq| seq)
        (cons 'sparql-bnf:|alt| (ebnf-rule-values-for rule 'sparql-bnf:|alt|)))))

(defun ebnf-rule-search (rules key)
  "Searches a list of BNF rules for the given rule name."
  (find key rules :key #'second))

(defun construct-transition-table (rules)
  "Constructs the transition table for RULES."
  (let* ((next-set (next-set rules))
         (predict-sets (predict-sets rules)))
    (loop for (rule-name . rules) in (group-by rules :key #'rule-name)
          append
          (list rule-name
                (let ((predicted
                        (loop for (rule-name primitive)
                                in (tree-db:keys-at predict-sets (list rule-name))
                              for rule = (tree-db:val predict-sets (list rule-name primitive))
                              append (list primitive rule))))
                  (if (rule-may-be-empty rule-name)
                      (loop for symbol in (getf next-set rule-name)
                            for current-prediction = (getf predicted symbol)
                            if current-prediction
                              append (list symbol current-prediction)
                            else
                              append (list symbol (empty-rule-for-rule-name rule-name rules)))
                      predicted))))))

(defun construct-transition-table-from-parsed-bnf (parsed-bnf)
  "Import EBNF converted through Ruby's EBNF module to BNF and written as s-expressions."
  (let ((empty-rule (make-rule :name 'sparql-bnf:|_empty| :expansion nil)))
    (loop
      for rule in parsed-bnf
      for rule-name = (ebnf-rule-name rule)
      for rule-expansion = (ebnf-rule-expansion rule)
      for rule-expansion-type = (first rule-expansion)
      for rule-expansion-options = (rest rule-expansion)
      for rule-first-all = (ebnf-rule-first rule)
      for rule-first-includes-empty-p = (some (lambda (k) (eq k 'sparql-bnf:|_eps|)) rule-first-all)
      for rule-first-options = (remove-if (lambda (k) (eq k 'sparql-bnf:|_eps|)) rule-first-all)
      for rule-follow = (ebnf-rule-follow rule)
      unless (ebnf-rule-terminal-p rule)
        append
        (list (ebnf-rule-name rule)
              (cond ((eq rule-expansion-type 'sparql-bnf:|seq|)
                     ;; sequence
                     (let* ((rule (make-rule :name rule-name :expansion rule-expansion-options))
                            (predicted (loop for first in rule-first-options
                                             append (list first rule)))
                            (empty-follow (when rule-first-includes-empty-p
                                            ;; follow will contain _empty deeper down the stack
                                            (loop for follow in rule-follow
                                                  append (list follow rule)))))
                       (concatenate 'list predicted empty-follow)))
                    ((eq rule-expansion-type 'sparql-bnf:|alt|)
                     ;; alternatives
                     ;;
                     ;; although this will result in duplicate rules,
                     ;; we are generating rules on the fly here.
                     (loop for option in rule-expansion-options
                           append
                           (cond ((eq option 'sparql-bnf:|_empty|)
                                  (loop for key in (cons 'sparql-bnf:|_eof| rule-follow)
                                        append (list key empty-rule)))
                                 ((terminalp option)
                                  (list option (make-rule :name rule-name
                                                          :expansion (list option))))
                                 (t ;; a subselection
                                  (let* ((ebnf-child-rule (ebnf-rule-search parsed-bnf option))
                                         (child-rule (make-rule :name rule-name
                                                                :expansion (list (ebnf-rule-name ebnf-child-rule)))))
                                    (loop for key in (ebnf-rule-first ebnf-child-rule)
                                          unless (eq key 'sparql-bnf:|_eps|)
                                          append (list key child-rule)))))))
                    (t (error "Found rule expansion which is neither sequence nor alternative.")))))))

;; eg: (defparameter *transition-table* (construct-transition-table-from-parsed-bnf (read-bnfsexp-from-file "~/code/lisp/sparql-parser/external/sparql.bnfsxp")))
(defparameter *transition-table* (construct-transition-table-from-parsed-bnf (support:read-bnfsexp-from-file "~/code/lisp/sparql-parser/external/sparql.bnfsxp")))

;; (defparameter *transition-table* (construct-transition-table *rules*)
;;   "Transition table [stack top, next symbol] => next rule")

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
            (subseq *scanning-string*
                    (scanned-token-start scanned-token)
                    (scanned-token-end scanned-token)))))

(defun print-match (match &key (stream *standard-output*) (rulep t) (indentation-width 0))
  "Prints the match tree in a clean manner"
  (let* ((prefix-prepend " ")
         (start-prefix (loop for i below indentation-width collect prefix-prepend)))
    (labels ((print-it (match prefix)
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
                         (format stream "~&~{~A~}[~A]~%" (cons prefix-prepend prefix) submatch)))))))
      (print-it match start-prefix))))

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

(let ((scanner (cl-ppcre:create-scanner
                           "^(\\s*(#[^
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

(defun scan-token (tokens start string)
  "Searches best matching token of TOKENS at START in STRING.

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
             for token in tokens
             ;; for scanner = (get-token-parser token)
             ;; for end-position = (second (multiple-value-list
             ;;                             (cl-ppcre:scan scanner string :start start)))
             for end-position = (sparql-terminals:scan token start string)
             if end-position collect (cons token end-position))
           #'> :key #'cdr)))
      (make-scanned-token :start start
                          :end (cdr solution)
                          :token (car solution)))))

(defun calculate-next-token (tokens start string)
  "Calculates the next token and position assuming TOKENS as options START
as the starting point in STRING."
  (let* ((token (scan-token tokens start string)))
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

(defun ensure-current-token ()
  "Calculates the current token if it is not known."
  (if *current-token*
      *current-token*
      (let* ((stack-top (match-term (car *stack*)))
             (next-token-list
              (if (terminalp stack-top)
                  (list stack-top)
                  (let ((transition-descriptions (getf *transition-table* stack-top)))
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
              (error "Could not calculate token at index ~A for stack-top ~A.~&Searched tokens ~A"
                     *next-char-idx* stack-top next-token-list))))))

(defun get-stack-transition (table thing)
  "Like getf, but understands strings too"
  (loop for (label result) on table by #'cddr
        if (equal thing label)
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
       (alexandria:if-let ((stack-transitions (getf *transition-table* stack-top)))
         (alexandria:if-let ((rule (get-stack-transition stack-transitions next-token)))
           (let ((stack-insertion (mapcar (lambda (term) (make-match :term term))
                                          (rule-expansion rule))))
             (setf (match-submatches stack-top-match)
                   stack-insertion)
             (setf (match-rule stack-top-match) rule)
             (setf *stack* `(,@stack-insertion
                             ,@(rest *stack*))))
           (error "No rule found for state ~A with token ~A" stack-top next-token))
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

(defun parse-string (string &key (max-steps 10000) (print-intermediate-states t) (print-solution t) (as-ebnf t))
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
    (print-match *match-tree* :rulep nil)))

