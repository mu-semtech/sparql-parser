(in-package #:sparql-generator)

;;;; SPARQL generator
;;;;
;;;; Generates the SPARQL query for an EBNF by checking if AST matches
;;;; the EBNF.  Supports creating a string representation of the query.
;;;;
;;;; Due to the LL1 nature of the language, we should be able to
;;;; consistently select "the right" element by traversing the EBNF
;;;; together with the AST.

(defun ebnf-rule-name (rule)
  "Returns the name of the ebnf-rule."
  (second rule))

(defun ebnf-rule-expansion (rule)
  "Expansion for RULE."
  (fourth rule))

(defun ebnf-rule-index (rule)
  "Yields a number indicating the index for the EBNF rule."
  (parse-integer (third rule)))

(defparameter *sparql-ebnf* (support:read-bnfsexp-from-file "~/code/lisp/sparql-parser/external/sparql.ebnfsxp"))

(defparameter *sparql-ebnf-hash*
  (alexandria:alist-hash-table (mapcar (lambda (rule) (cons (ebnf-rule-name rule) rule)) *sparql-ebnf*)))

(defun find-rule (symbol)
  "Finds rule with rule-name being SYMBOL."
  ;; (find symbol *sparql-ebnf* :key #'ebnf-rule-name)
  (gethash symbol *sparql-ebnf-hash*))

(defun is-valid (match &key (rule (find-rule (sparql-parser:match-term match))) (deep-p t))
  "Verifies MATCH using EBNF.

If CURRENT-RULE is given, this as assumed to be the starting point,
otherwise MATCH-TERM is used through EBNF-RULE-NAME.

An error is thrown when the match is not valid.  The error specifies
which element was expected to be valid but was not."
  (let ((available-tokens (sparql-parser:match-submatches match)))
    (labels ((subrule-p (expansion)
               "Whether the expansion is a further subrule or not."
               (listp expansion))
             (consumed-all-tokens-p ()
               (null available-tokens))
             (pick-tokens (expansion &optional optional-p)
               ;; expansion
               ;; available-tokens
               (flet ((no-solution-found ()
                        (if optional-p
                            (return-from pick-tokens nil)
                            (error "Could not find match for:~% ~A~%With rule:~% ~A~%First failed expansion~% ~A~%Rest expansion~% ~A~%Available tokens:~% ~A~%All tokens: ~A"
                                   match rule
                                   (first expansion) (rest expansion)
                                   available-tokens (sparql-parser:match-submatches match)))))
                 (cond
                   ((subrule-p expansion)
                    (case (first expansion)
                      (sparql-bnf:|seq|
                       ;; match each token, return nil if we can't match the full
                       ;; sequence, pop tokens when they match.
                       ;; (rest expansion)
                       (loop for seq-element in (rest expansion)
                             for current-token = (first available-tokens)
                             for current-token-term = (and current-token
                                                           (sparql-parser::match-p current-token)
                                                           (sparql-parser:match-term current-token))
                             do
                                (progn
                                  ;; seq-element
                                  (cond ((subrule-p seq-element)
                                         (unless (pick-tokens seq-element optional-p)
                                           (no-solution-found)))
                                        ((and (typep seq-element 'string)
                                              (stringp current-token-term))
                                         (if (string= current-token-term seq-element)
                                             (pop available-tokens)
                                             (no-solution-found)))
                                        ((typep seq-element 'symbol)
                                         (if (eq current-token-term seq-element)
                                             (pop available-tokens)
                                             (no-solution-found)))
                                        (t (no-solution-found)))))
                       t)
                      (sparql-bnf:|alt|
                       ;; try any of the alternatives
                       (if (some (alexandria:rcurry #'pick-tokens t) (rest expansion))
                           t
                           (no-solution-found)))
                      (sparql-bnf:|star|
                       ;; keep picking solutions
                       (prog1 t
                         (loop
                           for tokens-cons = available-tokens
                           while (and (pick-tokens (second expansion) t)
                                      (not (eq tokens-cons available-tokens))))))
                      (sparql-bnf:|plus|
                       ;; pick one solution, then keep picking
                       (if (pick-tokens (second expansion) optional-p)
                           (prog1 t
                             (loop
                               for tokens-cons = available-tokens
                               while (and (pick-tokens (second expansion) t)
                                          ;; available-tokens changed
                                          (not (eq tokens-cons available-tokens)))))
                           (no-solution-found)))
                      (sparql-bnf:|opt|
                       (prog1 t
                         (pick-tokens (second expansion) t)))
                      (otherwise (error "Found ~A which is not understood for expansion." (first expansion)))))
                   ((symbolp expansion)
                    (if (typecase (first available-tokens)
                          (null nil)
                          (sparql-parser:match
                              (eq (sparql-parser:match-term (first available-tokens))
                                  expansion))
                          (sparql-parser:scanned-token
                           ;; we don't care about string because (symbolp expansion)
                           (eq (sparql-parser:scanned-token-token (first available-tokens))
                               expansion)))
                        (prog1 t (pop available-tokens))
                        (no-solution-found)))
                   ((and (stringp expansion)
                         (typep (first available-tokens) 'sparql-parser:match))
                    (typecase (sparql-parser:match-term (first available-tokens))
                      (string (when (string= expansion (sparql-parser:match-term (first available-tokens)))
                                (pop available-tokens)))
                      (t (no-solution-found))))
                   ((null available-tokens)
                    (no-solution-found))
                   (t
                    (error "Unknown expansion and available token combination~%Expansion:~% ~A~%Available tokens:~% ~A" expansion available-tokens)
                    (no-solution-found)))))
             (submatch-is-valid (submatch)
               (if (symbolp (sparql-parser:match-term submatch))
                   (is-valid submatch)
                   t)))
      (if (sparql-parser:terminalp (sparql-parser:match-term match))
          (let ((submatch (first (sparql-parser:match-submatches match))))
            (and (typep submatch 'sparql-parser:scanned-token)
                 (equal (sparql-parser:match-term match)
                        (sparql-parser:scanned-token-token submatch))))
          (and (pick-tokens (ebnf-rule-expansion rule))
               (or (consumed-all-tokens-p) (error "Did not consume all tokens in:~% ~A~%Leftover:~% ~A~%Rule: ~A" match available-tokens rule))
               (or (not deep-p)
                   (every #'submatch-is-valid (sparql-parser:match-submatches match))))))))

(defun write-valid (match)
  "Writes out MATCH assuming it was valid."
  ;; We would like to have a basic round-trip start->finish for
  ;; rendering the contents.  This is an effort for doing just that,
  ;; even though we know there are going to be some limitations in the
  ;; approach taken.

  ;; For humour's sake, let's first calculate the length we'd need,
  ;; create a string of the appropriate size, and then fill it in.
  (labels ((submatch-strings (match)
             (cond ((typep match 'sparql-parser:scanned-token)
                    (list (or (sparql-parser:scanned-token-string match)
                              (subseq sparql-parser::*scanning-string*
                                      (sparql-parser:scanned-token-start match)
                                      (sparql-parser:scanned-token-end match)))))
                   ((and (typep match 'sparql-parser:match)
                         (typep (sparql-parser:match-term match) 'string))
                    (list (sparql-parser:match-term match)))
                   (t
                    (loop for sub in (sparql-parser:match-submatches match)
                          append (submatch-strings sub))))))
    (format nil "~{~A~,^ ~}" (submatch-strings match))))
