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

(defun is-valid (match &optional (rule (find-rule (sparql-parser:match-term match))))
  "Verifies MATCH using EBNF.

If CURRENT-RULE is given, this as assumed to be the starting point,
otherwise MATCH-TERM is used through EBNF-RULE-NAME."
  (let ((available-tokens (sparql-parser:match-submatches match)))
    (labels ((subrule-p (expansion)
               "Whether the expansion is a further subrule or not."
               (listp expansion))
             (consumed-all-tokens-p ()
               (null available-tokens))
             (pick-tokens (expansion)
               ;; expansion
               ;; available-tokens
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
                                       (unless (pick-tokens seq-element)
                                         (return-from pick-tokens nil)))
                                      ((and (typep seq-element 'string)
                                            (stringp current-token-term))
                                       (if (string= current-token-term seq-element)
                                           (pop available-tokens)
                                           (return-from pick-tokens nil)))
                                      ((typep seq-element 'symbol)
                                       (when (eq current-token-term seq-element)
                                         (pop available-tokens)))
                                      (t (return-from pick-tokens nil)))))
                     t)
                    (sparql-bnf:|alt|
                     ;; try any of the alternatives
                     (some #'pick-tokens (rest expansion)))
                    (sparql-bnf:|star|
                     ;; keep picking solutions
                     (loop
                       for tokens-cons = available-tokens
                       while (and (pick-tokens (second expansion))
                                  (not (eq tokens-cons available-tokens))))
                     t)
                    (sparql-bnf:|plus|
                     ;; pick one solution, then keep picking
                     (when (pick-tokens (second expansion))
                       (loop
                         for tokens-cons = available-tokens
                         while (and (pick-tokens (second expansion))
                                    (not (eq tokens-cons available-tokens))))))
                    (sparql-bnf:|opt|
                     (prog1 t
                       (pick-tokens (second expansion))))
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
                      nil))
                 ((and (stringp expansion)
                       (typep (first available-tokens) 'sparql-parser:match))
                  (typecase (sparql-parser:match-term (first available-tokens))
                    (string (when (string= expansion (sparql-parser:match-term (first available-tokens)))
                              (pop available-tokens)))
                    (t nil)))
                 ((null available-tokens)
                  nil)
                 (t nil)))
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
               (consumed-all-tokens-p)
               (every #'submatch-is-valid (sparql-parser:match-submatches match)))))))

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
                    (list (subseq sparql-parser::*scanning-string*
                                  (sparql-parser:scanned-token-start match)
                                  (sparql-parser:scanned-token-end match))))
                   ((and (typep match 'sparql-parser:match)
                         (typep (sparql-parser:match-term match) 'string))
                    (list (sparql-parser:match-term match)))
                   (t
                    (loop for sub in (sparql-parser:match-submatches match)
                          append (submatch-strings sub))))))
    (format nil "~{~A~,^ ~}" (submatch-strings match))))
