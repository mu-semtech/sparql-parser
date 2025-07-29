(in-package #:handle-update-unit)

;; Converting into CONSTRUCT:
;;
;; There are multiple cases for which we may build a CONSTRUCT query instead of a SELECT, from easy to hard:
;;
;; - only `insert-patterns` or `delete-patterns` and [no GRAPH or not sudo]
;; - only `insert-patterns` or `delete-patterns` and not a sudo query
;; - `insert-patterns` and `delete-patterns` and [no GRAPH or not sudo]; with two queries
;;
;; The cases following this will require rewriting the CONSTRUCT part.  In essence: we only have three things
;; we can return in the CONSTRUCT.  But there are two aspects which we need to cater for:
;; 
;; 1. For sudo, there's G S P O to take into account, so one must be a known value.
;;
;; 2. In order to have one construction for insert-patterns and delete-patterns, we need one of S P O to be a known
;;    value for each quad statement.
;;
;; We can reasonably create a different CONSTRUCT if this is the case and construct the quads from there.
;;
;; - only `insert-patterns` or `delete-patterns` and sudo query with fixed single fixed GRAPH
;; - each `insert-pattern` or `delete-pattern` has one known resource (S P or O) and [no GARPH or not sudo]
;; - there is only a single pattern (all variables), and [known GRAPH or no GRAPH or not sudo]
;; - sudo with single pattern and G S P or O known

(defun filled-in-patterns (patterns bindings)
  "Creates a set of QUADS for the given patterns and bindings.

Any pattern which has no variables will be returned as is.  Any pattern
with bindings will be filled in for each discovered binding.  If any
variables are missing this will not lead to a pattern."
  (flet ((pattern-has-variables (pattern)
           (loop for (place match) on pattern by #'cddr
                 when (and (sparql-parser:match-p match)
                           (sparql-parser:match-term-p match 'ebnf::|VAR1| 'ebnf::|VAR2|))
                   do
                      (progn
                        ;; TODO: this should be integrated with *error-on-unwritten-data*.  If the quad doesn't exist
                        ;; _only_ because the graph is still a variable, then we would want to error on this case.
                        ;; (when (eq place :graph)
                        ;;   (format t "~&WARNING: Quad pattern contains graph variable ~A which is not supported, quad will be dropped ~A~%" match pattern))
                        (return t))))
         (fill-in-pattern (pattern bindings)
           (loop for (place match) on pattern by #'cddr
                 if (and (sparql-parser:match-p match)
                         (sparql-parser:match-term-p match 'ebnf::|VAR1| 'ebnf::|VAR2|)
                         (jsown:keyp bindings (subseq (terminal-match-string match) 1))) ; binding contains key (OPTIONAL in queries)
                   append (list place
                                (let ((solution (jsown:val bindings (subseq (terminal-match-string match) 1))))
                                  (if solution
                                      (binding-as-match solution)
                                      match)))
                 else
                   append (list place match))))
    (let* ((patterns-without-bindings (remove-if #'pattern-has-variables patterns))
           (patterns-with-bindings (set-difference patterns patterns-without-bindings :test #'eq)))
      (concatenate
       'list
       patterns-without-bindings        ; pattern without binding is quad
       (loop for binding in bindings
             append
             (loop for pattern in patterns-with-bindings
                   for filled-in-pattern = (fill-in-pattern pattern binding)
                   unless (pattern-has-variables filled-in-pattern)
                     collect filled-in-pattern))))))

(defun quad-operation-to-quads (operation)
  "Converts a quad-operation to the corresponding insert-quads and delete-quads."
  ;; TODO: handle WITH iriref which should be removed for non sudo queries
  (case (operation-type operation)
    (:insert-triples
     (list :insert-quads (operation-data operation)))
    (:delete-triples
     (list :delete-quads (operation-data operation)))
    (:modify
     (let ((insert-patterns (operation-data-subfield operation :insert-patterns))
           (delete-patterns (operation-data-subfield operation :delete-patterns))
           (bindings (client:batch-create-full-solution-for-select-query
                      (operation-data-subfield operation :query)
                      :for :modify :usage :read)))
       (if bindings
           (let* ((filled-in-deletes (filled-in-patterns delete-patterns bindings))
                  (filled-in-inserts (filled-in-patterns insert-patterns bindings)))
             ;; TODO: Optionally error when INSERT or DELETE template does not contain variables AND no solution in WHERE
             (list :delete-quads filled-in-deletes :insert-quads filled-in-inserts)))))))
