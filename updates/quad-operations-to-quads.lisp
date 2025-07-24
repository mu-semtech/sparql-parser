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
