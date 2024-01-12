(in-package #:detect-quads)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Detect quads framework
;;;;
;;;; Provides abstractions on how to interpret the SPARQL EBNF for
;;;; detecting quads.
;;;;
;;;; Processing the EBNF requires passing content down, up and sideways.
;;;; However, it seems feasible to process content depth-first
;;;; left-to-right assuming each step in between gets its say in the
;;;; down and up-pass.
;;;;
;;;; To support this an info object is supplied.  This can provide a
;;;; value for each setting.  Its properties can be intiialized at each
;;;; level which means the property is scoped to children.
;;;;
;;;; Each level creates a function which can be called directly in order
;;;; to process that level.
;;;;
;;;; The entry-point is the handle macro.  This constructs a function to
;;;; initialize the state and provides various approaches to handle the
;;;; term.  The most commonly used is :process which is a list of terms
;;;; for which the responsibility is passed down.  See the comment for
;;;; more information in this macro.

(defpackage #:detect-quads-processing-handlers
  (:export #:|UpdateUnit|))

(defstruct info
  (prefixes)
  (base)
  (pname-ns)
  (operations)
  (quads)
  (graph)
  (subject)
  (predicate)
  (delete-quad-patterns)
  (insert-quad-patterns))

(declaim (special *info*))

(defmacro with-shadowing-context-for ((&rest property-plist) &body body)
  "Executes body with a shadowing context for the supplied key-value pairs
in property-plist.  These properties are expected to exist on the info
struct with similarly named accessor functions."
  (if property-plist
      (let ((local-variables-gensym
              (loop for (key) on property-plist by #'cddr
                    collect (gensym (format nil "ORIGINAL-~A" (symbol-name key))))))
        `(let (,@(loop for (key value) on property-plist by #'cddr
                       for local-var in local-variables-gensym
                       collect `(,local-var (,(intern (format nil "INFO-~A" (symbol-name key)))
                                             *info*))))
           ,@(loop for (key value) on property-plist by #'cddr
                   collect `(setf (,(intern (format nil "INFO-~A" (symbol-name key)))
                                   *info*)
                                  ,value))
           (prog1
               (progn ,@body)
             ,@(loop for (key value) on property-plist by #'cddr
                     for local-var in local-variables-gensym
                     collect `(setf (,(intern (format nil "INFO-~A" (symbol-name key)))
                                     *info*)
                                    ,local-var)))))
      (if (= 1 (length body))
          (first body)
          `(progn ,@body))))

(defun test-with-shadowing-context-for (value)
  (let ((start "hello"))
   (let ((*info* (make-info :prefixes start)))
     (with-shadowing-context-for (:prefixes value)
       (incf value 42)
       (format t "~&Info contains ~A and value is now ~A which differs by ~A~%"
               (info-prefixes *info*) value (abs (- value (info-prefixes *info*))))
       (assert (= 42 (abs (- value (info-prefixes *info*)))))
       9001)
     (format t "~&After evaluating, info-prefixes contains ~A which should be ~A~%"
             (info-prefixes *info*) start)
     (assert (equalp (info-prefixes *info*) start)))))

(defun term-handler-symbol (term)
  "Yields the name of the function to handle a given term."
  (intern (symbol-name term) (find-package '#:detect-quads-processing-handlers)))

(defmacro handle (term &key function note todo process accept local-context process-functions not-supported after &allow-other-keys)
  "We assume symbols are coming from a single other package.  This is correct in our setting but would otherwise be a strange way of using these symbols.

The most common goal for these handlers is to update the *info* object
to pass the necessary contextual information, yet the return value of
the function is shared back upward (this is used to get information
about the URI of something URI-like).

 - local-context :: properties to set/reset on *info* struct
 - function :: function to process the match, everything below is ignored in this case
 - process :: list of submatch terms that should be handled through this system
 - process-functions :: functions used to specifically process certain submatch types
 - accept :: accept a result without further descending, if this is the last result it is returned
 - after :: function which is called after calculations are made and which can alter the response
 - not-supported :: list of submatch terms that are explicitly not supported, throws error on match
 - note :: a note, ignored
 - todo :: a note on desired actions, ignored"
  (declare (ignore note todo))
  (let ((function-name (term-handler-symbol term))
        (match-gensym (gensym "MATCH"))
        (submatch-gensym (gensym "SUBMATCH"))
        (loop-result-gensym (gensym "LOOP-RESULT"))
        (result-gensym (gensym "RESULT")))
    `(defun ,function-name (,match-gensym)
       ;; check if the term is what we expect it to be
       (assert (eq (match-term ,match-gensym) ',term))
       ;; set up the context for processing children
       ;; (format t "~&Processing ~A~%" ,match-gensym)
       (with-shadowing-context-for (,@local-context)
         ;; loop over each of the sub-terms switch-case what we should execute
         (let
             ((,result-gensym
                ,(if function
                     (destructuring-bind ((variable) &body body)
                         function
                       `(let ((,variable ,match-gensym))
                          ,@body))
                     `(let (,loop-result-gensym)
                        (loop for ,submatch-gensym in (sparql-parser:match-match-submatches ,match-gensym)
                              unless (stringp (match-term ,submatch-gensym)) ;; TODO: find nicer way to skip these
                                do (setf ,loop-result-gensym
                                         (case (match-term ,submatch-gensym)
                                           ,@(loop for (term (var) . body) in process-functions
                                                   collect `(,term (let ((,var ,submatch-gensym))
                                                                     ,@body)))
                                           ,@(loop for term in not-supported
                                                   ;; TODO: create specific condition for unsupported terms
                                                   collect `(,term (error "Term ~A is not supported" ',term)))
                                           ,@(loop for term in process
                                                   collect `(,term (,(term-handler-symbol term) ,submatch-gensym)))
                                           ,@(loop for term in accept
                                                   collect `(,term ,submatch-gensym))
                                           (otherwise (error "Unexpected term ~A" (match-term ,submatch-gensym))))))
                        ,loop-result-gensym))))
           ,(if after
                (destructuring-bind ((result match) &body body)
                    after
                  `(let ((,result ,result-gensym)
                         (,match ,match-gensym))
                     ,@body))
                result-gensym))))))
