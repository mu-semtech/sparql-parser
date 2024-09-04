(in-package :quad-transformations)

(defparameter *user-quad-transform-functions* nil
  "List of quad transformation functions to try in the order in which they should be applied.")

(declaim (ftype (function (cons &key (:method (or (eql :insert) (eql :delete)))) cons) user-transform-delete-quads))
(defun user-transform-quads (quads &key method)
  "Transforms QUADS based on user rules for METHOD :insert or :delete.
   The rules may transform a single quad to no quad or to many quads."
  (if *user-quad-transform-functions*
      (let* ((any-quads-changed-p nil)
             (current-quads quads))
        (loop for function in *user-quad-transform-functions*
              do
                 (setf current-quads
                       (loop for quad in current-quads
                             for (new-quads changedp)
                               = (multiple-value-list (funcall function quad :method method))
                             when changedp
                               do (setf any-quads-changed-p t)
                             if changedp
                               append new-quads
                             else
                               append (list quad))))
        (values current-quads any-quads-changed-p))
      (values quads nil)))

(defun add-quad-processor (functor)
  "Adds a quad processor to the user quad processors.
   The function receives:
   - QUAD the quad up for replacement
   - METHOD which is either :insert or :delete

   The function should return (VALUES NEW-QUADS REPLACEP)
   - NEW-QUADS :: the quads for replacement if REPLACEP is non-nil
   - REPLACEP :: truethy iff new quads were supplied.

   Quads should not be modified, use COPY-QUAD first."
  (alexandria:appendf *user-quad-transform-functions* (list functor)))

(defmacro define-quad-transformation ((quad method) &body body)
  "Adds a quad processor to the user quad processors.
   - QUAD is bound to an individual QUAD in BODY
   - METHOD the variable name for the method, if it is a keyword, the
     quad will only be matched on that type (:INSERT or :DELETE).
   Should call REPLACE or KEEP as its last call."
  (let ((method-sym (gensym "METHOD"))
        (result-sym (gensym "RESULT-SYM"))
        (accept-p-sym (gensym "ACCEPT-P-SYM"))
        (used-internal-function-p-sym (gensym "USED-INTERNAL-FUNCTION-P-SYM"))
        (execute-body-sym (gensym "MVB-BODY")))
    `(add-quad-processor
      (lambda (,quad &key ,(if (keywordp method) method-sym method))
        (declare (ignorable method))
        (labels ((update (quads)
                   (cond
                     ((null quads)        ; empty list
                      (values nil t t))
                     ((listp (first quads)) ; list of quads
                      (values quads t t))
                     (t                   ; single quad
                      (values (list quads) t t))))
                 (keep ()
                   (values nil nil t))
                 (execute-body ()
                   (multiple-value-bind (result update-quad-p used-internal-function-p)
                       (progn ,@body)
                     (unless used-internal-function-p
                       (format t "~&[ERROR][QUAD-PROCESSOR] Quad processor user function did not call internal replacement function REPLACE or KEEP. Ignoring possible changes.~%"))
                     (values result update-quad-p))))
          ,(if (not (keywordp method))
               `(execute-body)
               `(if (eq ,method-sym ,method)
                    (,execute-body-sym)
                    (values ,quad nil))))))))
