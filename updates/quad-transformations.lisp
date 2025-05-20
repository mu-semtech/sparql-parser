(in-package :quad-transformations)

(defparameter *user-quad-transform-functions* nil
  "List of quad transformation functions to try in the order in which they should be applied.")

(declaim (ftype (function ((vector acl:dispatched-quad) &key (:method (or (eql :insert) (eql :delete)))) (vector acl:dispatched-quad)) user-transform-quads))
(defun user-transform-quads (dispatched-quads &key method)
  "Transforms QUADS based on user rules for METHOD :insert or :delete.
   The rules may transform a single quad to no quad or to many quads."
  (if (and *user-quad-transform-functions* (length dispatched-quads))
      (let* ((any-quads-changed-p nil)
             (resulting-quad-count 0)
             (extra-quads nil)
             ;; we assume the most common case is to have the same amount of quads
             (result (make-array (length dispatched-quads)
                                 :element-type (array-element-type dispatched-quads)
                                 :initial-contents dispatched-quads)))
        (loop for dispatched-quad across dispatched-quads
              for add-current-quad = t
              do
                 (loop for function in *user-quad-transform-functions*
                       for (new-quads changedp)
                         = (multiple-value-list
                            (funcall function
                                     (acl:dispatched-quad-quad dispatched-quad)
                                     :method method))
                       when changedp
                         do (setf any-quads-changed-p t
                                  add-current-quad nil)
                            (loop for new-quad in new-quads
                                  for new-dispatched-quad = (acl:copy-dispatched-quad dispatched-quad)
                                  do
                                     (setf (acl:dispatched-quad-quad new-dispatched-quad) new-quad)
                                  if (< resulting-quad-count (length result))
                                    do (setf (aref result resulting-quad-count) new-dispatched-quad)
                                  else
                                    do (push new-dispatched-quad extra-quads)
                                  do
                                     (incf resulting-quad-count)))
              when add-current-quad
                do (if (< resulting-quad-count (length result))
                       (setf (aref result resulting-quad-count) dispatched-quad)
                       (push dispatched-quad extra-quads))
                   (incf resulting-quad-count))
        (cond ((= resulting-quad-count (length result))
               (values result any-quads-changed-p))
              (extra-quads (let ((r (make-array (list resulting-quad-count)
                                                :element-type 'acl:dispatched-quad
                                                :initial-element (aref dispatched-quads 0))))
                             (loop for i from 0 for x across result do (setf (aref r i) x))
                             (loop for i from resulting-quad-count for x in extra-quads do (setf (aref r i) x))
                             (values r t)))
              (t (let ((r (make-array (list resulting-quad-count)
                                      :element-type 'acl:dispatched-quad
                                      :initial-element (aref dispatched-quads 0))))
                   (loop for i from 0 below resulting-quad-count do (setf (aref r i) (aref result i)))
                   (values r t)))))
      (values dispatched-quads nil)))

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
