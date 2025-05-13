(in-package :quad)

(deftype quad (cons))

;;;; Helper functions for processing quads

(defun make-quad (&key subject predicate object graph)
  "Constructs a new quad."
  (list :subject subject
        :predicate predicate
        :object object
        :graph graph))
(defun copy-quad (quad)
  "Copies a quad."
  (make-quad :subject (subject quad)
             :predicate (predicate quad)
             :object (object quad)
             :graph (graph quad)))

(defun quad-subject (quad)
  "Yields the subject of the quad."
  (getf quad :subject))
(defun (setf quad-subject) (value quad)
  (setf (getf quad :subject) value))

(defun quad-predicate (quad)
  "Yields the predicate of the quad."
  (getf quad :predicate))
(defun (setf quad-predicate) (value quad)
  (setf (getf quad :predicate) value))

(defun quad-object (quad)
  "Yields the object of the quad."
  (getf quad :object))
(defun (setf quad-object) (value quad)
  (setf (getf quad :object) value))

(defun quad-graph (quad)
  "Yields the graph of the quad."
  (getf quad :graph))
(defun (setf quad-graph) (value quad)
  (setf (getf quad :graph) value))

(defun to-human-string (quad)
  "Prints QUAD to a human string."
  (format nil "~A ~A ~A ~A"
          (quad-part-as-string (quad-graph quad))
          (quad-part-as-string (quad-subject quad))
          (quad-part-as-string (quad-predicate quad))
          (quad-part-as-string (quad-object quad))))

(defun quad-part-as-string (quad-part)
  "Gets string representation for one of `QUAD-GRAPH' `QUAD-SUBJECT' `QUAD-PREDICATE' or `QUAD-OBJECT'."
  (typecase quad-part
    (cons (sparql-manipulation:uri-wrap-marks (cdr quad-part)))
    (nil "UNDEF")
    (sparql-parser:match (sparql-generator:write-valid-match quad-part))
    ;; NOTE: following case should not happen but we don't want to throw real errors for a log.
    (t "ERROR")))

;; We can bind a bunch of functions automatically so we drop "-?quad-?"
;; from the symbol for nicer `quad:predicate` usage externally
(loop for key in '(make-quad copy-quad quad-subject quad-predicate quad-object quad-graph)
      for name = (symbol-name key)
      for short-name = (cl-ppcre:regex-replace (format nil "-?~A-?" (package-name *package*)) name "")
      for short-symbol = (intern short-name *package*)
      do
         (setf (fdefinition short-symbol)
               (fdefinition key))
      when (fboundp `(setf ,key))
        do
           (setf (fdefinition `(setf ,short-symbol))
                 (fdefinition `(setf ,key))))
