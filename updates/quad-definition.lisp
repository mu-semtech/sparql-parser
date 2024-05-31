(in-package #:quad)

(defstruct (quad (:copier copy))
  "A single triple with location and metadata regarding storage in the triplestore."
  (subject nil)
  (predicate nil)
  (object nil)
  (graph nil)
  (persist-p t :type (or null t)))

(defun subject (quad) (quad-subject quad))
(defun (setf subject) (value quad) (setf (quad-subject quad) value))

(defun predicate (quad) (quad-predicate quad))
(defun (setf predicate) (value quad) (setf (quad-predicate quad) value))

(defun object (quad) (quad-object quad))
(defun (setf object) (value quad) (setf (quad-object quad) value))

(defun graph (quad) (quad-graph quad))
(defun (setf graph) (value quad) (setf (quad-graph quad) value))

(defun persist-p (quad) (quad-persist-p quad))
(defun (setf persist-p) (value quad) (setf (quad-persist-p quad) value))

(defun getter-for-keyword (key)
  "Yields the getter function which matches KEY."
  (case key
    (:subject #'quad:subject)
    (:predicate #'quad:predicate)
    (:object #'quad:object)
    (:graph #'quad:graph)))

(defun get-key (quad key)
  "Gets KEY from QUAD, mapping the keyword parameters to function calls."
  (funcall (getter-for-keyword key) quad))
