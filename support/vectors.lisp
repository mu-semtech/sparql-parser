(in-package :support)

(defun filter-array (input-array condition)
  "Filters INPUT-ARRAY based on CONDITION called with one element of INPUT-ARRAY.

Assumes CONDITION is cheap to run and runs with no side effects."
  (if (= 0 (length input-array))
      (make-array (list 0) :element-type (array-element-type input-array))
      (let ((arr (make-array (list (loop for x across input-array when (funcall condition x) sum 1))
                             :element-type (array-element-type input-array)
                             :initial-element (aref input-array 0))))
        (loop for i = -1 then i
              for x across input-array
              when (funcall condition x)
                do (setf (aref arr (incf i)) x))
        arr)))

(defun map-array-same-type (input-array mapper)
  "Maps INPUT-ARRAY through MAPPER for each element of INPUT-ARRAY.  Creates a new array with the same datatype."
  (if (= 0 (length input-array))
      (make-array (list 0) :element-type (array-element-type input-array))
      (let ((arr (make-array (array-dimensions input-array)
                             :element-type (array-element-type input-array)
                             :initial-element (aref input-array 0))))
        (loop for i = -1 then i
              for x across input-array
              do (setf (aref arr (incf i)) (funcall mapper x)))
        arr)))

(defun map-array-same-type-multiple (input-array mapper)
  )
