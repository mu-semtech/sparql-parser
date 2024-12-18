(in-package :support)

(defmacro -> (x &body forms)
  "Pipes the variable through the supplied forms as their first argument."
  (let ((construct x))
    (dolist (f forms)
      (let ((new-construct
              `(,(first f) ,construct ,@(rest f))))
        (setf construct new-construct)))
    construct))

(defun hex-char (number-string)
  "Yields the character for the given number."
  (code-char (parse-integer number-string :radix 16)))

(defmacro debug-break (name &body body)
  "Puts in a break statement at the start and end of the body."
  (let ((result (gensym)))
    `(progn
       ;; (break "Running ~A~&~A" ,name ',body)
       (let ((,result
               (multiple-value-list (progn ,@body))))
         ;; (break "Results from ~A: ~A~& ran ~A" ',name ,result ',body)
         (apply #'values ,result)))))

(defun embed-unicode-characters (string)
  "Embeds the unicode characters expressed as #xABCD by their unicode counterpart."
  (flet ((unicode-to-string (unicode-hex-string)
           (string
            (code-char 
             (parse-integer unicode-hex-string
                            :radix 16)))))
    (cl-ppcre:regex-replace-all
     "#x[0-9A-F]{1,5}"
     string
     (lambda (target-string start end match-start match-end reg-starts reg-ends) 
       (declare (ignore start end reg-starts reg-ends))
       (let ((start (+ 2 match-start)) ; skip #x
             (end match-end))
         (unicode-to-string (subseq target-string start end)))))))

(defmacro match-tree-search ((var) &body ranges)
  "Constructs a tree search for the given set of non-overlapping ranges, with optional breakpoints."
  (let* ((char-ranges
           (loop for (low . high) in ranges
                 for low-char = (if (characterp low) low (hex-char low))
                 for high-char = (if (characterp high) high (hex-char high))
                 collect (cons low-char high-char)))
         (sorted-ranges (sort (copy-seq char-ranges) #'char< :key #'car)))
    (labels ((split-ranges (ranges)
               (let ((split-point (floor (/ (length ranges) 2))))
                 (values (subseq ranges 0 split-point)
                         (subseq ranges split-point))))
             (emit (focussed-ranges known-lower-bounds known-upper-bounds)
               (if (<= (length focussed-ranges) 3)
                   ;; two options best fit in an or even though we could skip
                   ;; some of the comparison
                   `(or ,@(loop for (low . high) in focussed-ranges
                                collect `(char<= ,@(unless (find low known-lower-bounds :test #'char=) (list low))
                                                 ,var
                                                 ,@(unless (find high known-upper-bounds :test #'char=) (list high)))))
                   (multiple-value-bind (left right)
                       (split-ranges focussed-ranges)
                     (let ((bound (cdar (last left))))
                      `(if (char<= ,var ,bound)
                           ;; we are in left
                           ,(emit left known-lower-bounds (cons bound known-upper-bounds))
                           ;; we are in right
                           ,(emit right (cons bound known-lower-bounds) known-upper-bounds)))))))
      (emit sorted-ranges nil nil))))

(defun group-by (list cmp &key (key #'identity))
  "Groups elements in LIST by CMP returning a new nested list."
  (loop for discovered-groups = nil then discovered-groups
        for item in list
        for group = (find (funcall key item) discovered-groups :key (lambda (x) (funcall key (first x))) :test cmp)
        if group
          do (setf (cdr (last group)) (list item))
        else
          do (push (list item) discovered-groups)
        finally (return discovered-groups)))

(defun group-by-size-and-count (items &key (max-size 100) (max-count 20) (item-size #'length))
  "Groups items such that MAX-SIZE and MAX-COUNT is not exceeded."
  (let ((current-list nil)
        (current-size 0)
        (current-count 0)
        (previous-lists nil))
    (dolist (item items)
      (let ((current-item-size (funcall item-size item)))
        (incf current-count)
        (incf current-size current-item-size)
        (cond ((null current-list)
               (push item current-list))
              ((and
                (<= current-count max-count)
                (<= current-size max-size))
               (push item current-list))
              (t
               (push (reverse current-list) previous-lists)
               (setf current-size current-item-size
                     current-count 1
                     current-list (list item))))))
    (reverse (cons current-list previous-lists))))

(defun pick-lists (left right &key pick single double)
  "Picks elements from the list, executing selector.
Pick should yield one of:
- :left to consume the left element, :right to consume the right element, :both to consume both elements.

SINGLE should process one element on the list.  The value returned is consed.

DOUBLE is called when :both is returned.  It receives two
elements.  The value returned is consed."
  (loop
    while (or left right)
    append (cond ((null left)
                  (loop while right collect (funcall single (pop right))))
                 ((null right)
                  (loop while left collect (funcall single (pop left))))
                 (t (case (funcall pick (first left) (first right))
                      (:left (list (funcall single (pop left))))
                      (:right (list (funcall single (pop right))))
                      (:both (list (funcall double (pop left) (pop right))))
                      (otherwise (error "pick must yield one of :left, :right, :both")))))))

;;;; derived types
(defparameter *next* 0)

(defmacro with-derived-types ((&rest derived-type-specifications) &body body)
  (let (outer-forms var-constraint-combinations)
    (dolist (specifier derived-type-specifications)
      (destructuring-bind (var-name (function-name . args)) specifier
        (multiple-value-bind (outer-form type-specifier)
            (apply function-name args)
          (push outer-form outer-forms)
          (push `(,var-name ',type-specifier) var-constraint-combinations))))
    `(progn (eval-when (:compile-toplevel :load-toplevel :execute)
              ,@outer-forms)
            (let (,@var-constraint-combinations)
              ,@body))))

;; derived types constructions
(defun typed-hash-table (key-type value-type)
  (let ((typed-hash-table-function-sym (intern (format nil "TYPED-HASH-TABLE-TEST-~A" (incf *next*)))))
    (values `(defun ,typed-hash-table-function-sym (hash-table)
               (hash-table-has-types-p hash-table ',key-type ',value-type))
            `(and hash-table (satisfies ,typed-hash-table-function-sym)))))

(defun hash-table-has-types-p (hash-table key-type value-type)
  "Constructs expansion for checking HASH-TABLE with KEY-TYPE as keys and VALUE-TYPE as values."
  ;; (format t "~&Checking if ~A is of type ~A => ~A~%" hash-table key-type value-type)
  (loop for k being the hash-keys of hash-table
        for v being the hash-values of hash-table
        ;; do (format t "~&~A => ~A~%" k v)
        when (or (not (typep k key-type))
                 (not (typep v value-type)))
          do (return nil)
        finally (return t)))

(defun typed-list (content-type)
  "Constructs expansion for checking list of CONTENT-TYPE elements."
  (let ((typed-list-test-function-sym (intern (format nil "TYPED-LIST-TEST-~A" (incf *next*)))))
    ;; (format t "~&~A will check for type ~A~%" typed-list-test-function-sym content-type)
    (values `(defun ,typed-list-test-function-sym (list)
               ;; (format t "~&Checking ~A for items of type ~A~%" list ',content-type)
               (every (lambda (item) (let ((is-of-type (typep item ',content-type)))
                                       ;; (format t "~&~A is ~:[NOT ~;~]of type ~A~%" item is-of-type ',content-type)
                                       is-of-type))
                      list))
            `(or null
                 (and cons
                      (satisfies ,typed-list-test-function-sym))))))

(defun typed-plist (key-type value-type &key (expand-length 0))
  (let ((typed-plist-test-function-sym (intern (format nil "TYPED-PLIST-TEST-~A" (incf *next*)))))
    (values `(defun ,typed-plist-test-function-sym (plist)
               (loop for (k v) on plist by #'cddr
                     unless (and (typep k ',key-type)
                                 (typep v ',value-type))
                       do (return nil)
                     finally (return t)))
            (labels ((expand-recursive (n &optional (final-cons-constraint 'null))
                       (if (= n 0)
                           final-cons-constraint
                           `(cons ,key-type (cons ,value-type ,(expand-recursive (1- n) final-cons-constraint))))))
              `(and (or ,@(loop for current-expansion-length from 0 to expand-length
                                if (= current-expansion-length expand-length)
                                  collect (expand-recursive current-expansion-length `(satisfies ,typed-plist-test-function-sym))
                                else
                                  collect (expand-recursive current-expansion-length)))
                    (satisfies ,typed-plist-test-function-sym))))))

;;;; exponential backoff
(defmacro with-exponential-backoff-retry ((&rest args &key (max-time-spent 60) (max-retries 10) (initial-pause-interval 1) (pause-interval-multiplier 2) (total-time-spent 0) (log t) (log-condition t))
                                          &body body)
  (declare (ignore max-time-spent max-retries initial-pause-interval pause-interval-multiplier total-time-spent log log-condition))
  `(let (*exponential-backoff-failure*)
     (declare (special *exponential-backoff-failure* *total-time-spent*))
     (exponential-backoff-retry*
      (lambda () ,@body)
      ,@args)))

(defun report-exponential-backoff-failure (&optional argument)
  "Reports the current run within exponential backoff to have failed."
  (declare (special *exponential-backoff-failure*))
  (setf *exponential-backoff-failure* argument))

(defun exponential-backoff-retry* (functor &key (max-time-spent 60) (max-retries 10) (initial-pause-interval 1) (pause-interval-multiplier 2) (total-time-spent 0) (log t) (log-condition nil))
  "Calls functor with exponential backoff."
  (declare (special *exponential-backoff-failure*))
  (let ((*total-time-spent* total-time-spent))  ; this is actually of the previous loop, but 🤷
    (declare (special *total-time-spent*))
    (let ((start-time (get-internal-real-time))
           (response (multiple-value-list (funcall functor))))
      (if *exponential-backoff-failure*
          (let* ((time-spent (- (get-internal-real-time) start-time))
                 (next-max-retries (and (> max-retries 0) (1- max-retries)))
                 (time-left (- max-time-spent (/ time-spent internal-time-units-per-second) initial-pause-interval))
                 (next-max-time-spent (and (> time-left 0) time-left)))
            (when log
             (format t "~&Exponential backoff :: ~A retries left, ~A time left, ~A total time spent, waiting ~A to retry.~&~@[-> reported reason: ~A~]~&"
                     max-retries
                     time-left
                     (+ time-spent total-time-spent)
                     initial-pause-interval
                     (and log-condition *exponential-backoff-failure*)))
            (setf *exponential-backoff-failure* nil)
            ;; TODO: provide option to send error on failure
            (when (and next-max-time-spent next-max-retries)
              (sleep initial-pause-interval)
              (exponential-backoff-retry* functor
                                          :max-time-spent next-max-time-spent
                                          :total-time-spent (+ time-spent total-time-spent)
                                          :max-retries next-max-retries
                                          :initial-pause-interval (* pause-interval-multiplier initial-pause-interval)
                                          :pause-interval-multiplier pause-interval-multiplier)))
          (apply #'values response)))))

;;;; jsown supporting functions
(defun jsown-equal (a b &key same-structure-p)
  "Compares two jsown structures

SAME-STRUCTURE-P indicates the objects were constructed in the same way
and the keys are therefore in the same order."
  (if (null same-structure-p)
      (error "jsown-equality is only implemented for same structure-p at this point.")
      (equal a b)))

(defun jsown-dedup (jsown-list &key same-structure-p)
  "Deduplicates a jsown structure."
  (remove-duplicates jsown-list
                     :test (alexandria:rcurry #'jsown-equal :same-structure-p same-structure-p)))

;;;; tree search
(defun depth-first-search (&key start condition descend)
  (if (funcall condition start)
      start
      (loop for item in (funcall descend start)
            if (depth-first-search :start item
                                   :condition condition
                                   :descend descend)
              return it)))

;;;; smarter case
(defmacro case+ ((value &key (test #'eql)) &body clauses)
  "Like CL:CASE but allows specifying the test to use."
  (let ((test-sym (gensym "CURRIED-TEST")))
    `(let ((,test-sym (alexandria:curry ,test ,value)))
       (cond ,@(loop for (spec . body) in clauses
                     collect
                     (cond ((listp spec)
                            `((some ,test-sym (list ,@spec))
                              ,@body))
                           ((find spec '(t otherwise))
                            `(t ,@body))
                           (t `((funcall ,test-sym ,spec)
                                ,@body))))))))
