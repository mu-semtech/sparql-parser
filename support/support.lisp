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
       (break "Running ~A~&~A" ,name ',body)
       (let ((,result
               (multiple-value-list (progn ,@body))))
         (break "Results from ~A: ~A~& ran ~A" ',name ,result ',body)
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

(defun read-bnfsexp-from-file (path)
  "Reads a bnf sxp file file frrom PATH."
  (let ((*package* (find-package :sparql-bnf))
        (*readtable* (let ((rt (copy-readtable)))
                       (set-dispatch-macro-character
                        #\# #\t
                        (lambda (s c n)
                          (declare (ignore s c n))
                          t)
                        rt)
                       (setf (readtable-case rt) :preserve)
                       rt)))
    (with-open-file (input path :direction :input)
      (read input))))
