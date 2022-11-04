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
       (break "Runnig ~A~&~A" ,name ',body)
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
