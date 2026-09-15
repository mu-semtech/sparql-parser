(in-package :cl-user)

(defun sparql-parser-debugger-hook (condition hook)
  (declare (ignore hook))
  (if (and (find-package :swank)
           (funcall (find-symbol "DEFAULT-CONNECTION" :swank)))
      (funcall (find-symbol "SWANK-DEBUGGER-HOOK" :swank) condition nil)
      (progn
        (format *error-output* "~&Unhandled condition: ~A~%~A~%"
                condition
                (sb-debug:list-backtrace))
        (sb-thread:abort-thread :allow-exit t))))

(setf sb-ext:*invoke-debugger-hook* #'sparql-parser-debugger-hook)
(setf cl:*debugger-hook* #'sparql-parser-debugger-hook)
