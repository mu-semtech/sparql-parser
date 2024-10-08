;;;; Shameful monkeypatching of existing libraries

;;; Trivial backtrace arrays
;;;
;;; We use trivial-backtrace to show backtraces when an error occurs.
;;; Part of the backtrace is Woo reusing large arrays.
;;; trivial-backtrace overwrites the *print-length* to be nil so
;;; everything is printed which is not what we want.  There seems to be
;;; no way to override this behaviour at the time of writing <2024-10-08
;;; Tue> but it is trivial to monkeypatch.
(in-package :trivial-backtrace)

(defun print-backtrace-to-stream (stream)
  (let (#+:sbcl-debug-print-variable-alist
	(sb-debug:*debug-print-variable-alist*
	 (list* '(*print-level* . nil)
		'(*print-length* . 128) ; monkeypatch
		sb-debug:*debug-print-variable-alist*))
	#-:sbcl-debug-print-variable-alist
	(sb-debug:*debug-print-level* nil)
	#-:sbcl-debug-print-variable-alist
	(sb-debug:*debug-print-length* nil))
    (sb-debug:print-backtrace :count most-positive-fixnum :stream stream :emergency-best-effort t)))
