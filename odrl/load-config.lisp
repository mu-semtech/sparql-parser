(in-package :cl-user)

(when odrl-config::*use-odrl-config-p*
  (format t "~& >> Loading configuration via ODRL")
  ;; TODO: this is dirty
  ;; Remove any configuration that was loaded by evaluating lisp config
  (setf acl::*access-specifications* nil)
  (setf acl::*graphs* nil)
  (setf acl::*rights* nil)
  ;; Load ODRL, if any
  (alexandria:if-let ((triples (odrl-config::load-policy-file)))
    (odrl-config::odrl-to-acl (odrl-config::make-rule-set triples))
    (format t "~&~%~%NO ODRL CONFIG MOUNTED; BOOTING WITH EMPTY CONFIGURATION~%~%")))
