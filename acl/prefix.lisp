(in-package #:prefix)

;;;;;;;;;;;;
;;; Used in expansion for acl configuration

;;; definition
(defparameter *prefixes* nil
  "plist of prefixes with their expansion.")

(defun define-prefix (prefix expansion)
  "Defines a new prefix"
  (alexandria:appendf *prefixes* (list prefix expansion)))

(defmacro define-prefixes (&body body)
  "Defines a series of prefixes by reading the list as a plist.

The car is assumed to be a keyward and the cadr is assumed to be the expanded string."
  `(progn ,@(loop for (prefix expansion) on body
                  by #'cddr
                  collect `(define-prefix ,prefix ,expansion))))

(define-prefixes
  :skos "http://www.w3.org/2004/02/skos/core#"
  :schema "http://schema.org/"
  :rdf "http://www.w3.org/1999/02/22-rdf-syntax-ns#")


;;; expansion

(defparameter *uri-protocol-check-on-prefix-expansion* t
  "Check URI protocol during prefix expansion.")
(defparameter *uri-protocol-accept-list-for-prefix-expansion* '("http:" "https:" "mailto:" "ftp:" "ftps:" "share:")
  "This list of protocols is accepted during the prefix expansion.  This list is subject to change.")

(defun expand (uri)
  "Expands the prefix if it could be found."
  (or (loop for (l-prefix expansion)
              on *prefixes*
                by #'cddr
            for prefix = (concatenate 'string (string-downcase (symbol-name l-prefix)) ":")
            when (search prefix uri :end2 (min (length prefix) (length uri)))
              return (concatenate 'string
                                  expansion
                                  (subseq uri (length prefix))))
      (if *uri-protocol-check-on-prefix-expansion*
          (progn
            (loop for prefix in *uri-protocol-accept-list-for-prefix-expansion*
                  when (search prefix uri :end2 (min (length prefix) (length uri)))
                    return uri)
            (error 'simple-error
                   :format-control "~&URI ~A was supplied but we could not expand it.~%  Disable prefix checking by setting (setf prefix:*uri-protocol-check-on-prefix-expansion* nil) or by adding the prefix to prefix:*uri-protocol-accept-list-for-prefix-expansion*.~%"
                   :format-arguments (list uri)))
          uri)))
