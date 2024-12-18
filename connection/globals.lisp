(in-package #:connection-globals)

;;;; Connection globals
;;;;
;;;; Globally available data received from the connection or set on the
;;;; connection.  Includes accessors for content like mu-call-id and
;;;; mu-auth-allowed-groups.

(defparameter *mu-auth-allowed-groups* nil
  "Allowed groups in jsown form.")
(defparameter *mu-auth-sudo* nil
  "T or NIL indicating if mu-auth-sudo was provided.")
(defparameter *mu-call-id* nil
  "Call id in string form.")
(defparameter *mu-session-id* nil
  "Session id in string form.")
(defparameter *mu-call-id-trail* nil
  "Call id trail in list form.")
(defparameter *mu-call-scope* nil
  "Call scope for the current request.  This is nil for microservices not
providing a call scope.")
(defparameter *source-ip* nil
  "The source-ip header of the request.  May be removed in the future.")

(defmacro with-call-context ((&key mu-call-id
                                mu-session-id
                                mu-auth-sudo
                                mu-auth-allowed-groups
                                mu-call-id-trail
                                (mu-call-scope 'acl:_)
                                source-ip)
                             &body body)
  `(let ((*mu-call-id* ,mu-call-id)
         (*mu-session-id* ,mu-session-id)
         (*mu-auth-sudo* ,mu-auth-sudo)
         (*mu-auth-allowed-groups* ,mu-auth-allowed-groups)
         (*mu-call-id-trail* , mu-call-id-trail)
         (*mu-call-scope* ,mu-call-scope)
         (*source-ip* ,source-ip))
     ,@body))

(defun mu-call-id ()
  "SETF-able mu-call-id for the current request."
  *mu-call-id*)

(defun (setf mu-call-id) (value)
  (setf *mu-call-id* value))

(defun mu-call-id-trail ()
  "SETF-able mu-call-id-trail for the current request."
  *mu-call-id-trail*)

(defun (setf mu-call-id-trail) (value)
  (setf *mu-call-id-trail* value))

(defun mu-session-id ()
  "SETF-able mu-session for the current request."
  *mu-session-id*)

(defun (setf mu-session-id) (value)
  (setf *mu-session-id* value))

(defun mu-auth-sudo ()
  "Truethy iff mu-auth-sudo was set on this request."
  *mu-auth-sudo*)

(defun mu-auth-allowed-groups ()
  "SETF-able mu-auth-allowed-groups for the current request."
  *mu-auth-allowed-groups*)

(defun (setf mu-auth-allowed-groups) (value)
  (setf *mu-auth-allowed-groups* value))

(defun mu-call-scope ()
  "SETF-able mu-call-scope for the current request."
  *mu-call-scope*)

(defun (setf mu-call-scope) (value)
  (setf *mu-call-scope* value))

(defun source-ip ()
  "SETF-able source ip address for the current request."
  *source-ip*)

(defun (setf source-ip) (value)
  (setf *source-ip* value))
