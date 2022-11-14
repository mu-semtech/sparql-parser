(in-package #:connection-globals)

;;;; Connection globals
;;;;
;;;; Globally available data received from the connection or set on the
;;;; connection.  Includes accessors for content like mu-call-id and
;;;; mu-auth-allowed-groups.

(defparameter *mu-auth-allowed-groups* nil
  "Allowed groups in jsown form.")
(defparameter *mu-call-id* nil
  "Call id in string form.")
(defparameter *mu-session-id* nil
  "Session id in string form.")

(defmacro with-call-context ((&key mu-call-id mu-session-id mu-auth-allowed-groups) &body body)
  `(let ((*mu-call-id* ,mu-call-id)
         (*mu-session-id* ,mu-session-id)
         (*mu-auth-allowed-groups* ,mu-auth-allowed-groups))
     ,@body))

(defun mu-call-id ()
  "SETF-able mu-call-id for the current request."
  *mu-call-id*)

(defun (setf mu-call-id) (value)
  (setf *mu-call-id* value))

(defun mu-session-id ()
  "SETF-able mu-session for the current request."
  *mu-session-id*)

(defun (setf mu-session-id) (value)
  (setf *mu-session-id* value))

(defun mu-auth-allowed-groups ()
  "SETF-able mu-auth-allowed-groups for the current request."
  *mu-auth-allowed-groups*)

(defun (setf mu-auth-allowed-groups) (value)
  (setf *mu-auth-allowed-groups* value))
