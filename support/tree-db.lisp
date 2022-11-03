(in-package :tree-db)

(defun create ()
  (make-hash-table :test 'common-lisp:equal))

(defun val (db key-list)
  (gethash key-list db))

(defun (setf val) (value db key-list)
  (setf (gethash key-list db) value))

(defun keys-at (db key-list)
  "Returns all keys in DB that have KEY-LIST as a subset."
  (loop for ks being the hash-keys of db
        if (search key-list ks :end2 (length key-list))
          collect ks))

(defun all-keys (db)
  "Returns all keys in the tree database."
  (keys-at db (list)))

(defun equal (a b)
  (every (lambda (key)
           (common-lisp:equal (val a key) (val b key)))
         (apply #'union (mapcar #'all-keys (list a b)))))

(defun copy (db)
  "Returns a copy of DB."
  (let ((new (create)))
    (loop for k in (all-keys db)
          for v = (val db k)
          do
             (setf (val new k) v))
    new))
