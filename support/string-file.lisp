(in-package :support)

(defparameter *string-max-size* 4096
  "Maximum size of a string before it gets converted.")

(defparameter *file-abbreviation-uri-prefix* "http://services.redpencil.io/sparql-parser/abbreviations/"
  "Prefix for the URIs which will contain the abbreviation of a string.")

(defun maybe-string-to-uri (string)
  "Converts a string to a corresponding URI if it is too large.

Yields two values, the first being the resulting string or uri, the
second being truethy iff the string was converted into a URI."
  (if (and *string-max-size*
           (> (length string) *string-max-size*))
      (values (format nil "~A~A"
                      *file-abbreviation-uri-prefix*
                      (make-string-file string))
              t)
      (values string nil)))

(defun uri-string-p (uri)
  "Yields truethy iff URI represents a string"
  (and (> (length uri) (length *file-abbreviation-uri-prefix*))))

(defun maybe-uri-to-string (uri)
  "If URI is the representation of a string, then the string will be extracted.

First value is the string representation of the uri or string, second
value is truethy iff the URI was converted to a string."
  (if (uri-string-p uri)
      (values (read-string-file (subseq uri (length *file-abbreviation-uri-prefix*))) t)
      (values uri nil)))

(defun make-sha-file-path (sha)
  "Constructs the file path for a given SHA."
  #+docker (make-pathname :directory '(:absolute "data") :name sha)
  #-docker (asdf:system-relative-pathname :sparql-parser (format nil "data/~A" sha)))

(defun make-string-file (string)
  "Writes contents of string to a file and yields its SHA.

Corresponds with READ-STRING-FILE."
  (let* ((sha (format nil "~{~16r~}" (sha1:sha1-digest string)))
         (target (make-sha-file-path sha)))
    (if (probe-file target)
        sha
        ;; TODO: add retry mechanism in case of overlapping calls
        (with-open-file (out target :direction :output
                                    :if-exists :supersede
                                    :if-does-not-exist :create)
          (write-sequence string out)
          sha))))

(defun read-string-file (sha)
  "Yields the contents of the file with the specified SHA as a string."
  (with-open-file (in (make-sha-file-path sha)
                      :direction :input
                      :if-does-not-exist :error)
    (with-output-to-string (out)
      (loop with buffer = (make-array (expt 2 10) :element-type 'character)
            for characters-read = (read-sequence buffer in)
            while (< 0 characters-read)
            do (write-sequence buffer out :start 0 :end characters-read)))))
