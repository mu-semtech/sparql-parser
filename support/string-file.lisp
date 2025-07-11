(in-package :support)

(defparameter *string-max-size* 256
  "Maximum size of a string before it gets converted.")

(defparameter *file-abbreviation-uri-prefix* "http://services.redpencil.io/sparql-parser/abbreviations/"
  "Prefix for the URIs which will contain the abbreviation of a string.")

(defun string-file-uri (sha &key lang datatype)
  "Constructs a string ffile based on the SHA, LANG and DATATYPE.

The result of supplying both LANG and DATATYPE is unspecified."
  (concatenate 'string
               *file-abbreviation-uri-prefix*
               sha
               (when (or lang datatype) "?")
               (when lang (format nil "lang=~A" (quri:url-encode lang)))
               (when datatype (format nil "datatype=~A" (quri:url-encode datatype)))))

(defun maybe-string-to-uri (string &rest args &key lang datatype)
  "Converts STRING to corresponding URI if it is too large.

Optional keyword argument LANG is used to identify the language (@nl)
Optional keyword argument DATATYPE is used to identify the datatype and should be the full URI.

The result of supplying both LANG and DATATYPE is unspecified.

Yields two values, the first being the resulting string or uri, the
second being truethy iff the string was converted into a URI."
  (declare (ignore lang datatype))
  (if (and *string-max-size*
           (> (length string) *string-max-size*))
      (values (apply #'string-file-uri
                     (make-string-file string)
                     args)
              t)
      (values string nil)))

(defun uri-string-p (uri)
  "Yields truethy iff URI represents a string"
  (let ((uri-prefix-length (length *file-abbreviation-uri-prefix*)))
    (and (> (length uri) uri-prefix-length)
         (string= uri *file-abbreviation-uri-prefix*
                  :end1 uri-prefix-length
                  :end2 uri-prefix-length))))

(defun maybe-uri-to-string (uri)
  "If URI is the representation of a string, then the string will be extracted.

First value is the string representation of the uri or string, second
value is truethy iff the URI was converted to a string.
The third value is the language tag if any, the fourth value the datatype if any."
  (if (uri-string-p uri)
      (let* ((question-position (position #\? uri :start (length *file-abbreviation-uri-prefix*)))
             (uuid (subseq uri (length *file-abbreviation-uri-prefix*) question-position))
             (lang-p (and question-position
                          (string= uri "?lang=" :start1 question-position :end1 (+ question-position (length "?lang=")))))
             (lang (and lang-p
                        (quri:url-decode (subseq uri (+ question-position (length "?lang="))))))
             (datatype-p (and question-position (not lang-p)))
             (datatype (and datatype-p
                            (quri:url-decode (subseq uri (+ question-position (length "?datatype=")))))))
        (values (read-string-file uuid)
                t
                lang
                datatype))
      (values uri nil nil nil)))

(defparameter *sha-file-directory*
  #+docker (make-pathname :directory '(:absolute "data" "strings"))
  #-docker (asdf:system-relative-pathname :sparql-parser (format nil "data/strings/"))
  "PATHNAME in which the string-files will be stored.")

(defun make-sha-file-path (sha)
  "Constructs the file path for a given SHA."
  (merge-pathnames *sha-file-directory* (make-pathname :name sha :version :newest)))

(defun make-string-file (string)
  "Writes contents of string to a file and yields its SHA.

Corresponds with READ-STRING-FILE."
  (let* ((octets (flexi-streams:string-to-octets string :external-format :utf-8))
         (sha (format nil "~{~16r~}" (sha1:sha1-digest octets)))
         (target (make-sha-file-path sha)))
    (if (probe-file target)
        sha
        ;; TODO: add retry mechanism in case of overlapping calls
        (progn
          (ensure-directories-exist target)
          (with-open-file (out target :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
            (write-sequence string out)
            sha)))))

(defun read-path-as-string (path)
  "Reads PATH (a pathname) as a string."
  (with-open-file (in path
                      :direction :input
                      :if-does-not-exist :error)
    (with-output-to-string (out)
      (loop with buffer = (make-array (expt 2 10) :element-type 'character)
            for characters-read = (read-sequence buffer in)
            while (< 0 characters-read)
            do (write-sequence buffer out :start 0 :end characters-read)))))

(defun read-string-file (sha)
  "Yields the contents of the file with the specified SHA as a string."
  (read-path-as-string (make-sha-file-path sha)))
