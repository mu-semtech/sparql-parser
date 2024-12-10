(in-package :sparql-terminals)

;;;; Sparql terminals
;;;;
;;;; this file contains the terminals of the SPARQL language and a way
;;;; of parsing the terminals.
;;;;
;;;; this library was optimized to understand whether a Lisp based
;;;; variant would be sufficiently fast in the interpretation of the
;;;; queries or whether it would be too slow.  for this many parsers
;;;; have to approaches available: a regular expression and hand-rolled
;;;; interpretation.  further optimizations seem possible but the
;;;; approach here has shown sufficient promise to verify the desired
;;;; performance.
;;;;
;;;; the code in this library could be cleaned up in that it contains
;;;; duplicate code.  it could be feasible to implement this code with
;;;; more limited base constructs and to inline the code.  the approach
;;;; visible here is sufficient for what we intend to verify.

;; (declaim (optimize (speed 0) (safety 3) (debug 3)))
(declaim (optimize (speed 3) (safety 0) (debug 0)))

(declaim (ftype (function (simple-string fixnum) (or null fixnum)) scan-uri scan-pname-ns tree-scan-pname-ln tree-scan-blank-node-label scan-double-negative scan-double-positive scan-decimal-negative scan-decimal-positive scan-integer-positive scan-integer-negative scan-string-literal-long-1 scan-whitespace scan-var1 scan-var2 scan-nil scan-anon scan-string-literal1 scan-string-literal2))
(declaim (ftype (function (simple-string fixnum &optional fixnum) (or null fixnum)) scan-integer tree-scan-pname-ns scan-double scan-decimal))

(defun scan-uri (string start)
  "Scans a string to check whether it's a URI or not."
  (let ((position start)
        (string-length (length string)))
    (flet ((scan-content ()
             (loop for char = (elt string position)
                   for char-code = (the fixnum (char-code char))
                   until (or (char= char #\>) (= string-length position))
                   if (or (<= 0 char-code 20)
                          (find (the fixnum char-code) '(60 34 123 125 124 94 96 92) :test #'eql))
                     do (return-from scan-uri nil)
                   else
                     do (incf position)))
           (scan-start ()
             (if (char= (elt string position) #\<)
                 (incf position)
                 (return-from scan-uri nil)))
           (scan-end ()
             (if (char= (elt string position) #\>)
                 (incf position)
                 (return-from scan-uri nil))))
      (scan-start)
      (scan-content)
      (scan-end))
    position))

(defun scan-double (string start &optional (string-length (length string)))
  "Scans for DOUBLE.

This solution bails out quickly, assuming DOUBLE is often requested with
failure."
  ;; DOUBLE ::= [0-9]+ '.' [0-9]* EXPONENT | '.' ([0-9])+ EXPONENT | ([0-9])+ EXPONENT
  (let ((position start))
    (flet ((scan-numbers ()
             (/= (the fixnum position)
                 (loop while (and (< position string-length)
                                  (char<= #\0 (elt string position) #\9))
                       do
                          (incf (the fixnum position))
                       finally (return position))))
           (scan-dot ()
             (when (and (< position string-length)
                        (char= #\. (elt string position)))
               (incf position)))
           (scan-exponent ()
             (when (and (< position string-length)
                        (or (char= #\e (elt string position))
                            (char= #\E (elt string position))))
               (incf position)))
           (scan-plus-minus ()
             (when (and (< position string-length)
                        (or (char= #\- (elt string position))
                            (char= #\+ (elt string position))))
               (incf position))))
      (if (scan-numbers)
          (progn
            ;; we may now have a ., maybe some numbers, and we must have
            ;; an exponent
            (scan-dot)                  ; optional
            (scan-numbers)              ; optional
            (and
             (scan-exponent)
             (prog1 t (scan-plus-minus)) ; optional
             (scan-numbers)
             position))
          (progn
            ;; we now have the case '.' ([0-9])+ EXPONENT
            (and
             (scan-dot)
             (scan-numbers)
             (scan-exponent)
             (prog1 t (scan-plus-minus)); optional
             (scan-numbers)
             position))))))

(defun scan-double-positive (string start)
  "Scans for DOUBLE_POSITIVE."
  (let ((string-length (length string)))
    (when (and (< start string-length)
               (char= (elt string start) #\+))
      (scan-double string (1+ start) string-length))))

(defun scan-double-negative (string start)
  "Scans for DOUBLE_NEGATIVE."
  (let ((string-length (length string)))
    (when (and (< start string-length)
               (char= (elt string start) #\-))
      (scan-double string (1+ start) string-length))))

(defun scan-decimal (string start &optional (string-length (length string)))
  "Scans for DECIMAL."
  ;; DECIMAL ::= [0-9]* '.' [0-9]+
  (let ((position start))
    (flet ((scan-numbers ()
             (/= (the fixnum position)
                 (loop while (and (< position string-length)
                                  (char<= #\0 (elt string position) #\9))
                       do
                          (incf (the fixnum position))
                       finally (return position))))
           (scan-dot ()
             (when (and (< position string-length)
                        (char= #\. (elt string position)))
               (incf position))))
      (scan-numbers) ; optional
      (and (scan-dot)
           (scan-numbers)
           position))))

(defun scan-decimal-positive (string start)
  "Scans for DECIMAL_POSITIVE."
  (let ((string-length (length string)))
    (when (and (< start string-length)
               (char= (elt string start) #\+))
      (scan-decimal string (1+ start) string-length))))

(defun scan-decimal-negative (string start)
  "Scans for DECIMAL_NEGATIVE."
  (let ((string-length (length string)))
    (when (and (< start string-length)
               (char= (elt string start) #\-))
      (scan-decimal string (1+ start) string-length))))

(flet ((scan-string-literal-long (quote-char string start)
         ;; STRING_LITERAL_LONG1 ::= "'''" ( ( "'" | "''" )? ( [^'\] | ECHAR ) )* "'''"
         ;; ECHAR ::= '\' [tbnrf\"']

         ;; We can roughly interpret this as "read any ECHAR specially" plus
         ;; "count the quotes".
         (declare (type #-be-cautious base-char #+be-cautious character)
                  (type #-be-cautious base-string #+be-cautious string string)
                  (type fixnum start))
         (let ((position start)
               (string-length (length string)))
           (when (and (< (+ 2 position) string-length)
                      (char= quote-char (elt string position))
                      (char= quote-char (elt string (+ 1 position)))
                      (char= quote-char (elt string (+ 2 position))))
             (incf position 3)
             (loop
               while (< position string-length)
               for char = (elt string position)
               do
                  (cond ((char= char quote-char)
                         (if (and (< (+ 2 position) string-length)
                                  (char= quote-char (elt string (+ 1 position)))
                                  (char= quote-char (elt string (+ 2 position))))
                             (progn
                               (incf position 3)
                               (return-from scan-string-literal-long position))
                             (incf position 1)))
                        ((char= char #.(elt "\\" 0))
                         (if (and (< (1+ position) string-length)
                                  (let ((next-char (elt string (1+ position))))
                                    (or (char= next-char #\n)
                                        (char= next-char #\")
                                        (char= next-char #\')
                                        (char= next-char #\\)
                                        (char= next-char #\r)
                                        (char= next-char #\f)
                                        (char= next-char #\t)
                                        (char= next-char #\b))))
                             (incf position 2)
                             (return-from scan-string-literal-long nil)))
                        (t (incf position))))))))
  (defun scan-string-literal-long-1 (string start)
    "Scans a string to check if it's a literal wrapped in triple single quotes."
    (scan-string-literal-long #\' string start))
  (defun scan-string-literal-long-2 (string start)
    "Scans a string to check if it's a literal wrapped in triple double quotes."
    (scan-string-literal-long #\" string start)))

(flet ((scan-single-char-quoted-string (string start delimiter-char)
         ;; STRING_LITERAL1 ::= "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
         ;; in which #27 is #\'
         (let ((position start)
               (string-length (length string)))
           ;; check opening char
           (when (and (< position string-length)
                      (char= delimiter-char (elt string position)))
             (incf position)
             ;; check inner characters
             (loop
               while (< position string-length)
               for char = (elt string position)
               if (char= char delimiter-char)
                 do (return position)
               else
                 do (cond ((or (char= #.(hex-char "A") char)
                               (char= #.(hex-char "D") char))
                           (return nil))
                          ((char= #\\ char)
                           (if (< (1+ position) string-length)
                               (let ((next-char (elt string (1+ position))))
                                 (if (or (char= #\t next-char)
                                         (char= #\' next-char)
                                         (char= #\" next-char)
                                         (char= #\b next-char)
                                         (char= #\n next-char)
                                         (char= #\r next-char)
                                         (char= #\f next-char)
                                         (char= #\\ next-char))
                                     (incf position 2)
                                     (return nil)))
                               (return nil)))
                          (t (incf position))))
             ;; check closing character
             (when (and (< position string-length)
                        (char= delimiter-char (elt string position)))
               (1+ position))))))
  (defun scan-string-literal1 (string start)
    (scan-single-char-quoted-string string start #\'))
  (defun scan-string-literal2 (string start)
    (scan-single-char-quoted-string string start #\")))

(defun scan-integer (string start &optional (string-length (length string)))
  "Scans a string to check whether it's an INTEGER."
  (let ((position start))
    (flet ((scan-numbers ()
             (/= (the fixnum position)
                 (loop while (and (< position string-length)
                                  (char<= #\0 (elt string position) #\9))
                       do
                          (incf (the fixnum position))
                       finally (return position)))))
      (when (scan-numbers)
        position))))

(defun scan-integer-positive (string start)
  "Scans a string to check whether it's a positive integer"
  (let ((string-length (length string)))
    (when (and (< start string-length)
               (char= (elt string start) #\+))
      (scan-integer string (1+ start) string-length))))

(defun scan-integer-negative (string start)
  "Scans a string to check whether it's a positive integer"
  (let ((string-length (length string)))
    (when (and (< start string-length)
               (char= (elt string start) #\-))
      (scan-integer string (1+ start) string-length))))

(defun scan-pname-ns (string start)
  "Scans a string to check whether it's a PN_PREFIX."
  ;; PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
  ;; PNAME_NS
  (let ((position start)
        (string-length (length string)))
    (flet ((scan-pn-prefix ()
             ;; 1. scan PN_CHARS_BASE
             ;; 2. scan PN_CHARS + .
             ;; 3. verify last character is not a .
             ;; Note: we can ignore scanning too far and missing the : because : is not part of PN_CHARS
             (flet ((scan-one-pn-chars-base ()
                      ;; "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF]"
                      (let* ((char (elt string position))
                             (accepted
                               (if (char<= char #\z) ; highest numbered regular letter is common case
                                   (or (char<= #\a char #\z)
                                       (char<= #\A char #\Z))
                                   (or (and (char<= #.(hex-char "C0") char #.(hex-char "F6"))
                                            (char/= #.(hex-char "D7") char))
                                       (char<= #.(hex-char "F8") char #.(hex-char "2FF"))
                                       (char<= #.(hex-char "370") char #.(hex-char "37D"))
                                       (char<= #.(hex-char "37F") char #.(hex-char "1FFF"))
                                       (char<= #.(hex-char "200C") char #.(hex-char "200D"))
                                       (char<= #.(hex-char "2070") char #.(hex-char "218F"))
                                       (char<= #.(hex-char "2C00") char #.(hex-char "2FEF"))
                                       (char<= #.(hex-char "3001") char #.(hex-char "D7FF"))
                                       (char<= #.(hex-char "F900") char #.(hex-char "FDCF"))
                                       (char<= #.(hex-char "FDF0") char #.(hex-char "FFFD"))
                                       (char<= #.(hex-char "10000") char #.(hex-char "EFFFF"))))))
                        (when accepted
                          (incf position))))
                    (scan-pn-chars-and-dot ()
                      ;; "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-]"
                      (flet ((accept-current-p ()
                               (let* ((char (elt string position)))
                                 (if (char<= char #\z) ; highest numbered regular letter is common case
                                     (or (char<= #\A char #\Z)
                                         (char<= #\a char #\z)
                                         (char= char #\_)
                                         (char= char #\-)
                                         (char<= #\0 char #\9))
                                     (or (char= char #.(hex-char "B7"))
                                         (and (char<= #.(hex-char "C0") char #.(hex-char "F6"))
                                              (not (char= char #.(hex-char "D7"))))
                                         (char<= #.(hex-char "00D8") char #.(hex-char "00F6")) 
                                         (char<= #.(hex-char "00F8") char #.(hex-char "02FF")) 
                                         (char<= #.(hex-char "0370") char #.(hex-char "037D")) 
                                         (char<= #.(hex-char "037F") char #.(hex-char "1FFF")) 
                                         (char<= #.(hex-char "200C") char #.(hex-char "200D")) 
                                         (char<= #.(hex-char "2070") char #.(hex-char "218F")) 
                                         (char<= #.(hex-char "2C00") char #.(hex-char "2FEF")) 
                                         (char<= #.(hex-char "3001") char #.(hex-char "D7FF")) 
                                         (char<= #.(hex-char "F900") char #.(hex-char "FDCF")) 
                                         (char<= #.(hex-char "FDF0") char #.(hex-char "FFFD")) 
                                         (char<= #.(hex-char "10000") char #.(hex-char "EFFFF")) 
                                         (char<= #.(hex-char "0300") char #.(hex-char "036F")) 
                                         (char<= #.(hex-char "203F") char #.(hex-char "2040")))))))
                        (loop while (and (accept-current-p)
                                         (< (1+ position) string-length))
                              do (incf position))
                        t))
                    (reverse-until-last-scanned-is-not-dot ()
                      ;; we know at least one PN_CHARS_BASE was read so we can jump back
                      (loop for char = (elt string (1- position))
                            while (char= char #\.)
                            do (decf position))
                      position))
               (and
                (scan-one-pn-chars-base)
                (scan-pn-chars-and-dot)
                (reverse-until-last-scanned-is-not-dot))))
           (scan-colon ()
             (when (and (< position string-length)
                        (char= (elt string position) #\:))
               (incf position))))
      
      (unless (scan-pn-prefix)
        (setf position start))
      (scan-colon))))

(flet ((scan-varname (string position string-length)
         ;; VARNAME ::= ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040] )*
         ;; PN_CHARS_U ::= PN_CHARS_BASE | '_'
         ;; PN_CHAR_BASE ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]

         ;; PN_CHAR_BASE
         ;; (#\A . #\Z)
         ;; (#\a . #\z)
         ;; ("00C0" . "00D6")
         ;; ("00D8" . "00F6")
         ;; ("00F8" . "02FF")
         ;; ("0370" . "037D")
         ;; ("037F" . "1FFF")
         ;; ("200C" . "200D")
         ;; ("2070" . "218F")
         ;; ("2C00" . "2FEF")
         ;; ("3001" . "D7FF")
         ;; ("F900" . "FDCF")
         ;; ("FDF0" . "FFFD")
         ;; ("10000" . "EFFFF")
         (flet ((pn-chars-u-or-number ()
                  (let ((char (elt string position))) ; validity happened in scan-var*
                     (when (if (char<= char #\z)
                              (support:match-tree-search (char)
                                (#\A . #\Z)
                                (#\a . #\z)
                                (#\_ . #\_)
                                (#\0 . #\9))
                              (support:match-tree-search (char)
                                ("00C0" . "00D6")
                                ("00D8" . "00F6")
                                ("00F8" . "02FF")
                                ("0370" . "037D")
                                ("037F" . "1FFF")
                                ("200C" . "200D")
                                ("2070" . "218F")
                                ("2C00" . "2FEF")
                                ("3001" . "D7FF")
                                ("F900" . "FDCF")
                                ("FDF0" . "FFFD")
                                ("10000" . "EFFFF")))
                      (incf position))))
                (some-pn-chars-u-and-extra ()
                  (flet ((accept ()
                           (let ((char (elt string position))) ; validity happened in scan-var*
                             (if (char<= char #\z)
                                 (support:match-tree-search (char)
                                   (#\A . #\Z)
                                   (#\a . #\z)
                                   (#\_ . #\_)
                                   (#\0 . #\9))
                                 (support:match-tree-search (char)
                                   ("00C0" . "00D6")
                                   ("00D8" . "00F6")
                                   ("00F8" . "02FF")
                                   ("0370" . "037D")
                                   ("037F" . "1FFF")
                                   ("200C" . "200D")
                                   ("2070" . "218F")
                                   ("2C00" . "2FEF")
                                   ("3001" . "D7FF")
                                   ("F900" . "FDCF")
                                   ("FDF0" . "FFFD")
                                   ("10000" . "EFFFF")
                                   ;; up is ranges of PN_CHARS_U, below are extra ranges
                                   ("B7" . "B7")
                                   ("0300" . "036F")
                                   ("203F" . "2040"))))))
                    (loop while (and (< position string-length)
                                     (accept))
                          do (incf position)))))
           (when (pn-chars-u-or-number)
             (some-pn-chars-u-and-extra)
             position))))
  (defun scan-var1 (string start)
    (let ((string-length (length string)))
      (when (and (< (+ start 2) string-length)
                 (char= (elt string start) #\?))
        (scan-varname string (1+ start) string-length))))
  (defun scan-var2 (string start)
    (let ((string-length (length string)))
      (when (and (< (+ start 2) string-length)
                 (char= (elt string start) #\$))
        (scan-varname string (1+ start) string-length)))))

(defun tree-scan-pname-ns (string start &optional (string-length (length string)))
  "Scans a string to check whether it's a PN_PREFIX."
  ;; PN_PREFIX ::= PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
  ;; PNAME_NS
  (let ((position start))
    (flet ((scan-pn-prefix ()
             ;; 1. scan PN_CHARS_BASE
             ;; 2. scan PN_CHARS + .
             ;; 3. verify last character is not a .
             ;; Note: we can ignore scanning too far and missing the : because : is not part of PN_CHARS
             (flet ((scan-one-pn-chars-base ()
                      ;; "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF]"
                      (let* ((char (elt string position))
                             (accepted
                               (if (char<= #\z)
                                   (support:match-tree-search (char)
                                     (#\A . #\Z) (#\a . #\z))
                                   (support:match-tree-search (char)
                                     ("C0" . "00D6")
                                     ("00D8" . "00F6")
                                     ("00F8" . "02FF")
                                     ("0370" . "037D")
                                     ("037F" . "1FFF")
                                     ("200C" . "200D")
                                     ("2070" . "218F")
                                     ("2C00" . "2FEF")
                                     ("3001" . "D7FF")
                                     ("F900" . "FDCF")
                                     ("FDF0" . "FFFD")
                                     ("10000" . "EFFFF")))))
                        (when accepted
                          (incf position))))
                    (scan-pn-chars-and-dot ()
                      ;; "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040.-]"
                      (flet ((accept-current-p ()
                               (let* ((char (elt string position)))
                                 (if (char<= #\z)
                                     (support:match-tree-search (char)
                                       (#\A . #\Z) (#\_ . #\_) (#\- . #\-) (#\0 . #\9) (#\a . #\z) (#\. . #\.))
                                     (support:match-tree-search (char)
                                       ("00C0" . "00D6")
                                       ("00D8" . "00F6")
                                       ("00F8" . "02FF")
                                       ("0370" . "037D")
                                       ("037F" . "1FFF")
                                       ("200C" . "200D")
                                       ("2070" . "218F")
                                       ("2C00" . "2FEF")
                                       ("3001" . "D7FF")
                                       ("F900" . "FDCF")
                                       ("FDF0" . "FFFD")
                                       ("10000" . "EFFFF")
                                       ("00B7" . "00B7")
                                       ("0300" . "036F")
                                       ("203F" . "2040"))))))
                        (loop while (and (accept-current-p)
                                         (< (1+ position) string-length))
                              do (incf position))
                        t))
                    (reverse-until-last-scanned-is-not-dot ()
                      ;; we know at least one PN_CHARS_BASE was read so we can jump back
                      (loop for char = (elt string (1- position))
                            while (char= char #\.)
                            do (decf position))
                      position))
               (and
                (scan-one-pn-chars-base)
                (scan-pn-chars-and-dot)
                (reverse-until-last-scanned-is-not-dot))))
           (scan-colon ()
             (when (and (< position string-length)
                        (char= (elt string position) #\:))
               (incf position))))
      (unless (scan-pn-prefix)
        (setf position start))
      (scan-colon))))

(defun tree-scan-pname-ln (string start)
  (let* ((string-length (length string))
         (position (tree-scan-pname-ns string start string-length)))
    (declare (type fixnum string-length)
             (type (or fixnum null) position))
    (when position
      ;; (PN_CHARS_U | ':' | [0-9] | PLX ) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX) )?
      (labels ((is-hex (char)
                 (support:match-tree-search (char)
                   (#\0 . #\9)
                   (#\a . #\f)
                   (#\A . #\F)))
               (scan-plx (start-char)
                 (cond ((and (char= #\% start-char)
                             (< (+ 2 position) string-length))
                        (when (and (is-hex (elt string (+ 1 position)))
                                   (is-hex (elt string (+ 2 position))))
                          (incf position 3)))
                       ((and (char= #\\ start-char)
                             (< (1+ position) string-length))
                        (let ((char (elt string (1+ position))))
                          (when (support:match-tree-search (char)
                                  ;; combined list based on allowed characters
                                  (#\! . #\!)
                                  (#\# . #\/)
                                  (#\; . #\;)
                                  (#\= . #\=)
                                  (#\? . #\?)
                                  (#\_ . #\_)
                                  (#\~ . #\~))
                            (incf position 2))))
                       (t nil)))
               (scan-first-character ()
                 ;; (PN_CHARS_U | ':' | [0-9] | PLX )
                 (let ((char (elt string position))) ; validity happened in scan-var*
                   (if (char<= char #\z)
                       (if (support:match-tree-search (char)
                             (#\A . #\Z)
                             (#\a . #\z)
                             (#\_ . #\_)
                             (#\0 . #\9)
                             (#\: . #\:))
                           (incf position)
                           (scan-plx char))
                       (when (support:match-tree-search (char)
                               ("00C0" . "00D6")
                               ("00D8" . "00F6")
                               ("00F8" . "02FF")
                               ("0370" . "037D")
                               ("037F" . "1FFF")
                               ("200C" . "200D")
                               ("2070" . "218F")
                               ("2C00" . "2FEF")
                               ("3001" . "D7FF")
                               ("F900" . "FDCF")
                               ("FDF0" . "FFFD")
                               ("10000" . "EFFFF"))
                         (incf position)))))
               (scan-many-middle-characters ()
                 ;; ((PN_CHARS | '.' | ':' | PLX)*
                 (flet ((valid ()
                          (let ((char (elt string position))) ; validity happened in scan-var*
                            (if (char<= char #\z)
                                (if (support:match-tree-search (char)
                                      (#\A . #\Z)
                                      (#\a . #\z)
                                      (#\_ . #\_)
                                      (#\- . #\-)
                                      (#\0 . #\9)
                                      (#\: . #\:)
                                      (#\. . #\.))
                                    (incf position)
                                    (scan-plx char))
                                (when (support:match-tree-search (char)
                                        ("00C0" . "00D6")
                                        ("00D8" . "00F6")
                                        ("00F8" . "02FF")
                                        ("0370" . "037D")
                                        ("037F" . "1FFF")
                                        ("200C" . "200D")
                                        ("2070" . "218F")
                                        ("2C00" . "2FEF")
                                        ("3001" . "D7FF")
                                        ("F900" . "FDCF")
                                        ("FDF0" . "FFFD")
                                        ("10000" . "EFFFF")
                                        ;; above is extended range for PN_CHARS_BASE, below is the extra for PN_CHARS
                                        ("00B7" . "00B7")
                                        ("0300" . "036F")
                                        ("203F" . "2040"))
                                  (incf position))))))
                   (loop while (and (< position string-length)
                                    (valid)))
                   position)) ; we read one too far
               (reverse-until-last-scanned-is-not-dot ()
                 ;; we know at least one non-dot was read so we can jump back
                 (loop for char = (elt string (1- position))
                       while (char= char #\.)
                       do (decf position))
                 position))
        (and (scan-first-character)
             (scan-many-middle-characters)
             (reverse-until-last-scanned-is-not-dot))))))

(defun tree-scan-blank-node-label (string start)
  "Scans a string to theck whether it's a BLANK_NODE_LABEL."
  ;; BLANK_NODE_LABEL ::= _:' ( PN_CHARS_U | [0-9] ) ((PN_CHARS|'.')* PN_CHARS )?
  ;; PN_CHARS := PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
  ;; PN_CHARS_BASE ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
  ;; PN_CHARS_U ::= PN_CHARS_BASE | '_'
  (let ((position start)
        (string-length (length string))
        (lowest-valid-position (+ 2 start)))
    (flet ((scan-underscore-colon ()
             (when (and (< lowest-valid-position string-length) ; we need at least one character after _:
                        (char= (elt string position) #\_)
                        (char= (elt string (1+ position)) #\:))
               (incf position 2)))
           (scan-chars-u-or-number ()
             ;; [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
             ;; '_'
             ;; [0-9]
             (when (let ((char (elt string position))) ; length is checked higher up
                     (if (char<= #\z)
                         (support:match-tree-search (char)
                           (#\A . #\Z)
                           (#\a . #\z)
                           (#\0 . #\9)
                           (#\_ . #\_))
                         (support:match-tree-search (char)
                           ("00C0" . "00D6")
                           ("00D8" . "00F6")
                           ("00F8" . "02FF")
                           ("0370" . "037D")
                           ("037F" . "1FFF")
                           ("200C" . "200D")
                           ("2070" . "218F")
                           ("2C00" . "2FEF")
                           ("3001" . "D7FF")
                           ("F900" . "FDCF")
                           ("FDF0" . "FFFD")
                           ("10000" . "EFFFF"))))
               (incf position)))
           (scan-while-pn-chars-and-dot ()
             ;; "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-]"
             (flet ((accept-current-p ()
                      (let* ((char (elt string position)))
                        (if (char<= #\z)
                            (support:match-tree-search (char)
                              (#\A . #\Z) (#\_ . #\_) (#\- . #\-) (#\0 . #\9) (#\a . #\z) (#\. . #\.))
                            (support:match-tree-search (char)
                              ("00C0" . "00D6")
                              ("00D8" . "00F6")
                              ("00F8" . "02FF")
                              ("0370" . "037D")
                              ("037F" . "1FFF")
                              ("200C" . "200D")
                              ("2070" . "218F")
                              ("2C00" . "2FEF")
                              ("3001" . "D7FF")
                              ("F900" . "FDCF")
                              ("FDF0" . "FFFD")
                              ("10000" . "EFFFF")
                              ("00B7" . "00B7")
                              ("0300" . "036F")
                              ("203F" . "2040"))))))
               (loop while (and (accept-current-p)
                                (< (1+ position) string-length))
                     do (incf position))
               t)))
      ;; The EBNF shows that we should first scan far _:, after that
      ;; there should be one ( PN_CHARS_U | [0-9] ) followed by many
      ;; (PN_CHARS | .) and then move backwards if we'd have scanned a
      ;; '.'.  There may be cases where the . is the next token.
      (when (and (scan-underscore-colon)
                 (scan-chars-u-or-number))
        ;; now we can scan as many chars-and-dot, and then move
        ;; backwards for each dot (which we *know* we will encounter
        ;; because the first after _: cannot be a dot.
        (scan-while-pn-chars-and-dot)
        (loop for char = (elt string (1- position))
              while (char= char #\.)
              do (decf position))
        position))))

(flet ((scan-wrapped-whitespace (string start left-char right-char)
         (let ((string-length (length string))
               (position start))
           (when (and (< position (length string))
                      (char= (elt string position) left-char))
             (incf position)
             (loop while (and (< position string-length)
                              (let ((char (elt string position)))
                                (or (char= char #.(support:hex-char "20"))
                                    (char= char #.(support:hex-char "D"))
                                    (char= char #.(support:hex-char "A"))
                                    (char= char #.(support:hex-char "9")))))
                   do (incf position))
             (when (char= (elt string position) right-char)
               (1+ position))))))
  (defun scan-nil (string start)
    (scan-wrapped-whitespace string start #\( #\)))
  (defun scan-anon (string start)
    (scan-wrapped-whitespace string start #\[ #\])))

(defun scan-whitespace (string start)
  (let ((string-length (length string))
        (position start))
    (if (< start (length string))
        (loop
          while (< position string-length)
          for char = (elt string position)
          do
             (cond ((or (char= char #.(support:hex-char "20"))
                        (char= char #.(support:hex-char "D"))
                        (char= char #.(support:hex-char "A"))
                        (char= char #.(support:hex-char "9")))
                    (incf position))
                   ((char= char #\#)
                    ;; read until newline

                    ;; We can scan for only newline as both linux as
                    ;; windows have that on their newline syntax.  This
                    ;; is the last with CRLF (windows) and the only one
                    ;; for Linux (LF) and the character is most
                    ;; commonly written as #\Newline.
                    (incf position)
                    (loop
                      until (or (= position string-length)
                                (char= (elt string position) #\Newline))
                      do
                         (incf position)))
                   (t (return position))))
        start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SPARQL terminal syntax
;;;;
;;;; Scanners for SPARQL's terminal syntax

(defparameter *raw-syntax-strings-full*
  `((ebnf::|PN_LOCAL_ESC| . "\\\\[_~.\\-!$&'()*+,;=/?#@%]")
    (ebnf::|HEX| . "[0-9A-Fa-f]")
    (ebnf::|PERCENT| . "%[0-9A-Fa-f][0-9A-Fa-f]")
    (ebnf::|PLX| . "(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%])")
    (ebnf::|PN_LOCAL| . "([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_]|:|[0-9]|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%]))(([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-]|\\.|:|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%]))*([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-]|:|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%])))?"  ) ; :PN_LOCAL . (PN_CHARS_U | ':' | [0-9] | PLX ) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX) )?
    (ebnf::|PN_PREFIX| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF](([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-.])*[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-])?") ; :PN_PREFIX . PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
    (ebnf::|PN_CHARS| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-]") ; :PN_CHARS . PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
    (ebnf::|VARNAME| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9][A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040]*") ; :VARNAME . ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040] )*
    (ebnf::|PN_CHARS_U| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_]") ; :PN_CHARS_U . PN_CHARS_BASE | '_'
    (ebnf::|PN_CHARS_BASE| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF]") ; :PN_CHARS_BASE . [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
    (ebnf::|ANON| . "\\[[#x20#x9#xD#xA]*\\]") ; :ANON . '[' WS* ']'
    (ebnf::|WS| . "[#x20#x9#xD#xA]") ; :WS . #x20 | #x9 | #xD | #xA
    (ebnf::|SPARQLNIL| . "\\([#x20#x9#xD#xA]*\\)") ; :NIL . '(' WS* ')'
    (ebnf::|ECHAR| . "\\\\[tbnrf\\\\\"']") ; :ECHAR . '\' [tbnrf\"']
    (ebnf::|STRING_LITERAL_LONG2| . "\"\"\"((\"|\"\")?([^\"\\]|\\\\[tbnrf\\\\\"']))*\"\"\"") ; :STRING_LITERAL_LONG2 . '"""' ( ( '"' | '""' )? ( [^"\] | ECHAR ) )* '"""'
    (ebnf::|STRING_LITERAL_LONG1| . "'''(('|'')?([^'\\\\]|\\\\[tbnrf\\\\\"']))*'''") ; :STRING_LITERAL_LONG1 . "'''" ( ( "'" | "''" )? ( [^'\] | ECHAR ) )* "'''"
    (ebnf::|STRING_LITERAL2| . "\"(([^#x22#x5C#xA#xD])|\\\\[tbnrf\\\\\"'])*\"") ; :STRING_LITERAL2 . '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
    (ebnf::|STRING_LITERAL1| . "'(([^#x27#x5C#xA#xD])|\\\\[tbnrf\\\\\"'])*'") ; :STRING_LITERAL1 . "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
    (ebnf::|EXPONENT| . "[eE][+-]?[0-9]+") ; :EXPONENT . [eE] [+-]? [0-9]+
    (ebnf::|DOUBLE_NEGATIVE| . "-([0-9]+\\.[0-9]*[eE][+-]?[0-9]+|\\.([0-9])+[eE][+-]?[0-9]+|([0-9])+[eE][+-]?[0-9]+)" ) ; :DOUBLE_NEGATIVE . '-' DOUBLE
    (ebnf::|DECIMAL_NEGATIVE| . "-[0-9]*\\.[0-9]+") ; :DECIMAL_NEGATIVE . '-' DECIMAL
    (ebnf::|INTEGER_NEGATIVE| . "-INTEGER") ; :INTEGER_NEGATIVE . '-' INTEGER
    (ebnf::|DOUBLE_POSITIVE| . "\\+([0-9]+\\.[0-9]*[eE][+-]?[0-9]+|\\.([0-9])+[eE][+-]?[0-9]+|([0-9])+[eE][+-]?[0-9]+)") ; :DOUBLE_POSITIVE . '+' DOUBLE
    (ebnf::|DECIMAL_POSITIVE| . "\\+[0-9]*\\.[0-9]+") ; :DECIMAL_POSITIVE . '+' DECIMAL
    (ebnf::|INTEGER_POSITIVE| . "\\+INTEGER") ; :INTEGER_POSITIVE . '+' INTEGER
    (ebnf::|DOUBLE| . "[0-9]+\\.[0-9]*[eE][+-]?[0-9]+|\\.([0-9])+[eE][+-]?[0-9]+|([0-9])+[eE][+-]?[0-9]+") ; :DOUBLE . [0-9]+ '.' [0-9]* EXPONENT | '.' ([0-9])+ EXPONENT | ([0-9])+ EXPONENT
    (ebnf::|DECIMAL| . "[0-9]*\\.[0-9]+") ; :DECIMAL . [0-9]* '.' [0-9]+
    (ebnf::|INTEGER| . "[0-9]+") ; :INTEGER . [0-9]+
    (ebnf::|LANGTAG| . "@[a-zA-Z]+(-[a-zA-Z0-9]+)*") ; :LANGTAG . '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
    (ebnf::|VAR2| . "$[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9][A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040]*") ; :VAR2 . '$' VARNAME
    (ebnf::|VAR1| . "\\?[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9][A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040]*") ; :VAR1 . '?' VARNAME
    (ebnf::|BLANK_NODE_LABEL| . "_:[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9]([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040\\.-]*[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-])?" ) ; :BLANK_NODE_LABEL . '_:' ( PN_CHARS_U | [0-9] ) ((PN_CHARS|'.')* PN_CHARS)?
    (ebnf::|PNAME_LN| . "([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF](([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-.])*[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-])?)?:(([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_]|:|[0-9]|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%]))(([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-]|\\.|:|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%]))*([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-]|:|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%])))?)") ; :PNAME_LN . PNAME_NS PN_LOCAL
    (ebnf::|PNAME_NS| . "([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF](([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-.])*[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-])?)?:") ; :PNAME_NS . PN_PREFIX? ':'
    (ebnf::|IRIREF| . "<([^<>\"{}|^`#x00-#x20])*>") ; :IRIREF . '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
    )
  "The syntax strings in their raw embedded regex format.")

(defparameter *raw-syntax-strings-simple*
  `((ebnf::|PN_LOCAL_ESC| . "\\\\[_~.\\-!$&'()*+,;=/?#@%]")
    (ebnf::|HEX| . "[0-9A-Fa-f]")
    (ebnf::|PERCENT| . "%[0-9A-Fa-f][0-9A-Fa-f]")
    (ebnf::|PLX| . "(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%])")
    (ebnf::|PN_LOCAL| . "([A-Za-z#x00C0-#x00D6#x00D8-#x00F6_]|:|[0-9]|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%]))(([A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7-]|\\.|:|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%]))*([A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7-]|:|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%])))?"  ) ; :PN_LOCAL . (PN_CHARS_U | ':' | [0-9] | PLX ) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX) )?
    (ebnf::|PN_PREFIX| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6](([A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7.-])*[A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7-])?") ; :PN_PREFIX . PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
    (ebnf::|PN_CHARS| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7-]") ; :PN_CHARS . PN_CHARS_U | '-' | [0-9] | #x00B7 | [] | []
    (ebnf::|VARNAME| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9][A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7]*") ; :VARNAME . ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #x00B7 | [] | [] )*
    (ebnf::|PN_CHARS_U| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6_]") ; :PN_CHARS_U . PN_CHARS_BASE | '_'
    (ebnf::|PN_CHARS_BASE| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6]") ; :PN_CHARS_BASE . [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
    (ebnf::|ANON| . "\\[[#x20#x9#xD#xA]*\\]") ; :ANON . '[' WS* ']'
    (ebnf::|WS| . "[#x20#x9#xD#xA]") ; :WS . #x20 | #x9 | #xD | #xA
    (ebnf::|SPARQLNIL| . "\\([#x20#x9#xD#xA]*\\)") ; :NIL . '(' WS* ')'
    (ebnf::|ECHAR| . "\\\\[tbnrf\\\\\"']") ; :ECHAR . '\' [tbnrf\"']
    (ebnf::|STRING_LITERAL_LONG2| . "\"\"\"((\"|\"\")?([^\"\\]|\\\\[tbnrf\\\\\"']))*\"\"\"") ; :STRING_LITERAL_LONG2 . '"""' ( ( '"' | '""' )? ( [^"\] | ECHAR ) )* '"""'
    (ebnf::|STRING_LITERAL_LONG1| . "'''(('|'')?([^'\\\\]|\\\\[tbnrf\\\\\"']))*'''") ; :STRING_LITERAL_LONG1 . "'''" ( ( "'" | "''" )? ( [^'\] | ECHAR ) )* "'''"
    (ebnf::|STRING_LITERAL2| . "\"(([^#x22#x5C#xA#xD])|\\\\[tbnrf\\\\\"'])*\"") ; :STRING_LITERAL2 . '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
    (ebnf::|STRING_LITERAL1| . "'(([^#x27#x5C#xA#xD])|\\\\[tbnrf\\\\\"'])*'") ; :STRING_LITERAL1 . "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
    (ebnf::|EXPONENT| . "[eE][+-]?[0-9]+") ; :EXPONENT . [eE] [+-]? [0-9]+
    (ebnf::|DOUBLE_NEGATIVE| . "-([0-9]+\\.[0-9]*[eE][+-]?[0-9]+|\\.([0-9])+[eE][+-]?[0-9]+|([0-9])+[eE][+-]?[0-9]+)" ) ; :DOUBLE_NEGATIVE . '-' DOUBLE
    (ebnf::|DECIMAL_NEGATIVE| . "-[0-9]*\\.[0-9]+") ; :DECIMAL_NEGATIVE . '-' DECIMAL
    (ebnf::|INTEGER_NEGATIVE| . "-INTEGER") ; :INTEGER_NEGATIVE . '-' INTEGER
    (ebnf::|DOUBLE_POSITIVE| . "\\+([0-9]+\\.[0-9]*[eE][+-]?[0-9]+|\\.([0-9])+[eE][+-]?[0-9]+|([0-9])+[eE][+-]?[0-9]+)") ; :DOUBLE_POSITIVE . '+' DOUBLE
    (ebnf::|DECIMAL_POSITIVE| . "\\+[0-9]*\\.[0-9]+") ; :DECIMAL_POSITIVE . '+' DECIMAL
    (ebnf::|INTEGER_POSITIVE| . "\\+INTEGER") ; :INTEGER_POSITIVE . '+' INTEGER
    (ebnf::|DOUBLE| . "[0-9]+\\.[0-9]*[eE][+-]?[0-9]+|\\.([0-9])+[eE][+-]?[0-9]+|([0-9])+[eE][+-]?[0-9]+") ; :DOUBLE . [0-9]+ '.' [0-9]* EXPONENT | '.' ([0-9])+ EXPONENT | ([0-9])+ EXPONENT
    (ebnf::|DECIMAL| . "[0-9]*\\.[0-9]+") ; :DECIMAL . [0-9]* '.' [0-9]+
    (ebnf::|INTEGER| . "[0-9]+") ; :INTEGER . [0-9]+
    (ebnf::|LANGTAG| . "@[a-zA-Z]+(-[a-zA-Z0-9]+)*") ; :LANGTAG . '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
    (ebnf::|VAR2| . "$[A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9][A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7]*") ; :VAR2 . '$' VARNAME
    (ebnf::|VAR1| . "\\?[A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9][A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7]*") ; :VAR1 . '?' VARNAME
    (ebnf::|BLANK_NODE_LABEL| . "_:[A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9]([A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7\\.-]*[A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7-])?" ) ; :BLANK_NODE_LABEL . '_:' ( PN_CHARS_U | [0-9] ) ((PN_CHARS|'.')* PN_CHARS)?
    (ebnf::|PNAME_LN| . "([A-Za-z#x00C0-#x00D6#x00D8-#x00F6](([A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7.-])*[A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7-])?)?:(([A-Za-z#x00C0-#x00D6#x00D8-#x00F6_]|:|[0-9]|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%]))(([A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7-]|\\.|:|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%]))*([A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7-]|:|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%])))?)") ; :PNAME_LN . PNAME_NS PN_LOCAL
    (ebnf::|PNAME_NS| . "([A-Za-z#x00C0-#x00D6#x00D8-#x00F6](([A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7.-])*[A-Za-z#x00C0-#x00D6#x00D8-#x00F6_0-9#x00B7-])?)?:") ; :PNAME_NS . PN_PREFIX? ':'
    (ebnf::|IRIREF| . "<([^<>\"{}|^`#x00-#x20])*>") ; :IRIREF . '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
    )
  "The syntax strings in their raw embedded regex format.")

;; TODO: make these optimizations conditional!
;; (setf cl-ppcre:*use-bmh-matchers* t)
;; (setf cl-ppcre:*optimize-char-classes* :charmap)
(defparameter *raw-syntax-strings* *raw-syntax-strings-simple*)

(defparameter *term-names*
  (loop for (term . regex) in *raw-syntax-strings*
        collect term)
  "All known term names.")

(defun terminalp (symbol)
  "Yields truethy iff the given symbol is a terminal symbol."
  (and (assoc symbol *raw-syntax-strings*) t))

(defparameter *syntax-scanners*
  (flet ((prefix (string prefix)
           (concatenate 'string prefix string)))
    (alexandria:alist-hash-table
     (loop for (term . regex) in *raw-syntax-strings*
           collect (cons term
                         (support:-> regex
                           (support:embed-unicode-characters)
                           (prefix "^")
                           (cl-ppcre:create-scanner :multi-line-mode t :case-insensitive-mode nil)))))))

(defparameter *known-words--regular-case-insensitive*
  '("BASE" "PREFIX" "SELECT" "REDUCED" "CONSTRUCT" "DESCRIBE" "ASK" "FROM" "GROUP"
    "HAVING" "ORDER" "BY" "ASC" "DESC" "LIMIT" "OFFSET" "LOAD" "INTO" "CLEAR"
    "DROP" "CREATE" "ADD" "MOVE" "COPY" "TO" "WITH" "WHERE" "DELETE" "INSERT"
    "USING" "DEFAULT" "NAMED" "ALL" "OPTIONAL" "GRAPH" "SERVICE" "SILENT" "BIND"
    "AS" "VALUES" "UNDEF" "MINUS" "UNION" "FILTER"  "IN" "STR" "LANG"
    "LANGMATCHES" "DATATYPE" "BOUND" "IRI" "URI" "BNODE" "RAND" "ABS" "CEIL"
    "FLOOR" "ROUND" "CONCAT" "STRLEN" "UCASE" "LCASE" "ENCODE_FOR_URI" "CONTAINS"
    "STRSTARTS" "STRENDS" "STRBEFORE" "STRAFTER" "YEAR" "MONTH" "DAY" "HOURS"
    "MINUTES" "SECONDS" "TIMEZONE" "TZ" "NOW" "UUID" "STRUUID" "MD5" "SHA1"
    "SHA256" "SHA384" "SHA512" "COALESCE" "IF" "STRLANG" "STRDT" "sameTerm"
    "isIRI" "isURI" "isBLANK" "isLITERAL" "isNUMERIC" "REGEX" "SUBSTR" "REPLACE"
    "NOT" "EXISTS" "COUNT" "SUM" "MIN" "MAX" "AVG" "SAMPLE" "GROUP_CONCAT"
    "DISTINCT"  "SEPARATOR" "true" "false")
  "Regular strings that should be matched case insensitive.")
(defparameter *known-words--case-sensitive*
  '("a"))
(defparameter *known-words--regex-escaped*
  '(("{" . "^\\{")
    ("}" . "^\\}")
    ("." . "^\\.")
    ("?" . "^\\?")
    ("|" . "^\\|")
    ("^" . "^\\^")
    ("[" . "^\\[")
    ("]" . "^\\]")
    ("||" . "^\\|\\|")
    ("&&" . "^&&")
    ("!=" . "^!=")
    ("<" . "^<")
    (">" . "^>")
    ("<=" . "^<=")
    (">=" . "^>=")
    ("/" . "^/")
    ("!" . "^!")
    ("+" . "^\\+")
    ("*" . "^\\*")
    ("-" . "^-")
    ("," . "^,")
    ("(" . "^\\(")
    (";" . "^;")
    ("=" . "^=")
    (")" . "^\\)")
    ("^^" . "^\\^\\^"))
  "An alist containing a phrase as its car and the escaped regex-ready sequence as its cdr.

  This list contains too many sequences, so as to make clear that
  they've been considered for escaping.")
(defparameter *known-words--case-insensitive-with-flexible-spacing*
  `(("INSERT DATA" . ,(support:embed-unicode-characters "^INSERT[#x20#x9#xD#xA]+DATA"))
    ("DELETE DATA" . ,(support:embed-unicode-characters "^DELETE[#x20#x9#xD#xA]+DATA"))
    ("DELETE WHERE" . ,(support:embed-unicode-characters "^DELETE[#x20#x9#xD#xA]+WHERE")))
  "An alist containing a phrase as its car and the regex as its cdr.")

(defparameter *known-words-scanners*
  (let ((table (make-hash-table :test 'equal)))
    (dolist (x *known-words--regular-case-insensitive*)
      (setf (gethash x table)
            (cl-ppcre:create-scanner (format nil "^~a" x) :case-insensitive-mode t)))
    (dolist (x *known-words--case-sensitive*)
      (setf (gethash x table)
            (cl-ppcre:create-scanner (format nil "^~a" x))))
    (dolist (x *known-words--regex-escaped*)
      (setf (gethash (car x) table)
            (cl-ppcre:create-scanner (cdr x))))
    (dolist (x *known-words--case-insensitive-with-flexible-spacing*)
      (setf (gethash (car x) table)
            (cl-ppcre:create-scanner (cdr x) :case-insensitive-mode t)))
    table))

(defun get-known-word-scanner (word)
  "Yields the scanner for WORD."
  (gethash word *known-words-scanners*))

(defun get-symbol-scanner (terminal)
  "Yields the scanner for terminal symbol TERMINAL."
  (gethash terminal *syntax-scanners*))

(defparameter *token-history* (make-hash-table :test 'equal))

(defmacro with-internal-runtime-processing ((var &optional (activep t)) (&rest operations) &body body)
  "Run code and do something with the time it took after processing.

  The consumed time willb e stored in VAR.
  The timed operations are BODY.
  The cleanup processing are OPERATIONS."
  (if activep
      `(let ((,var (common-lisp:get-internal-run-time)))
         (prog1
             (progn ,@body)
           (setf ,var (- (get-internal-run-time) ,var))
           ,@operations))
      `(progn ,@body)))

(declaim (ftype (function ((or #-be-cautious base-string #+be-cautious string symbol)) (function (#-be-cautious base-string #+be-cautious string fixnum) (or null fixnum))) scanner-for))
(defun scanner-for (token)
  (cond ((eq token 'ebnf:|_eof|)
         (lambda (string start)
           (when (= start (length string))
             start)))
        ((eq token 'ebnf::|IRIREF|)
         #'scan-uri)
        ((eq token 'ebnf::|PNAME_NS|)
         #'tree-scan-pname-ns)
        ((eq token 'ebnf::|DOUBLE|)
         #'scan-double)
        ((eq token 'ebnf::|INTEGER|)
         #'scan-integer)
        ((eq token 'ebnf::|INTEGER_POSITIVE|)
         #'scan-integer-positive)
        ((eq token 'ebnf::|INTEGER_NEGATIVE|)
         #'scan-integer-negative)
        ((eq token 'ebnf::|STRING_LITERAL_LONG1|)
         #'scan-string-literal-long-1)
        ((eq token 'ebnf::|STRING_LITERAL_LONG2|)
         #'scan-string-literal-long-2)
        ((eq token 'ebnf::|DECIMAL|)
         #'scan-decimal)
        ((eq token 'ebnf::|BLANK_NODE_LABEL|)
         #'tree-scan-blank-node-label)
        ((eq token 'ebnf::|VAR1|)
         #'scan-var1)
        ((eq token 'ebnf::|VAR2|)
         #'scan-var2)
        ((eq token 'ebnf::|PNAME_LN|)
         #'tree-scan-pname-ln)
        ((eq token 'ebnf::|SPARQLNIL|)
         #'scan-nil)
        ((eq token 'ebnf::|ANON|)
         #'scan-anon)
        ((eq token 'ebnf::|DECIMAL_POSITIVE|)
         #'scan-decimal-positive)
        ((eq token 'ebnf::|DECIMAL_NEGATIVE|)
         #'scan-decimal-negative)
        ((eq token 'ebnf::|DOUBLE_POSITIVE|)
         #'scan-decimal-positive)
        ((eq token 'ebnf::|DOUBLE_NEGATIVE|)
         #'scan-decimal-negative)
        ((eq token 'ebnf::|STRING_LITERAL1|)
         #'scan-string-literal1)
        ((eq token 'ebnf::|STRING_LITERAL2|)
         #'scan-string-literal2)
        (t (alexandria:if-let
               ((scanner (typecase token
                           (string (get-known-word-scanner token))
                           (symbol (get-symbol-scanner token))
                           (t (error "Could not find scanners for type ~A of token ~A" (type-of token) token)))))
             (lambda (string start)
               (multiple-value-bind (start end)
                   (cl-ppcre:scan scanner string :start start)
                 (declare (ignore start))
                 end))
             (error "Could not find token scanner for ~A" token)))))

(declaim (ftype (function ((or #-be-cautious base-string #+be-cautious string symbol) fixnum #-be-cautious base-string #+be-cautious string) (or null fixnum)) scan))
(defun scan (token start string)
  ;; (incf (gethash token *token-history* 0))
  (with-internal-runtime-processing (time-spent nil)
      ((incf (the fixnum (gethash token *token-history* 0)) time-spent))
    (funcall (scanner-for (if (stringp token) (coerce token '#-be-cautious base-string #+be-cautious string) token)) string start)))
