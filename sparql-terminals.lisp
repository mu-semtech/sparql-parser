(in-package :sparql-terminals)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SPARQL terminal syntax
;;;;
;;;; Scanners for SPARQL's terminal syntax

(defparameter *raw-syntax-strings*
  `((sparql-bnf::|PN_LOCAL_ESC| . "\\\\[_~.\\-!$&'()*+,;=/?#@%]")
    (sparql-bnf::|HEX| . "[0-9A-Fa-f]")
    (sparql-bnf::|PERCENT| . "%[0-9A-Fa-f][0-9A-Fa-f]")
    (sparql-bnf::|PLX| . "(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%])")
    (sparql-bnf::|PN_LOCAL| . "([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_]|:|[0-9]|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%]))(([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-]|\\.|:|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%]))*([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-]|:|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%])))?"  ) ; :PN_LOCAL . (PN_CHARS_U | ':' | [0-9] | PLX ) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX) )?
    (sparql-bnf::|PN_PREFIX| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF](([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-.])*[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-])?") ; :PN_PREFIX . PN_CHARS_BASE ((PN_CHARS|'.')* PN_CHARS)?
    (sparql-bnf::|PN_CHARS| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-]") ; :PN_CHARS . PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
    (sparql-bnf::|VARNAME| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9][A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040]*") ; :VARNAME . ( PN_CHARS_U | [0-9] ) ( PN_CHARS_U | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040] )*
    (sparql-bnf::|PN_CHARS_U| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_]") ; :PN_CHARS_U . PN_CHARS_BASE | '_'
    (sparql-bnf::|PN_CHARS_BASE| . "[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF]") ; :PN_CHARS_BASE . [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
    (sparql-bnf::|ANON| . "\\[[#x20#x9#xD#xA]*\\]") ; :ANON . '[' WS* ']'
    (sparql-bnf::|WS| . "[#x20#x9#xD#xA]") ; :WS . #x20 | #x9 | #xD | #xA
    (sparql-bnf::|NIL| . "\\([#x20#x9#xD#xA]*\\)") ; :NIL . '(' WS* ')'
    (sparql-bnf::|ECHAR| . "\\\\[tbnrf\\\\\"']") ; :ECHAR . '\' [tbnrf\"']
    (sparql-bnf::|STRING_LITERAL_LONG2| . "\"\"\"((\"|\"\")?([^\"\\]|\\\\[tbnrf\\\\\"']))*\"\"\"") ; :STRING_LITERAL_LONG2 . '"""' ( ( '"' | '""' )? ( [^"\] | ECHAR ) )* '"""'
    (sparql-bnf::|STRING_LITERAL_LONG1| . "'''(('|'')?([^'\\\\]|\\\\[tbnrf\\\\\"']))*'''") ; :STRING_LITERAL_LONG1 . "'''" ( ( "'" | "''" )? ( [^'\] | ECHAR ) )* "'''"
    (sparql-bnf::|STRING_LITERAL2| . "\"(([^#x22#x5C#xA#xD])|\\\\[tbnrf\\\\\"'])*\"") ; :STRING_LITERAL2 . '"' ( ([^#x22#x5C#xA#xD]) | ECHAR )* '"'
    (sparql-bnf::|STRING_LITERAL1| . "'(([^#x27#x5C#xA#xD])|\\\\[tbnrf\\\\\"'])*'") ; :STRING_LITERAL1 . "'" ( ([^#x27#x5C#xA#xD]) | ECHAR )* "'"
    (sparql-bnf::|EXPONENT| . "[eE][+-]?[0-9]+") ; :EXPONENT . [eE] [+-]? [0-9]+
    (sparql-bnf::|DOUBLE_NEGATIVE| . "-([0-9]+\\.[0-9]*[eE][+-]?[0-9]+|\\.([0-9])+[eE][+-]?[0-9]+|([0-9])+[eE][+-]?[0-9]+)" ) ; :DOUBLE_NEGATIVE . '-' DOUBLE
    (sparql-bnf::|DECIMAL_NEGATIVE| . "-[0-9]*\\.[0-9]+") ; :DECIMAL_NEGATIVE . '-' DECIMAL
    (sparql-bnf::|INTEGER_NEGATIVE| . "-INTEGER") ; :INTEGER_NEGATIVE . '-' INTEGER
    (sparql-bnf::|DOUBLE_POSITIVE| . "\\+([0-9]+\\.[0-9]*[eE][+-]?[0-9]+|\\.([0-9])+[eE][+-]?[0-9]+|([0-9])+[eE][+-]?[0-9]+)") ; :DOUBLE_POSITIVE . '+' DOUBLE
    (sparql-bnf::|DECIMAL_POSITIVE| . "\\+[0-9]*\\.[0-9]+") ; :DECIMAL_POSITIVE . '+' DECIMAL
    (sparql-bnf::|INTEGER_POSITIVE| . "\\+INTEGER") ; :INTEGER_POSITIVE . '+' INTEGER
    (sparql-bnf::|DOUBLE| . "[0-9]+\\.[0-9]*[eE][+-]?[0-9]+|\\.([0-9])+[eE][+-]?[0-9]+|([0-9])+[eE][+-]?[0-9]+") ; :DOUBLE . [0-9]+ '.' [0-9]* EXPONENT | '.' ([0-9])+ EXPONENT | ([0-9])+ EXPONENT
    (sparql-bnf::|DECIMAL| . "[0-9]*\\.[0-9]+") ; :DECIMAL . [0-9]* '.' [0-9]+
    (sparql-bnf::|INTEGER| . "[0-9]+") ; :INTEGER . [0-9]+
    (sparql-bnf::|LANGTAG| . "@[a-zA-Z]+(-[a-zA-Z0-9]+)*") ; :LANGTAG . '@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*
    (sparql-bnf::|VAR2| . "$[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9][A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040]*") ; :VAR2 . '$' VARNAME
    (sparql-bnf::|VAR1| . "\\?[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9][A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040]*") ; :VAR1 . '?' VARNAME
    (sparql-bnf::|BLANK_NODE_LABEL| . "_:[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9]([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040\\.-]*[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-])?" ) ; :BLANK_NODE_LABEL . '_:' ( PN_CHARS_U | [0-9] ) ((PN_CHARS|'.')* PN_CHARS)?
    (sparql-bnf::|PNAME_LN| . "([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF](([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-.])*[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-])?)?:(([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_]|:|[0-9]|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%]))(([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-]|\\.|:|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%]))*([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-]|:|(%[0-9A-Fa-f][0-9A-Fa-f])|(\\\\[_~.\\-!$&'()*+,;=/?#@%])))?)") ; :PNAME_LN . PNAME_NS PN_LOCAL
    (sparql-bnf::|PNAME_NS| . "([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF](([A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-.])*[A-Za-z#x00C0-#x00D6#x00D8-#x00F6#x00F8-#x02FF#x0370-#x037D#x037F-#x1FFF#x200C-#x200D#x2070-#x218F#x2C00-#x2FEF#x3001-#xD7FF#xF900-#xFDCF#xFDF0-#xFFFD#x10000-#xEFFFF_0-9#x00B7#x0300-#x036F#x203F-#x2040-])?)?:") ; :PNAME_NS . PN_PREFIX? ':'
    (sparql-bnf::|IRIREF| . "<([^<>\"{}|^`#x00-#x20])*>") ; :IRIREF . '<' ([^<>"{}|^`\]-[#x00-#x20])* '>'
    )
  "The syntax strings in their raw embedded regex format.")

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

(defun scan (token start string)
  (if (eq token 'sparql-bnf:|_eof|)
      (when (= start (length string))
        start)
      (alexandria:if-let
          ((scanner (typecase token
                      (string (get-known-word-scanner token))
                      (symbol (get-symbol-scanner token))
                      (t (error "Could not find scanners for type ~A of token ~A" (type-of token) token)))))
        (multiple-value-bind (start end)
            (cl-ppcre:scan scanner string :start start)
          (declare (ignore start))
          end)
        (error "Could not find token scanner for ~A" token)))))
