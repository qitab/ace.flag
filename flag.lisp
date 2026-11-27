;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Utilities to parse command line flags.
;;; The parse-command-line utility offers parsing of command line flags that
;;; is close to ABSL binary flag parsing.
;;;
;;; The parser supports flags defined through the FLAG:DEFINE macro.
;;;
;;; Flags whose type is defined or expands to 'boolean' are considered boolean flags and this
;;; affects how those are parsed. Especially, the '--no' sets the flag to 'nil' and no other
;;; argument is consumed. Boolean flags that are not followed by another argument or the other
;;; argument is prefixed with '-' are set to 't' and no argument is consumed.
;;; Finally, boolean flags accept a set of boolean indicators: "yes", "no", "true", "false", ...
;;; which are consumed by the boolean flag at parsing.
;;;

(defpackage #:ace.flag
  (:use #:common-lisp
        #:ace.core.check
        #:sb-kernel)
  (:local-nicknames (#:string #:ace.core.string))
  (:export #:command-line
           #:parse-command-line
           #:print-help
           #:*help-preamble*
           #:define))

(in-package #:ace.flag)

;;; Some string-parsing things

(defun true-value-string-p (value)
  "Tests that the string VALUE is one of the designators for a true boolean value."
  (declare (optimize (safety 1))) ; re-enable type-checking if it was 0
  (declare (string value) (values boolean &optional))
  (and (member value '("true" "yes" "t") :test #'equalp) t))

(defun false-value-string-p (value)
  "Tests that the string VALUE is one of the designators for a false boolean value."
  (declare (optimize (safety 1))) ; re-enable type-checking if it was 0
  (declare (string value) (values boolean &optional))
  (and (member value '("false" "null" "nil" "no") :test #'equalp) t))

(defun check-compound-specifier (specifier)
  "Checks that SPECIFIER (after top-level deftype expansion) does not use AND,
and uses OR only as a two-arm nullable union (OR NULL T) or (OR T NULL)."
  (let ((spec (sb-ext:typexpand specifier)))
    (when (consp spec)
      (case (first spec)
        (and
         (error "Conjunction type ~S is not supported as a flag type." specifier))
        (or
         (let ((arms (rest spec))
               (null-type (specifier-type 'null)))
           (unless (and (= (length arms) 2)
                        (let ((c1 (specifier-type (first arms)))
                              (c2 (specifier-type (second arms))))
                          (if (type= c1 null-type)
                              (not (type= c2 null-type))
                              (type= c2 null-type))))
             (error "Union flag type ~S must be of the form (OR NULL <type>) or (OR <type> NULL)."
                    specifier))))))))

(defun acceptable-ctype-p (ctype)
  "Returns true if CTYPE is supported by the built-in flag parser:
BOOLEAN, symbol MEMBER enums, STRING, single-interval INTEGER subtypes
(including FIXNUM), SINGLE-FLOAT, DOUBLE-FLOAT, or a nullable union with NULL."
  (let* ((null-type (specifier-type 'null))
         (nullable-p (csubtypep null-type ctype))
         (non-null (if nullable-p
                       (type-difference ctype null-type)
                       ctype)))
    (and (or (and (member-type-p non-null)
                  (every #'symbolp (member-type-members non-null)))
             (type= non-null (specifier-type 'string))
             (and (numeric-type-p non-null)
                  (eq (numeric-type-complexp non-null) :real)
                  (member (numeric-type-class non-null) '(integer float))))
         t)))

(defun parse-as-type (specifier value)
  "Parses string VALUE according to SPECIFIER. Returns (values parsed-value parsed-p)."
  (declare (type (or symbol cons) specifier) (type string value))
  (check-compound-specifier specifier)
  (let* ((ctype (specifier-type specifier))
         (null-type (specifier-type 'null))
         (nullable-p (csubtypep null-type ctype))
         (non-null (if nullable-p
                       (type-difference ctype null-type)
                       ctype)))
    (cond ((type= ctype (specifier-type 'boolean))
           (cond ((true-value-string-p value)  (values t t))
                 ((false-value-string-p value) (values nil t))
                 (t (values nil nil))))
          ((and (member-type-p non-null)
                (every #'symbolp (member-type-members non-null)))
           (let ((found (find value (member-type-members non-null)
                              :key #'symbol-name :test #'string-equal)))
             (cond (found (values found t))
                   ((and nullable-p (false-value-string-p value)) (values nil t))
                   (t (values nil nil)))))
          ((type= non-null (specifier-type 'string))
           (values value t))
          ((and (numeric-type-p non-null)
                (eq (numeric-type-complexp non-null) :real)
                (member (numeric-type-class non-null) '(integer float)))
           (let* ((len (length value))
                  (parsed
                   (and (plusp len)
                        (ignore-errors
                         (case (numeric-type-class non-null)
                           ;; With integers, we use our own logic to recognize hex input
                           ;; because strtol() contains a footgun: 0100 is octal, not decimal.
                           ;; With floats, strtod() gets us infinity and NaN handling for free.
                           (integer
                            (let* ((sign (case (char value 0) (#\- -1) (#\+ 1)))
                                   (start (if sign 1 0)))
                              (if (and (< (+ start 2) len)
                                       (digit-char-p (char value (+ start 2)) 16)
                                       (string-equal value "0x" :start1 start :end1 (+ start 2)))
                                  (* (or sign 1) (parse-integer value :start (+ start 2) :radix 16))
                                  (parse-integer value))))
                           (float (multiple-value-bind (d consumed) (sb-posix:strtod value)
                                    (when (and d (plusp consumed) (= consumed (length value)))
                                      (coerce d (numeric-type-format non-null))))))))))
             (cond ((and parsed (ctypep parsed non-null))
                    (values parsed t))
                   ((and nullable-p (false-value-string-p value))
                    (values nil t))
                   (t
                    (values nil nil)))))
          (t
           (error "Unsupported flag type specifier: ~S" specifier)))))

;;;
;;; Defining flags ...
;;;

(defconstant +flags-package+ (or (find-package "FLAGS") (make-package "FLAGS"))
  "The default package for flags. All flags are external symbols in this package.")

(defvar *flags* (make-hash-table :test #'equal)
  "Stores a map from flag names to variable symbols.")

(defvar *flags-normalized* (make-hash-table :test #'equal)
  "Stores a map from flag names in the normalized form to variable symbols.")

(defun acceptable-flag-name-p (name)
  "True if the NAME is in lower case, starts with an alpha character, contains only alphanumeric
 characters, dots, underscores, or hyphens, and ends with an alphanumeric character.
 The name is not allowed to contain both underscores and hyphens. Single-letter flag names
 may be uppercase.
 "
  (declare (string name) (values boolean))
  (let ((length (length name)))
    (cond ((> length 1)
           (and (every (lambda (c) (or (and (lower-case-p c) (alpha-char-p c))
                                       (digit-char-p c)
                                       (member c '(#\- #\_ #\.) :test #'char=)))
                       name)
                (alpha-char-p (char name 0))
                (alphanumericp (char name (1- length)))
                (not (and (find #\- name :test #'char=)
                          (find #\_ name :test #'char=)))))
          ((= length 1)
           (alpha-char-p (char name 0))))))

(defun normalized-flag-name (name)
  "Trims '*' from the NAME. Replaces '_' with '-'. Sets the string to lower case."
  (declare (string name) (values (or null string)))
  (let ((trimmed (string-trim "*" (substitute #\- #\_ name))))
    (when (> (length trimmed) 1)
      (setf trimmed (string-downcase trimmed)))
    (and (acceptable-flag-name-p trimmed) trimmed)))

(defmacro define (flag default doc &key type name names parser
                                   (def 'sb-ext:define-load-time-global)
                                   setter
                                   &aux parsed-type)
  (declare (type (member sb-ext:define-load-time-global defvar defparameter) def))
  "Defines a flag and registers it as such under a name with stripped '*' and '-' in place of '_'.
 Flags in the FLAGS package are external. Note that the default name of the flag at command line
 does not include the package specifier and thus flags that share the same name may rise conflicts.
 Flags that accept nil allow for the --noflag syntax and may conflict with other flags having
 that 'no' prefixed name.

 Macro arguments:
  FLAG          - Defines the Lisp parameter that will store the flag.
                  When the symbol specified is a string or a keyword,
                  the flag is interned into and exported from the FLAGS package.
  DEFAULT       - The default value for the flag.
  DOC           - The documentation for the variable that also shows in help.
  TYPE          - Type to be assigned to the variable.
  NAME, NAMES   - The names of the flag to be used at the command-line.
                  Unless the NAME is specified the default is the name of the variable
                  in lowercase and with the '*' characters trimmed.
  PARSER        - A parser function to transform the value string into the flag value.
                  A non-nil return value from the parser is considered correctly parsed.
                  Use (values nil t) to represent correctly parsed NIL value.
                  If parser is not specified, a default parser for the flag type maybe invoked
                  using type-utils:parse-type method.
  DEF           - The defining operation used to define the flag.
                  Default is SB-EXT:DEFINE-LOAD-TIME-GLOBAL.
  SETTER        - Optional custom setter function used to set the flag instead of SET."

  (when name (push name names))
  (flet ((fail (&rest args)
           (apply #'warn args) (return-from define)))
    (unless (typep flag '(or string symbol))
      (fail "The name ~S of the flag is not a string or symbol." flag))
    (unless (typep doc 'string)
      (fail "The flag ~S requires a help string." doc))
    (unless type
      (fail "Flag definition ~S lacks a :TYPE" flag))
    (unless (typep type '(or symbol cons))
      (fail "The type of flag ~S needs to be a proper type specifier. Provided: ~S." flag type))
    (unless (symbolp parser)
      (fail "The parser ~S specified for the flag ~S is not a symbol." parser flag))
    (setq parsed-type (specifier-type type))
    (when (csubtypep (specifier-type 'keyword) parsed-type)
      (error "Don't parse flag ~S as a symbol. Use string instead" flag))
    (unless parser
      (check-compound-specifier type)
      (unless (acceptable-ctype-p parsed-type)
        (error "Unsupported flag type ~S for flag ~S." type flag)))
    (dolist (name names)
      (unless (stringp name)
        (fail "The additional names of the flag ~S need to be strings. Provided: ~S." flag name))
      (unless (plusp (length name))
        (fail "One of the names ~S for the flag ~S is empty." names flag)))

    (when (typep flag '(or keyword string))
      (export (setf flag (intern (string flag) +flags-package+)) +flags-package+))

    (let* ((non-normalized (remove-if #'acceptable-flag-name-p names))
           (len (length non-normalized)))
      (when non-normalized
        (fail "The name~P~{ ~S~} for the flag ~S ~:[is~;are~] not well formed. ~
               The flag name can contain only alphanumeric and the '.', '-' and '_' characters. ~
               It should start with an alpha character and end with an alphanumeric character."
              len non-normalized flag (> len 1))))

    (let ((flag-variable-name
           (unless names
             (typecase flag
               (string flag)
               (symbol (string-trim "*" (string-downcase (symbol-name flag))))))))
      (when (and (null names) (not (acceptable-flag-name-p flag-variable-name)))
        (fail "Cannot derive a flag name for the flag ~S. ~
                The flag name can contain only alphanumeric and the '.' and '-' characters. ~
                It should start with an alpha character and end with an alphanumeric character."
              flag))

      (let* ((provided-names (or names (list flag-variable-name)))
             (names (mapcar #'normalized-flag-name provided-names))
             (nullable (csubtypep (specifier-type 'null) parsed-type)))

        `(progn
           (register ',flag ',provided-names ,nullable *flags*)
           (register ',flag ',names ,nullable *flags-normalized*)
           ,@(when parser
               `((setf (get ',flag 'parser) ',parser)))
           (setf (get ',flag 'specified-type) ',type)
           ,@(when setter `((setf (get ',flag 'setter) ,setter)))

           (declaim (type ,type ,flag))
           (,def ,flag (the ,type ,default) ,doc)

           ,@(when (eq (symbol-package flag) +flags-package+)
               ;; All flags in the flags package are external.
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (export ',flag +flags-package+))))
           ',flag)))))

(defun no-prefix-p (name)
  (declare (string name))
  "True if flag NAME starts with the 'no' prefix."
  (and (> (length name) 2) ; the whole flag name can't be just "no"
       (string:prefixp "no" name)
       (alpha-char-p (char name 2))))

(defun register (flag names nullablep registry)
  "Registers the FLAG and checks for flag name clashes.
 Arguments:
 FLAG - is the symbol name for the variable,
 NAMES - is a list of strings to be used as a flag name on the command line,
 NULLABLEP - indicates that the type of the flag can accept the NULL value.
 REGISTRY - is the hashtable that maps names to variable symbols."
  (declare (symbol flag) (list names) (values symbol))
  (dolist (name names)
    ;; Check for conflicting flag names.
    (unless (or (member (gethash name registry) (list nil flag) :test #'eq)
                (null (symbol-package (gethash name registry))))
      (restart-case
          (error "The flag name ~S was registered for symbol ~S." name (gethash name registry))
        (overwrite ()
          :report "Overwrite the flag-name mapping and point it to the new flag."
          (remhash name registry))
        (skip-name ()
          :report "Skip this flag name and let the old flag-name point to the old symbol."
          (setf name nil))
        (skip-flag ()
          :report "Skip this flag entirely and let the old flag-name point to the old symbol."
          (return-from register))))

    (when name
      ;; Check for boolean flag clashes.
      (when (no-prefix-p name)
        (let ((name-no (subseq name 2)))
          (when (gethash name-no registry)
            (restart-case
                (error "Flag ~S starts with the 'no' prefix and there was a flag ~S for ~S."
                       name name-no (gethash name-no registry))
              (overwrite ()
                :report "Overwrite the flag-name mapping and point it to the new flag."
                (remhash name-no registry))))))

      (when nullablep
        (let ((noname (format nil "no~A" name)))
          (when (gethash noname registry)
            (restart-case
                (error "The flag ~S for ~S shadowed the negated form of ~S."
                       noname (gethash noname registry) name)
              (overwrite ()
                :report "Overwrite the flag-name mapping and point it to the new flag."
                (remhash noname registry))))))

      (setf (gethash name registry) flag)))
  flag)

;;;
;;; Print help ...
;;;

(defvar *print-help-exclude-packages* '("COMMON-LISP")
  "A list of packages that are excluded from the PRINT-HELP output.")

(declaim ((or string null) *help-preamble*))
(sb-ext:defglobal *help-preamble* nil "String printed at the top of PRINT-HELP.")

(defun print-help (&key
                       (stream *standard-output*)
                       (right-margin 80)
                       (indent 4)
                       (indent-help 5)
                       (parentheses "()")
                       (prefix "--")
                       (normalize nil)
                       (include-packages nil)
                       (exclude-packages *print-help-exclude-packages*))
   "Prints help for flags registered using the DEFINE macro.
The flags are ordered by package name and by the symbol-name.

Parameters:
 STREAM       - output stream (default *standard-output*).
 RIGHT-MARGIN - the print right-margin for the flag names and docstrings.
 INDENT       - the indentation of the flag names.
 INDENT-HELP  - the indentation of the docstrings.
 PARENTHESES  - NIL or characters used to surround the help text for each flag.
                (NIL, a character, or a sequence of two characters.)
 PREFIX       - one of '-' or '--' for prefix to be used when printing long flag names.
 NORMALIZE - if non-nil, the names of flags are printed in the normalized form.
             Normalized form uses hyphens and lower-case characters.
 INCLUDE-PACKAGES - a list of package designators to print flags from.
 EXCLUDE-PACKAGES - a list of packages excluded from the PRINT-HELP output."
  (declare (stream stream)
           (fixnum right-margin indent indent-help)
           (list include-packages exclude-packages))
  (when *help-preamble* (write-string *help-preamble* stream))
  (let ((*print-right-margin* right-margin)
        (*print-pretty* t)
        (*package* (find-package "COMMON-LISP-USER"))
        (line-prefix (make-string (max 0 indent) :initial-element #\Space))
        (flags->names (make-hash-table :test #'eq))
        (flags-sorted)
        last-package
        open-char close-char)

    (setf include-packages (mapcar #'string include-packages)
          exclude-packages (mapcar #'string exclude-packages))

    (multiple-value-setq (open-char close-char)
      (typecase parentheses
        (null (values nil nil))
        (character (values parentheses parentheses))
        (sequence (values (elt parentheses 0) (elt parentheses 1)))))

    (flet ((included-package-p (package)
             (when package
               (if (null include-packages)
                   (not (member (package-name package) exclude-packages :test #'string=))
                   (member (package-name package) include-packages :test #'string=)))))

      (maphash (lambda (name flag)
                 (when (included-package-p (symbol-package flag))
                   (pushnew name (gethash flag flags->names) :test #'equalp)))
               (if normalize *flags-normalized* *flags*)))

    (maphash (lambda (flag names) (push (list* flag names) flags-sorted)) flags->names)

    ;; Sorting by package and flag-name.
    (setf flags-sorted
          (stable-sort
           (sort flags-sorted #'string<
                 :key (lambda (s) (normalized-flag-name (symbol-name (car s)))))
           #'string< :key (lambda (s) (package-name (symbol-package (car s))))))

    (dolist (flag+names flags-sorted)
      (let* ((flag (first flag+names))
             (names (sort (rest flag+names) #'string<))
             (doc (documentation flag 'variable))
             (type (or (get flag 'specified-type) (error "can't call type:declaimed on ~S" flag)))
             (value (and (boundp flag) (symbol-value flag)))
             (flag-package (symbol-package flag)))
        (unless (eq flag-package last-package)
          (format stream "~:[~;~%~]Flags from ~(~A~):~%"
                  last-package (package-name flag-package))
          (setf last-package flag-package))
        (terpri stream)
        (pprint-logical-block (stream nil :prefix line-prefix)
          (dolist (name names)
            (let ((prefix (if (= 1 (length name)) "-" prefix)))
              (format stream "~@<~@;~A~A~:[~_~;~:@_~]~:>" prefix name (cdr names))))
          (when (plusp (length doc))
            (format stream "~vT~@[~C~]~@<~@;~A~@[~C~]~:>"
                    indent-help open-char doc close-char))

          (let ((*print-lines* 1)
                (*print-circle* t)
                (*print-level* 3)
                (*print-length* 5))
            (when type
              (format stream "~_~vTType: ~S" indent-help type))
            (when value
              (ignore-errors
               (format stream "~_~vTValue: ~S" indent-help value)))))
        (terpri stream)))
    (values)))

;;;
;;; Command line ...
;;;

(defun command-line ()
  "Returns the full command-line as a list with program name and arguments."
  (declare (values list))
  sb-ext:*posix-argv*)

;;;
;;; Flag parsing ...
;;;

(defparameter *normalize* nil
  "When non-nil the parsed flags will be transformed into a normalized form.
 The normalized form contains hyphens in place of underscores, trims '*' characters,
 and puts the name into lower case for flags names longer than one character.")

(defun flag-info (arg)
  "Search for a variable and a type corresponding to the flag-name as specified by ARG.
 The flag-name is optionally put into the normalized form (see *NORMALIZE*).
 Returns (values flag-name var type no-p):
  flag-name - the argument with '_' replaced by '-' with optionally removed '--' or '--no' prefix.
  var - the variable symbol.
  no-p - indicator that the flag-name was prefixed with 'no'."
  (declare (string arg) (values (or null string) symbol boolean))
  (let ((arg-no-dashes (cond ((and (> (length arg) 2) (string:prefixp "--" arg))
                              (subseq arg 2))
                             ((and (> (length arg) 1) (string:prefixp "-" arg))
                              (subseq arg 1))))
        (flags (if *normalize* *flags-normalized* *flags*)))
    (if arg-no-dashes
        (let* ((flag-string (if *normalize*
                                (normalized-flag-name arg-no-dashes)
                                arg-no-dashes))
               (no-p (and flag-string
                          (no-prefix-p flag-string)
                          (not (gethash flag-string flags))))
               (flag-name (if no-p (subseq flag-string 2) flag-string))
               (variable (gethash flag-name flags)))
          (values (or flag-name arg-no-dashes) variable no-p))
        (values nil nil nil))))

(defun boolean-type-p (type)
  "Returns true if the TYPE is a boolean type-specifier."
  (declare ((or cons symbol) type))
  (type= (specifier-type type) (specifier-type 'boolean)))

(defun parse-variable (variable value &key no-p equal-sign-p)
  "Parses a string VALUE accordingly to the type of a VARIABLE.
 NO-P means that the flag had a 'no' prefix.
 EQUAL-SIGN-P means that the flag and value pair have been specified with the '=' sign.
 Returns (values type value parsed-p consume-p)."
  (declare (symbol variable) ((or null string) value)
           (values (or cons symbol) t boolean boolean))
  (let* ((specified-type (get variable 'specified-type))
         (type (or specified-type (error "can't call type:declaimed on ~S" variable)))
         (parser (get variable 'parser)))

    (cond ((and no-p (typep nil type))
           (values type nil t nil))

          (parser
           (multiple-value-bind (result parsed-p) (funcall parser value)
             (values type result (and (or parsed-p result) (typep result type) t) t)))

          ((boolean-type-p type)
           ;; Special handling of pure boolean flags.
           (cond ((or (null value) (string:prefixp "-" value))
                  ;; No value or the next arg is actually a flag.
                  (values type t t nil))
                 ((and equal-sign-p (true-value-string-p value))
                  ;; A value of "true", "yes", "t".
                  (values type t t t))
                 ((and equal-sign-p (false-value-string-p value))
                  ;; A value of "false", "null", "nil", "no".
                  (values type nil t t))
                 (t
                  (values type nil nil nil))))

          (value
           (multiple-value-bind (result parsedp) (parse-as-type type value)
             (values type result (and parsedp (typep result type)) t)))
          (t
           (values nil nil nil nil)))))

(defun parse-command-line (&key (args (command-line))
                                (setp t)
                                ((:normalize *normalize*) *normalize*))
  "Parses the flags taken by default from the program command-line arguments.
 Arguments:
  ARGS - are the program arguments, the first one of which usually being the program name,
  SETP - if true, the variables are set as they are parsed,
  NORMALIZE - if true, the names of arguments are put into a normalized form.
 Returns (values unparsed-args parsed-flag-variables parsed-values)."
  (ace.core.collect:with-collectors (parsed-vars parsed-values unparsed)
    (loop (unless args (return))
          (let* ((arg (pop args))
                 (pos= (position #\= arg)) ; Support the --flag=value syntax.
                 (flag-string (if pos= (subseq arg 0 pos=) arg))
                 (value-string (if pos= (subseq arg (1+ pos=)) (car args))))
            (multiple-value-bind (flag-name var no-p) (flag-info flag-string)
              (cond ((equal flag-string "--")
                       ;; An empty flag stops parsing of the arguments.
                       (unparsed arg)
                       (mapc #'unparsed args)
                       (return))

                      ;; Could not locate the variable or
                      ;; the flag has --noflag=value syntax.
                    ((or (null var) (and no-p pos=))
                       (unparsed arg)
                       (unless (or pos= (null args) (string:prefixp "-" (car args)))
                         (unparsed (pop args))))

                    (t
                       (multiple-value-bind (type value parsed-p consume-p)
                           (parse-variable var value-string :no-p no-p :equal-sign-p (and pos= t))
                         (check parsed-p "Could not parse ~S as the value of ~S [type: ~A]"
                                value-string flag-name type)
                         (when setp
                           (let ((setter (get var 'setter)))
                             (if setter
                                 (funcall setter value)
                                 (set var value))))
                         (parsed-vars var)
                         (parsed-values value)
                         (when (and consume-p (not pos=))
                           (pop args))))))))

    (values unparsed parsed-vars parsed-values)))
