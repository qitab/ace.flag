;;; Utilities to parse command line flags.
;;; The parse-command-line utility offers parsing of command line flags that
;;; is close to google3 binary flag parsing.
;;;
;;; The parser supports flags defined through the FLAG:DEFINE macro.
;;; ace.flag:print-help is a utility that formats the flags registered with define-flag.
;;;
;;; Flags may refer to special variables. E.g. --cl:print-base and --cl::*print-base* refer to
;;; the '*print-base*' special variable. This behavior is controlled by --lisp-global-flags.
;;; The user may also allow this by default by passing :global-flags t
;;; to the PARSE-COMMAND-LINE function.
;;;
;;; Flags whose type is defined or expands to 'boolean' are considered boolean flags and this
;;; affects how those are parsed. Especially, the '--no' sets the flag to 'nil' and no other
;;; argument is consumed. Boolean flags that are not followed by another argument or the other
;;; argument is prefixed with '-' are set to 't' and no argument is consumed.
;;; Finally, boolean flags accept a set of boolean indicators: "yes", "no", "true", "false", ...
;;; which are consumed by the boolean flag at parsing.
;;;
;;; Flags may accept any combination of types.
;;; By default the values parsed are: numbers, symbols, keywords, strings.
;;; Numbers are parsed in C++/Java syntax allowing for non-finite values ("inf", "-inf", "nan").
;;;
;;;

(ace.core.package:defpackage* #:ace.flag
  (:use #:common-lisp
        #:ace.core.defun
        #:ace.core.check)
  (:use-alias #:ace.core.string
              #:ace.core.macro
              #:ace.core.number
              #:ace.core.os
              #:ace.core.type
              #:ace.flag.parse)
  (:import-from #:ace.core.check #:check)
  (:import-from #:ace.core.collect #:with-collectors)
  (:export #:command-line
           #:parse-command-line
           #:print-help
           #:define
           #:*global-flags*
           #:*normalize*))

(in-package #:ace.flag)

;;;
;;; Defining flags ...
;;;

(defconstant +flags-package+ (or (find-package "FLAGS") (make-package "FLAGS"))
  "The default package for flags. All flags are external symbols in this package.")

(defvar *flags* (make-hash-table :test #'equal)
  "Stores a map from flag names to variable symbols.")

(defvar *flags-normalized* (make-hash-table :test #'equal)
  "Stores a map from flag names in the normalized form to variable symbols.")

(defun* acceptable-flag-name-p (name)
  "True if the NAME is in lower case, starts with an alpha character, contains only alphanumeric
 characters, dots, underscores, or hyphens, and ends with an alphanumeric character.
 The name is not allowed to contain both underscores and hyphens. Single-letter flag names
 may be uppercase.
 "
  (declare (self foldable (string) boolean))
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

(defun* normalized-flag-name (name)
  "Trims '*' from the NAME. Replaces '_' with '-'. Sets the string to lower case."
  (declare (self foldable (string) (or null string)))
  (let ((trimmed (string-trim "*" (substitute #\- #\_ name))))
    (when (> (length trimmed) 1)
      (setf trimmed (string-downcase trimmed)))
    (and (acceptable-flag-name-p trimmed) trimmed)))

(defmacro define (flag default doc &key type name names parser (def 'defparameter) setter)
  "Defines a flag and registers it as such under a name with stripped '*' and '-' in place of '_'.
 Flags in the FLAGS package are external. Note that the default name of the flag at command line
 does not include the package specifier and thus flags that share the same name may rise conflicts.
 Flags that accept nil allow for the --noflag syntax and may conflict with other flags having
 that 'no' prefixed name.

 Macro arguments:
  FLAG          - Defines the Lisp parameter that will store the flag.
                  When the symbol specified is a string or a keyword,
                  the flag is interned into and exported from the FLAGS package.
  DEFAULT       - The default value for the flag. May determine the type if not provided.
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
                  Default is DEFPARAMETER. If NIL, the flag variable is not defined by this form
                  and the default value is ignored.
  SETTER        - The setter used to set the flag. If NIL, a default SETF setter is used.

 If no TYPE has been specified the type of the flag is derived from the DEFAULT value by following:
    BOOLEAN       -> BOOLEAN
    BIT           -> INTEGER
    FIXNUM        -> INTEGER
    CONS          -> LIST
    any STRING    -> STRING
    any CHARACTER -> CHARACTER
    otherwise     -> (type-of value)."

  (when name (push name names))
  (flet ((fail (&rest args)
           (apply #'warn args) (return-from define)))
    (unless (typep flag '(or string symbol))
      (fail "The name ~S of the flag is not a string or symbol." flag))
    (unless (typep doc 'string)
      (fail "The flag ~S requires a help string." doc))
    (unless (typep type '(or symbol cons))
      (fail "The type of flag ~S needs to be a proper type specifier. Provided: ~S." flag type))
    (dolist (name names)
      (unless (stringp name)
        (fail "The additional names of the flag ~S need to be strings. Provided: ~S." flag name))
      (unless (plusp (length name))
        (fail "One of the names ~S for the flag ~S is empty." names flag)))
    (unless (or (null type) (type:unknownp type)
                (not (constantp default)) (typep (eval default) type)) ; NOLINT
      (fail "The flag ~S default ~S is not of the required type: ~S." flag default type))

    (unless (symbolp parser)
      (fail "The parser ~S specified for the flag ~S is not a symbol." parser flag))

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
             (value (macro:gensym* :value))
             (specified-type type))

        (unless type
          ;; Derive type from the type of the default argument.
          (let ((declaimed (type:declaimed flag)))
            (setf type (cond ((not (member declaimed '(t nil)))
                              declaimed)
                             ((constantp default)
                              (type:upgraded-type-of (eval default))) ; NOLINT
                             (t nil)))))
        `(progn
           (let ((nullable (typep nil ',type)))
             (register ',flag ',provided-names nullable *flags*)
             (register ',flag ',names nullable *flags-normalized*))
           ,@(when parser
               `((setf (get ',flag 'parser) ',parser)))
           ,@(when specified-type
               `((setf (get ',flag 'specified-type) ',specified-type)))

           ,@(when def
               `((declaim (type ,type ,flag))
                 (,def ,flag ,default ,doc)))

           (eval-when (:load-toplevel)
             (setf (get ',flag 'setter)
                   ,(or setter
                        `(lambda (,value)
                           (declare (optimize safety))
                           (setf ,flag ,value)))))

           ,@(when (eq (symbol-package flag) +flags-package+)
               ;; All flags in the flags package are external.
               `((eval-when (:compile-toplevel :load-toplevel :execute)
                   (export ',flag +flags-package+))))
           ',flag)))))

(defun* no-prefix-p (name &key (start 0))
  (declare (self (string &key fixnum) boolean))
  "True if flag NAME starts with the 'no' prefix.
 START is the index on the NAME to look for the prefix."
  (and (string:prefixp "no" name :start1 start)
       (> (length name) (+ 2 start))
       (alpha-char-p (char name (+ 2 start)))))

(defun* register (flag names nullablep registry)
  "Registers the FLAG and checks for flag name clashes.
 Arguments:
 FLAG - is the symbol name for the variable,
 NAMES - is a list of strings to be used as a flag name on the command line,
 NULLABLEP - indicates that the type of the flag can accept the NULL value.
 REGISTRY - is the hashtable that maps names to variable symbols."
  (declare (self (symbol list boolean) symbol))
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

(defgeneric print-help (&key stream right-margin indent indent-help parentheses
                        prefix global-flags normalize include-packages
                        exclude-packages documentation type)
  (:documentation
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
 GLOBAL-FLAGS - one of:
         NIL - none (default),
         :external - only external symbols,
         T - all special and global variables.
 NORMALIZE - if non-nil, the names of flags are printed in the normalized form.
             Normalized form uses hyphens and lower-case characters.
 INCLUDE-PACKAGES - a list of package designators to print flags from.
 EXCLUDE-PACKAGES - a list of packages excluded from the PRINT-HELP output.
 DOCUMENTATION - if :REQUIRED (default), only flags with a documentation strings are printed.
 TYPE - only flags with values of the type are printed. Default: (or number string symbol).
 LONG-FORM - if true print each flag and its help on multiple lines."))

(defmethod print-help (&key
                       (stream *standard-output*)
                       (right-margin 80)
                       (indent 4)
                       (indent-help 5)
                       (parentheses "()")
                       (prefix "--")
                       (global-flags nil)
                       (normalize nil)
                       (include-packages nil)
                       (exclude-packages *print-help-exclude-packages*)
                       (documentation :required)
                       (type '(or number string symbol))
                       (long-form nil))
  (declare (stream stream)
           (fixnum right-margin indent indent-help)
           (list include-packages exclude-packages))
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
                   (member (package-name package) include-packages :test #'string=))))
           (add-flag (sym package)
             (when (and sym package (boundp sym) (eq (symbol-package sym) package)
                        (or (type:specialp sym) (type:globalp sym))
                        (not (gethash sym flags->names))
                        (or (not (eq documentation :required))
                            (documentation sym 'variable))
                        (typep (symbol-value sym) type))
               (pushnew (format nil "~(~S~)" sym) (gethash sym flags->names) :test #'equalp))))

      (maphash (lambda (name flag)
                 (when (included-package-p (symbol-package flag))
                   (pushnew name (gethash flag flags->names) :test #'equalp)))
               (if normalize *flags-normalized* *flags*))

      (ecase global-flags
        ((nil))
        (:external
         (dolist (package (list-all-packages))
           (when (included-package-p package)
             (do-external-symbols (sym package)
               (add-flag sym package)))))
        ((t)
         (do-all-symbols (sym)
           (let ((package (symbol-package sym)))
             (when (included-package-p package)
               (add-flag sym package)))))))

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
             (type (or (get flag 'specified-type) (type:declaimed flag)))
             (value (and (boundp flag) (symbol-value flag)))
             (long-form (or long-form (cdr names)))
             (flag-package (symbol-package flag)))
        (unless (eq flag-package last-package)
          (format stream "~:[~;~%~]Flags from ~(~A~):~%"
                  last-package (package-name flag-package))
          (setf last-package flag-package))
        (terpri stream)
        (pprint-logical-block (stream nil :prefix line-prefix)
          (dolist (name names)
            (let ((prefix (if (= 1 (length name)) "-" prefix)))
              (format stream "~@<~@;~A~A~:[~_~;~:@_~]~:>" prefix name long-form)))
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

(defun* command-line ()
  "Returns the full command-line as a list with program name and arguments."
  (declare (self () list))
  #+sbcl sb-ext:*posix-argv*
  #+clisp (coerce (ext:argv) 'list)
  #+abcl ext:*command-line-argument-list*
  #+clozure (ccl::command-line-arguments)
  #+gcl si:*command-args*
  #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
  #+cmu extensions:*command-line-strings*
  #+allegro (sys:command-line-arguments)
  #+lispworks sys:*line-arguments-list*)

;;;
;;; Flag parsing ...
;;;

(deftype global-flags () '(member nil :external t))

(define *global-flags* nil
  "When provided, allows specifying global and special variables as a flag on the command line.
 The values are NIL - for none, :external - for package external, and T - for all flags."
  :type global-flags
  :name "lisp-global-flags")

(define *normalize* nil
  "When non-nil the parsed flags will be transformed into a normalized form.
 The normalized form contains hyphens in place of underscores, trims '*' characters,
 and puts the name into lower case for flags names longer than one character."
  :name "lisp-normalize-flags"
  :type boolean)

(defun* find-variable (name)
  "Search for a variable given a NAME.
 The name is transformed into uppercase.
 A colon is used to derive the package name.
 Then a symbol of a special variable is searched in the package."
  (declare (self (string) symbol))
  (when *global-flags*
    (let* ((name (string-upcase name))
           (pos (position #\: name :from-end t))
           (package-name (if (and pos (plusp pos))
                             (subseq name 0 (if (char= (char name (1- pos)) #\:) (1- pos) pos))
                             (return-from find-variable)))
           (package (find-package package-name))
           (name (subseq name (1+ pos))))
      (flet ((find-global (name)
               (multiple-value-bind (symbol access) (find-symbol name package)
                 (when (and (or (type:specialp symbol) (type:globalp symbol))
                            (ecase *global-flags*
                              (t t)
                              (:external (eq access :external))))
                   symbol))))
        (when package
          (find-global name))))))

(defun* flag-info (arg)
  "Search for a variable and a type corresponding to the flag-name as specified by ARG.
 The flag-name is optionally put into the normalized form (see *NORMALIZE*).
 Returns (values flag-name var type no-p):
  flag-name - the argument with '_' replaced by '-' with optionally removed '--' or '--no' prefix.
  var - the variable symbol.
  no-p - indicator that the flag-name was prefixed with 'no'."
  (declare (self (string) (or null string) symbol boolean))
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
               (variable (or (gethash flag-name flags)
                             (find-variable arg-no-dashes))))
          (values (or flag-name arg-no-dashes) variable no-p))
        (values nil nil nil))))

(defun* boolean-type-p (type)
  "Returns true if the TYPE is a boolean type-specifier."
  (declare (self ((or cons symbol)) boolean))
  (and (subtypep type 'boolean) (subtypep 'boolean type)))

(defun* parse-variable (variable value &key no-p equal-sign-p)
  "Parses a string VALUE accordingly to the type of a VARIABLE.
 NO-P means that the flag had a 'no' prefix.
 EQUAL-SIGN-P means that the flag and value pair have been specified with the '=' sign.
 Returns (values type value parsed-p consume-p)."
  (declare (self (symbol (or null string) &key boolean boolean)
                 (or cons symbol) t boolean boolean))
  (let* ((specified-type (get variable 'specified-type))
         (type (or specified-type (type:declaimed variable)))
         (parser (get variable 'parser)))

    (cond ((and no-p (typep nil type))
           (values type nil t nil))

          (parser
           (multiple-value-bind (result parsed-p) (funcall parser value)
             (values type result (and (or parsed-p result) t) t)))

          ((boolean-type-p type)
           ;; Special handling of pure boolean flags.
           (cond ((or (null value) (string:prefixp "-" value))
                  ;; No value or the next arg is actually a flag.
                  (values type t t nil))
                 ((and equal-sign-p (parse:true-value-string-p value))
                  ;; A value of "true", "yes", "t".
                  (values type t t t))
                 ((and equal-sign-p (parse:false-value-string-p value))
                  ;; A value of "false", "null", "nil", "no".
                  (values type nil t t))
                 (t
                  (values type nil nil nil))))

          (value
           (multiple-value-bind (result parsedp) (parse:type type value)
             (values type result (and parsedp (typep result type)) t)))
          (t
           (values nil nil nil nil)))))

(defun getenv-option (option)
  "True if OPTION is found in the LISP_FLAG_OPTIONS environment variable."
  (let ((options (string:split (os:getenv "LISP_FLAG_OPTIONS") :by " ,")))
    (and (find option options :test #'string-equal) t)))

(defun* parse-command-line (&key (args (command-line))
                            (setp t)
                            (global-flags *global-flags* global-flags-p)
                            (normalize *normalize* normalize-p))
  "Parses the flags taken by default from the program command-line arguments.
 Arguments:
  ARGS - are the program arguments, the first one of which usually being the program name,
  SETP - if true, the variables are set as they are parsed,
  GLOBAL-FLAGS - if true, also look at global and special variables as flags.
  NORMALIZE - if true, the names of arguments are put into a normalized form.
 Returns (values unparsed-args parsed-flag-variables parsed-values)."
  (declare (self (&key list t t t) list list list))
  (with-collectors (parsed-vars parsed-values unparsed)
    (loop with *global-flags* = (if global-flags-p global-flags (getenv-option "globals"))
          with *normalize* = (if normalize-p normalize (getenv-option "normalize"))
          with args = args
          for arg = (pop args)
          while arg do
            (let* ((pos= (position #\= arg)) ; Support the --flag=value syntax.
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
