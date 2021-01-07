;;; Generic utility for parsing complex Lisp types from text.
;;;
;;; type-expand - will take a type specifier and return an expanded canonical form.
;;; variable-declared-type - will take a variable and return a type-specifier for it.
;;; parse:type
;;;

(ace.core.package:defpackage* #:ace.flag.parse
  (:shadow #:type)
  (:use #:common-lisp
        #:ace.core.number)
  (:use-alias (#:parse #:ace.flag.parse))
  (:import-from #:ace.core.type #:expand)
  (:export #:type
           #:true-value-string-p
           #:false-value-string-p))

(in-package #:ace.flag.parse)

(defun true-value-string-p (value)
  "Tests that the string VALUE is one of the designators for a true boolean value."
  (declare (string value) (values boolean &optional))
  (and (member value '("true" "yes" "t") :test #'equalp) t))

(defun false-value-string-p (value)
  "Tests that the string VALUE is one of the designators for a false boolean value."
  (declare (string value) (values boolean &optional))
  (and (member value '("false" "null" "nil" "no") :test #'equalp) t))

(defun type-selector (type-specifier)
  "Returns the type selector for a type.
 Returns the TYPE-SPECIFIER itself if it is an atom.
 Returns the first element of the TYPE-SPECIFIER if it is list."
  (if (consp type-specifier) (first type-specifier) type-specifier))

;;;
;; parse:type allows to parse a string given a more complicated type-specifier.
;;;

(defgeneric parse:type (type-selector value &key &allow-other-keys) ; NOLINT
  (:documentation
"Generic function that parses a string VALUE of the given type.
The TYPE-SELECTOR is the type-specifier itself or the car of the type-specifier."))

(defmethod parse:type (type-selector (value string) &key specifier)
  "A default method to parse a type.
 Tries to read the VALUE using Lisp reader. Numbers are favored. Then keywords.
 TYPE-SELECTOR is usually the first atom in the type specifier.
 SPECIFIER is the full type specifier."
  (cond ((null type-selector)
         (values nil nil))

        ((null specifier)
         (let* ((specifier type-selector))
           (multiple-value-bind (parsed-value parsed-p)
               (parse:type (type-selector specifier) value :specifier specifier)
             (if parsed-p
                 (values parsed-value t)
                 (let ((expanded (expand specifier)))
                   (unless (equalp specifier expanded)
                     (parse:type (type-selector expanded) value :specifier expanded)))))))

        (t
         ;; Default parser tries numbers first, then read-from-string, falls back to string.
         (with-standard-io-syntax
           (let* ((number (read-number-from-string value))
                  (result
                    (if (and number (typep number specifier))
                        number
                        (let ((*package* (find-package "KEYWORD"))
                              (thing (ignore-errors (read-from-string value nil))))
                          (cond ((and thing (typep thing specifier))
                                 thing)
                                ((typep value specifier) ; use the string value
                                 value))))))
             (values result (and (or result (false-value-string-p value)) t)))))))


(defmethod parse:type ((type-selector (eql 'boolean)) value &key)
  ; Parses a boolean value.
  (cond ((true-value-string-p value)  (values t t))
        ((false-value-string-p value) (values nil t))
        ;; Flags starting with --no are handled on a different level.
        ;; Flags with value T default to true.
        ((eq value t)          (values t t))
        (t
         (values nil nil))))

(defmethod parse:type ((type-selector (eql 'or)) (value string) &key specifier top)
  ; Parses an or type. Iterates through the subtypes.
  (assert specifier)
  (dolist (sub-type (rest specifier))
    (multiple-value-bind (parsed-value parsed-p)
        (parse:type (type-selector sub-type) value :specifier sub-type :top top)
      (when (and parsed-p (or (null top) (typep parsed-value top)))
        (return (values parsed-value t))))))

(defmethod parse:type ((type-selector (eql 'and)) (value string) &key specifier top)
  ; Parses an and type. Iterates through the subtypes.
  (assert specifier)
  (dolist (sub-type (rest specifier))
    (multiple-value-bind (parsed-value parsed-p)
        (parse:type (type-selector sub-type) value :specifier sub-type :top (or top specifier))
      (when (and parsed-p (typep parsed-value (or top specifier)))
        (return (values parsed-value t))))))

(defmethod parse:type ((type-selector (eql 'member)) (value string) &key specifier top)
  ; Parses a member specifier. Iterates through members. Compares by equalp.
  (assert specifier)
  (let ((members (rest specifier)))
    (if (or (equal members '(t nil)) (equal members '(nil t)))
        (parse:type 'boolean value :specifier 'boolean)
        ;; else
        (let ((number (read-number-from-string value)))
          (dolist (member members)
            (when (or (null top) (typep member top))
              (typecase member
                (null   (when (false-value-string-p value)
                          (return (values nil t))))
                (symbol (when (equalp (symbol-name member) value)
                          (return (values member t))))
                (number (when (eql member number)
                          (return (values member t)))))))))))

(defmethod parse:type ((type-selector (eql 'null)) (value string) &key)
  ; Parses a false value.
  (values nil (false-value-string-p value)))

(defmethod parse:type ((type-selector (eql 'string)) (value string) &key)
  ; Returns the value as string.
  (values value (and value t)))

(defmethod parse:type ((type-selector (eql 'base-string)) (value string) &key)
  ; Returns the value as string.
  (values value (and value t)))

(defmethod parse:type ((type-selector (eql 'vector)) (value string) &key specifier)
  ; Returns the value as string.
  (when (typep value (or specifier type-selector))
    (values value (and value t))))

(defmethod parse:type ((type-selector (eql 'simple-array)) (value string) &key specifier)
  ; Returns the value as string.
  (when (typep value (or specifier type-selector))
    (values value (and value t))))

(defmethod parse:type ((type-selector (eql 'keyword)) (value string) &key)
  ; Interns the value into the keyword package.
  (let ((colon (position #\: value :test #'char=)))
    (cond ((null colon)
           (values (intern (string-upcase value) (find-package "KEYWORD")) t))
          ((= colon 0)
           (unless (find #\: value :test #'char= :start 1)
             (parse:type 'keyword (subseq value 1))))
          (t
           (values nil nil)))))

(defmethod parse:type ((type-selector (eql 'symbol)) (value string) &key)
  ; Parses a symbol that is prefixed with the package.
  (let* ((full-name (string-trim " " (string-upcase value)))
         (pos (position #\: full-name :from-end t))
         (package-name
           (when (and pos (plusp pos))
             (subseq full-name 0 (if (char= (char full-name (1- pos)) #\:) (1- pos) pos))))
         (package (and package-name (find-package package-name)))
         (symbol-name (and pos (subseq full-name (1+ pos))))
         (symbol (cond (package
                        (find-symbol symbol-name package))
                       ((eql pos 0)
                        (find-symbol symbol-name (find-package "KEYWORD"))))))
    (values symbol (and symbol t))))

(defmethod parse:type ((type-selector (eql 'number)) (value string) &key)
  ; Parses a number.
  (let ((result (read-number-from-string value)))
    (values result (and result t))))

(defmethod parse:type ((type-selector (eql 'single-float)) (value string) &key)
  ; Parses a number.
  (let* ((number (read-number-from-string value))
         (result (and (numberp number) (coerce number 'single-float))))
    (values result (and result t))))

(defmethod parse:type ((type-selector (eql 'double-float)) (value string) &key)
  ; Parses a number.
  (let* ((number (read-number-from-string value))
         (result (and (numberp number) (coerce number 'double-float))))
    (values result (and result t))))

(defmethod parse:type ((type-selector (eql 'integer)) (value string) &key specifier)
  ; Parses a number.
  (let* ((number (read-number-from-string value))
         (result (and (integerp number) number)))
    (values result (typep result (or specifier type-selector)))))

(defmethod parse:type ((type-selector (eql 'fixnum)) (value string) &key)
  ; Parses a fixnum number.
  (let* ((number (read-number-from-string value))
         (result (and (typep number 'fixnum) number)))
    (values result (and result t))))

(defmethod parse:type ((type-selector (eql 'mod)) (value string) &key specifier)
  ; Parses a number.
  (parse:type 'integer value :specifier specifier))

(defmethod parse:type ((type-selector (eql 'signed-byte)) (value string) &key specifier)
  ; Parses a number.
  (parse:type 'integer value :specifier specifier))

(defmethod parse:type ((type-selector (eql 'unsigned-byte)) (value string) &key specifier)
  ; Parses a number.
  (let* ((number (read-number-from-string value :unsigned-p t))
         (result (and (typep number 'unsigned-byte) number)))
    (values result (typep result (or specifier type-selector)))))
