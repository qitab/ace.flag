;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:ace.flag.parse
  (:shadow #:type)
  (:use #:common-lisp
        #:ace.core.number)
  (:import-from #:ace.core.type #:expand)
  (:export ;#:type
           #:true-value-string-p
           #:false-value-string-p))

(in-package #:ace.flag.parse)

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

(sb-ext:defglobal acceptable-flag-types '(boolean string integer fixnum real single-float double-float))
    
(defun type-selector (type-specifier)
  "Returns the type specifier which must be one of the supported specifiers"
  (declare (optimize (safety 1) (speed 1)))
  (the (member . #.acceptable-flag-types) type-specifier))

;;;
;; ace.flag.parse:type parses a string according to a type-specifier.
;;;

(defgeneric type (type-selector value &key &allow-other-keys) ; NOLINT
  (:documentation
"Generic function that parses a string VALUE of the given type.
The TYPE-SELECTOR is the type-specifier itself or the car of the type-specifier."))

(defmethod type (type-selector (value string) &key specifier)
  "A default method to parse a type.
 Tries to read the VALUE using Lisp reader. Numbers are favored. Then keywords.
 TYPE-SELECTOR is usually the first atom in the type specifier.
 SPECIFIER is the full type specifier."
  (error "WTF")
  (cond ((null type-selector)
         (values nil nil))

        ((null specifier)
         (let* ((specifier type-selector))
           (multiple-value-bind (parsed-value parsed-p)
               (type (type-selector specifier) value :specifier specifier)
             (if parsed-p
                 (values parsed-value t)
                 (let ((expanded (expand specifier)))
                   (unless (equalp specifier expanded)
                     (type (type-selector expanded) value :specifier expanded)))))))

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


(defmethod type ((type-selector (eql 'boolean)) value &key)
  ; Parses a boolean value.
  (cond ((true-value-string-p value)  (values t t))
        ((false-value-string-p value) (values nil t))
        (t
         (values nil nil))))

(defmethod type ((type-selector (eql 'string)) (value string) &key)
  (values value t))

(defmethod type ((type-selector (eql 'real)) (value string) &key)
  (let ((result (read-number-from-string value)))
    (values result (and result t))))

(defmethod type ((type-selector (eql 'single-float)) (value string) &key)
  (let* ((number (read-number-from-string value))
         (result (and (numberp number) (coerce number 'single-float))))
    (values result (and result t))))

(defmethod type ((type-selector (eql 'double-float)) (value string) &key)
  (let* ((number (read-number-from-string value))
         (result (and (numberp number) (coerce number 'double-float))))
    (values result (and result t))))

(defmethod type ((type-selector (eql 'integer)) (value string) &key (specifier nil spec-p))
  (when spec-p
    (error "srsly WTF"))
  (let* ((number (read-number-from-string value))
         (result (and (integerp number) number)))
    (values result (typep result (or specifier type-selector)))))

(defmethod type ((type-selector (eql 'fixnum)) (value string) &key)
  (let* ((number (read-number-from-string value))
         (result (and (typep number 'fixnum) number)))
    (values result (and result t))))
