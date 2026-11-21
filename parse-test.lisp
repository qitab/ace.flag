;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Tests for command-line flag value parsing.
;;;

(defpackage #:ace.flag.parse-test
  (:use #:common-lisp
        #:ace.test)
  (:import-from #:ace.core.number
                #:double-float-not-a-number))

(in-package #:ace.flag.parse-test)

(defun parse-type (x y) (ace.flag::parse-as-type x y))

(defun test-parse-type (type string expected)
  (multiple-value-bind (actual parsedp) (parse-type type string)
    (expect (equal expected actual)
            "While parsing ~S as ~S, the actual value ~A differs from expected ~A"
            string type actual expected)

    (expect (equalp (type-of expected) (type-of actual))
           "While parsing ~S as ~S actual type ~A differs from expected ~A"
           string type (type-of expected) (type-of actual))

    (expect parsedp "Expected to parse ~S as ~S to ~A" string type expected)))

(defun test-not-parsed (type string)
  (multiple-value-bind (actual parsedp) (parse-type type string)
    (declare (ignore actual))
    (expect (not parsedp) "Not expected to parse ~S as ~S" string type)))

(deftest test-parse-type-boolean ()
  (test-parse-type 'boolean "true" t)
  (test-parse-type 'boolean "yes" t)
  (test-parse-type 'boolean "t" t)

  (test-parse-type 'boolean "True" t)
  (test-parse-type 'boolean "YES" t)
  (test-parse-type 'boolean "T" t)

  (test-parse-type 'boolean "false" nil)
  (test-parse-type 'boolean "null" nil)
  (test-parse-type 'boolean "nil" nil)
  (test-parse-type 'boolean "no" nil)

  (test-parse-type 'boolean "False" nil)
  (test-parse-type 'boolean "Null" nil)
  (test-parse-type 'boolean "NIL" nil)
  (test-parse-type 'boolean "NO" nil)

  (test-not-parsed 'boolean "+")
  (test-not-parsed 'boolean "-")
  (test-not-parsed 'boolean "1")
  (test-not-parsed 'boolean "0")
  (test-not-parsed 'boolean "What-Ever"))

(deftest test-parse-type-string ()
  (test-parse-type 'string "" "")
  (test-parse-type 'string "A" "A")
  (test-parse-type 'string "a" "a")

  (test-parse-type 'string "0" "0")
  (test-parse-type 'string "-1" "-1")
  (test-parse-type 'string "+1.0" "+1.0")
  (test-parse-type 'string ":key" ":key")

  (test-parse-type 'string "ABC" "ABC")
  (test-parse-type 'string "Abc" "Abc")
  (test-parse-type 'string "abc" "abc")

  (test-parse-type 'string "ABC DEF" "ABC DEF")
  (test-parse-type 'string "Abc Def" "Abc Def")
  (test-parse-type 'string "abc def" "abc def"))

(deftest test-parse-type-float ()
  (test-parse-type 'single-float "0" 0.0)
  (test-parse-type 'single-float "-0" 0.0)

  (test-parse-type 'single-float "0." 0.0)
  (test-parse-type 'single-float "1." 1.0)
  (test-parse-type 'single-float ".0" 0.0)
  (test-parse-type 'single-float ".1" .1)

  (test-parse-type 'single-float "0000" 0.0)
  (test-parse-type 'single-float "0001" 1.0)
  (test-parse-type 'single-float "1" 1.0)
  (test-parse-type 'single-float "-1" -1.0)
  (test-parse-type 'single-float "1.0" 1.0)
  (test-parse-type 'single-float "0xFF" 255.0)
  (test-parse-type 'single-float "-0xFF" -255.0))

(deftest test-parse-type-double ()
  (test-parse-type 'double-float "0" 0.0d0)
  (test-parse-type 'double-float "-0" 0.0d0)

  (test-parse-type 'double-float "0." 0.0d0)
  (test-parse-type 'double-float "1." 1.0d0)

  (test-parse-type 'double-float "0000" 0.0d0)
  (test-parse-type 'double-float "0001" 1.0d0)
  (test-parse-type 'double-float "1" 1.0d0)
  (test-parse-type 'double-float "-1" -1.0d0)
  (test-parse-type 'double-float "1.0" 1.0d0)
  (test-parse-type 'double-float "0xFF" 255.0d0)
  (test-parse-type 'double-float "-0xFF" -255.0d0))

(deftest test-parse-type-integer ()
  (test-parse-type 'integer "0" 0)
  (test-parse-type 'integer "0000" 0)
  (test-parse-type 'integer "0001" 1)
  (test-parse-type 'integer "+0" 0)
  (test-parse-type 'integer "+0000" 0)
  (test-parse-type 'integer "+0001" 1)
  (test-parse-type 'integer "-0" 0)
  (test-parse-type 'integer "-0000" 0)
  (test-parse-type 'integer "-0001" -1)

  (test-parse-type 'integer "0." 0)
  (test-parse-type 'integer "1." 1)
  (test-parse-type 'integer "1" 1)
  (test-parse-type 'integer "-1" -1)
  (test-parse-type 'integer "0xFF" #xFF)
  (test-parse-type 'integer "-0xFF" #x-FF)

  (test-not-parsed 'integer "0e0")
  (test-not-parsed 'integer "0.0")
  (test-not-parsed 'integer ".0")
  (test-not-parsed 'integer ".1")
  (test-not-parsed 'integer "1.0")
  (test-not-parsed 'integer "0-1")
  (test-not-parsed 'integer "t")
  (test-not-parsed 'integer "FF")
  (test-not-parsed 'integer "")
  (test-not-parsed 'integer ":100"))

(deftest test-parse-type-fixnum ()
  (test-parse-type 'fixnum "0" 0)
  (test-parse-type 'fixnum "0000" 0)
  (test-parse-type 'fixnum "0001" 1)
  (test-parse-type 'fixnum "+0" 0)
  (test-parse-type 'fixnum "+0000" 0)
  (test-parse-type 'fixnum "+0001" 1)
  (test-parse-type 'fixnum "-0" 0)
  (test-parse-type 'fixnum "-0000" 0)
  (test-parse-type 'fixnum "-0001" -1)

  (test-parse-type 'fixnum "0." 0)
  (test-parse-type 'fixnum "1." 1)
  (test-parse-type 'fixnum "1" 1)
  (test-parse-type 'fixnum "-1" -1)
  (test-parse-type 'fixnum "0xFF" #xFF)
  (test-parse-type 'fixnum "-0xFF" #x-FF)

  (test-not-parsed 'fixnum "0x1FFFFFFFFFFFFFFFF")
  (test-not-parsed 'fixnum "0e0")
  (test-not-parsed 'fixnum "0.0")
  (test-not-parsed 'fixnum ".0")
  (test-not-parsed 'fixnum ".1")
  (test-not-parsed 'fixnum "1.0")
  (test-not-parsed 'fixnum "0-1")
  (test-not-parsed 'fixnum "t")
  (test-not-parsed 'fixnum "FF")
  (test-not-parsed 'fixnum "")
  (test-not-parsed 'fixnum ":100"))

(deftest test-parse-type-or ()
  ;; Both (or null T) and (or T null) must behave identically.
  (test-parse-type '(or null string) "abc" "abc")
  (test-parse-type '(or string null) "abc" "abc")
  (test-parse-type '(or null string) "no" "no")
  (test-parse-type '(or string null) "no" "no")

  (test-parse-type '(or null integer) "42" 42)
  (test-parse-type '(or integer null) "42" 42)
  (test-parse-type '(or null integer) "nil" nil)
  (test-parse-type '(or integer null) "null" nil)

  (test-parse-type '(or null fixnum) "-7" -7)
  (test-parse-type '(or fixnum null) "false" nil)

  (test-parse-type '(or null single-float) "1.5" 1.5)
  (test-parse-type '(or single-float null) "no" nil)

  (test-parse-type '(or null double-float) "nan" double-float-not-a-number)
  (test-parse-type '(or double-float null) "nil" nil)

  (test-parse-type '(or null (member :a :b)) "a" :a)
  (test-parse-type '(or (member :a :b) null) "nil" nil)

  ;; Non-nullable or multi-arm unions are rejected.
  (expect-error (parse-type '(or single-float double-float) "1.0"))
  (expect-error (parse-type '(or single-float integer) "1"))
  (expect-error (parse-type '(or integer string) "abc"))
  (expect-error (parse-type '(or null integer string) "1"))
  (expect-error (parse-type 'null "nil"))
  (expect-error (parse-type 'float "1.0"))
  (expect-error (parse-type 'real "1"))
  (expect-error (parse-type 'number "1")))
