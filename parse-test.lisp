;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Tests for the numbers package.
;;;

(defpackage #:ace.flag.parse-test
  (:use #:common-lisp
        #:ace.test)
  (:local-nicknames (#:parse #:ace.flag.parse))
  (:import-from #:ace.core.number
                #:double-float-not-a-number))

(in-package #:ace.flag.parse-test)

(defun test-parse-type (type string expected)
  (multiple-value-bind (actual parsedp) (parse:type type string)
    (expect (equal expected actual)
            "While parsing ~S as ~S, the actual value ~A differs from expected ~A"
            string type actual expected)

    (expect (equalp (type-of expected) (type-of actual))
           "While parsing ~S as ~S actual type ~A differs from expected ~A"
           string type (type-of expected) (type-of actual))

    (expect parsedp "Expected to parse ~S as ~S to ~A" string type expected)))

(defun test-not-parsed (type string)
  (multiple-value-bind (actual parsedp) (parse:type type string)
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

(deftest test-parse-type-null ()
  (test-parse-type 'null "false" nil)
  (test-parse-type 'null "null" nil)
  (test-parse-type 'null "nil" nil)
  (test-parse-type 'null "no" nil)

  (test-parse-type 'null "False" nil)
  (test-parse-type 'null "Null" nil)
  (test-parse-type 'null "NIL" nil)
  (test-parse-type 'null "NO" nil)

  (test-not-parsed 'null "true")
  (test-not-parsed 'null "YES")
  (test-not-parsed 'null "What-ever"))

(deftest test-parse-type-keyword ()
  (test-parse-type 'keyword ":keyword" :keyword)
  (test-parse-type 'keyword "true" :true)
  (test-parse-type 'keyword "False" :false)
  (test-parse-type 'keyword "1" :1)
  (test-parse-type 'keyword "-1" :-1)
  (test-parse-type 'keyword "nil" :nil)
  (test-parse-type 'keyword "t" :t))

(deftest test-parse-type-symbol ()
  (test-parse-type 'symbol "ace.flag.parse-test:test-parse-type" 'test-parse-type)
  (test-parse-type 'symbol "ace.flag.parse-test::test-parse-type" 'test-parse-type)
  (test-parse-type 'symbol ":test-parse-type" :test-parse-type)

  (test-not-parsed 'symbol "test:::test-parse-type")
  (test-not-parsed 'symbol "test-parse-type")
  (test-not-parsed 'symbol "::test-parse-type")
  (test-not-parsed 'symbol ":::test-parse-type")
  (test-not-parsed 'symbol "t")
  (test-not-parsed 'symbol "nil")
  (test-not-parsed 'symbol "0"))

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


(deftest test-parse-type-number ()
  (test-parse-type 'number "0" 0)
  (test-parse-type 'number "0000" 0)
  (test-parse-type 'number "0001" 1)
  (test-parse-type 'number "1" 1)
  (test-parse-type 'number "-1" -1)
  (test-parse-type 'number "1.0" 1.0d0)
  (test-parse-type 'number "0xFF" #xFF)
  (test-parse-type 'number "-0xFF" #x-FF)

  (test-not-parsed 'number "0-1")
  (test-not-parsed 'number "t")
  (test-not-parsed 'number "FF")
  (test-not-parsed 'number "")
  (test-not-parsed 'number ":100"))

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


(deftest test-parse-type-mod ()
  (test-parse-type '(mod 500) "0" 0)
  (test-parse-type '(mod 500) "0000" 0)
  (test-parse-type '(mod 500) "0001" 1)
  (test-parse-type '(mod 500) "+0" 0)
  (test-parse-type '(mod 500) "+0000" 0)
  (test-parse-type '(mod 500) "+0001" 1)
  (test-parse-type '(mod 500) "-0" 0)
  (test-parse-type '(mod 500) "-0000" 0)

  (test-parse-type '(mod 500) "0." 0)
  (test-parse-type '(mod 500) "1." 1)
  (test-parse-type '(mod 500) "1" 1)
  (test-parse-type '(mod 500) "0xFF" #xFF)

  (test-not-parsed '(mod 500) "-1")
  (test-not-parsed '(mod 500) "-0xFF")
  (test-not-parsed '(mod 500) "-0001")
  (test-not-parsed '(mod 500) "0x1FFFFFFFFFFFFFFFF")
  (test-not-parsed '(mod 500) "0e0")
  (test-not-parsed '(mod 500) "0.0")
  (test-not-parsed '(mod 500) ".0")
  (test-not-parsed '(mod 500) ".1")
  (test-not-parsed '(mod 500) "1.0")
  (test-not-parsed '(mod 500) "0-1")
  (test-not-parsed '(mod 500) "t")
  (test-not-parsed '(mod 500) "FF")
  (test-not-parsed '(mod 500) "")
  (test-not-parsed '(mod 500) ":100"))


(deftest test-parse-type-signed-byte ()
  (test-parse-type 'signed-byte "0" 0)
  (test-parse-type 'signed-byte "0000" 0)
  (test-parse-type 'signed-byte "0001" 1)
  (test-parse-type 'signed-byte "+0" 0)
  (test-parse-type 'signed-byte "+0000" 0)
  (test-parse-type 'signed-byte "+0001" 1)
  (test-parse-type 'signed-byte "-0" 0)
  (test-parse-type 'signed-byte "-0000" 0)
  (test-parse-type 'signed-byte "-0001" -1)

  (test-parse-type 'signed-byte "0." 0)
  (test-parse-type 'signed-byte "1." 1)
  (test-parse-type 'signed-byte "1" 1)
  (test-parse-type 'signed-byte "-1" -1)
  (test-parse-type 'signed-byte "0xFF" #xFF)
  (test-parse-type 'signed-byte "-0xFF" #x-FF)

  (test-not-parsed 'signed-byte "0e0")
  (test-not-parsed 'signed-byte "0.0")
  (test-not-parsed 'signed-byte ".0")
  (test-not-parsed 'signed-byte ".1")
  (test-not-parsed 'signed-byte "1.0")
  (test-not-parsed 'signed-byte "0-1")
  (test-not-parsed 'signed-byte "t")
  (test-not-parsed 'signed-byte "FF")
  (test-not-parsed 'signed-byte "")
  (test-not-parsed 'signed-byte ":100"))

(deftest test-parse-type-unsinged-byte ()
  (test-parse-type 'unsigned-byte "0" 0)
  (test-parse-type 'unsigned-byte "0000" 0)
  (test-parse-type 'unsigned-byte "0001" 1)
  (test-parse-type 'unsigned-byte "+0" 0)
  (test-parse-type 'unsigned-byte "+0000" 0)
  (test-parse-type 'unsigned-byte "+0001" 1)
  (test-parse-type 'unsigned-byte "-0" 0)
  (test-parse-type 'unsigned-byte "-0000" 0)

  (test-parse-type 'unsigned-byte "0." 0)
  (test-parse-type 'unsigned-byte "1." 1)
  (test-parse-type 'unsigned-byte "1" 1)
  (test-parse-type 'unsigned-byte "0xFF" #xFF)
  (test-parse-type 'unsigned-byte "0x1FFFFFFFFFFFFFFFF" #x1FFFFFFFFFFFFFFFF)

  (test-not-parsed 'unsigned-byte "-1")
  (test-not-parsed 'unsigned-byte "-0xFF")
  (test-not-parsed 'unsigned-byte "-0001")
  (test-not-parsed 'unsigned-byte "0e0")
  (test-not-parsed 'unsigned-byte "0.0")
  (test-not-parsed 'unsigned-byte ".0")
  (test-not-parsed 'unsigned-byte ".1")
  (test-not-parsed 'unsigned-byte "1.0")
  (test-not-parsed 'unsigned-byte "0-1")
  (test-not-parsed 'unsigned-byte "t")
  (test-not-parsed 'unsigned-byte "FF")
  (test-not-parsed 'unsigned-byte "")
  (test-not-parsed 'unsigned-byte ":100"))

(deftest test-parse-type-or ()
  (test-parse-type '(or number boolean) "true" t)
  (test-parse-type '(or boolean keyword) "yes" t)
  (test-parse-type '(or single-float boolean) "t" t)

  (test-parse-type '(or integer boolean) "True" t)
  (test-parse-type '(or fixnum boolean) "YES" t)
  (test-parse-type '(or null boolean) "T" t)

  (test-parse-type '(or number string) "True" "True")
  (test-parse-type '(or number keyword) "YES" :YES)

  (test-parse-type '(or signed-byte boolean) "false" nil)
  (test-parse-type '(or unsigned-byte boolean) "null" nil)
  (test-parse-type '(or boolean boolean) "nil" nil)
  (test-parse-type '(or null boolean) "no" nil)

  (test-parse-type '(or null double-float) "nan" double-float-not-a-number)

  (test-parse-type '(or (mod 100) boolean) "False" nil)
  (test-parse-type '(or boolean keyword) "Null" nil)
  (test-parse-type '(or number boolean) "NIL" nil)
  (test-parse-type '(or symbol boolean) "NO" nil))

(deftest test-parse-type-and ()
  (test-parse-type '(and unsigned-byte fixnum) "10" 10)
  (test-not-parsed '(and unsigned-byte fixnum) "-10")
  (test-not-parsed '(and unsigned-byte fixnum) "0x1FFFFFFFFFFFFFFFF")

  (test-parse-type '(and boolean symbol) "yes" t)
  (test-parse-type '(and symbol boolean) "t" t)

  (test-parse-type '(and null boolean) "nil" nil)

  ;; Test backtracing ...
  (test-parse-type '(and (or number string) (or string keyword)) "false" "false")
  (test-parse-type '(and (or number string) (or keyword string)) "false" "false")
  (test-parse-type '(and (or string number) (or string keyword)) "false" "false")
  (test-parse-type '(and (or string number) (or keyword string)) "false" "false")


  (test-parse-type '(and (or (and number integer) string)
                     (or string (and unsigned-byte fixnum))) "0xFF" #xFF)
  (test-parse-type '(and (or (and number integer) string)
                     (or (and unsigned-byte fixnum) string)) "0xFF" #xFF)
  (test-parse-type '(and (or string number) (or keyword fixnum)) "0xFF" #xFF)
  (test-parse-type '(and (or string number) (or fixnum keyword)) "0xFF" #xFF)

  (test-parse-type '(and (or number null) boolean) "false" nil))
