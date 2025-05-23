;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; Test the ace.flag package.
;;;

(defpackage #:ace.flag-test
  (:documentation "Test command line flag parsing code in the ACE.FLAG package.")
  (:local-nicknames (#:number #:ace.core.number)
                    (#:string #:ace.core.string)
                    (#:flag #:ace.flag))
  (:use #:common-lisp #:ace.test)
  (:import-from #:ace.flag
                #:parse-variable
                #:*global-flags*)
  (:import-from #:ace.core.etc #:define-constant))


(in-package #:ace.flag-test)

;;; All the tests except one expect that NAME gets defined via DEFPARAMETER,
;;; whereas by default the flag can't be LET-bound.
(defmacro test-define (name val doc &rest rest)
  `(flag:define ,name ,val ,doc :def defparameter ,@rest))

(test-define *boolean* t "Test boolean flag.")

(test-define *boolean2* nil "Test boolean flag." :type boolean)

(test-define *true* t "Test boolean flag.")
(test-define *false* nil "Test boolean flag." :type boolean)

(test-define *null* nil "Always null" :type null)

(test-define *keyword* nil "A keyword flag" :type (or null keyword))

(test-define *member* :a "A member flag" :type (member nil :a :b :c))

(test-define *implicit-keyword* :a "An implicit keyword flag.")

(test-define *implicit-string* "string" "An implicit string flag.")

(test-define *string* nil "A string flag." :type (or null string))

(test-define *FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF*
    "very-long-flag" "This is a very long flag.")

(test-define *very-long-doc* "doc"
  "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")

(test-define *lorem-ipsum* "doc"
  "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident,
sunt in culpa qui officia deserunt mollit anim id est laborum."
  :type (or null (member :lorem :ipsum :dolor) string))

(test-define *print-help-and-newlines* 10
  "This
doc has embedded
   newlines and extra spaces.")

(test-define *implicit-integer* 10 "")

(test-define *integer* 10 "An integer flag" :type (or (integer -100 100) null))

(test-define *number* 10 "A number flag." :type number)

(test-define *float* 10.0 "A float flag" :type single-float)

(test-define *double* 10.0d0 "A double flag" :type double-float)
(test-define *double2* nil "A double flag" :type (or double-float null))
(test-define *double3* nil "A double flag" :type (or double-float null))

(test-define *mod* 10 "Modulo 10" :type (mod 16))

(test-define *plus* 10 "Positive fixnum" :type (and fixnum unsigned-byte))

(test-define *some-flag* nil "A Flag that accepts inf, -inf, nan, or number."
  :type (or (member nil :inf :-inf :nan) number))

(test-define naked-flag nil "" :type boolean)

(test-define flags::a-flag 0 "This is a flag in the flags package." :type fixnum)

(test-define :b-flag 0 "This is a flag in the flags package." :type fixnum)

(test-define "C-FLAG" 0 "This is a flag in the flags package." :type fixnum)

(declaim (fixnum *parameter*))
(defparameter *parameter* 10)

(test-define *named-flag* 20 "A named flag." :name "a-named-flag")

(test-define *named-flag2* 20 "A named flag." :names ("a-named-flag2" "named-flag2"))

(defun parse-hex (x) (parse-integer x :radix 16))

(test-define *parsed-flag* 0 "A flag in hex." :parser parse-hex)

(deftype foo () 'fixnum)

(defmethod ace.flag.parse:type ((type (eql 'foo)) value &key)
  ;; Test default parsers for types.
  (values (parse-integer value :radix 16) t))

(test-define *foo* 0 "A foo flag" :type foo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *print-help-output*
  "Flags from ace.flag-test:

  -B
  --boo
     [Short test boolean flag.]
     Type: BOOLEAN
     Value: T

  --boolean [Test boolean flag.] Type: BOOLEAN Value: T

  --boolflag [Boolean flag used for testing.] Type: BOOLEAN

  --boolean2 [Test boolean flag.] Type: BOOLEAN

  --color [no doc] Type: ACE.FLAG-TEST::COLOR Value: :RED

  --double [A double flag] Type: DOUBLE-FLOAT Value: 10.0d0

  --dfflag [no doc] Type: (DOUBLE-FLOAT -1.0d0 1.0d0) Value: 0.12345d0

  --double2 [A double flag] Type: (OR DOUBLE-FLOAT NULL)

  --double3 [A double flag] Type: (OR DOUBLE-FLOAT NULL)

  --false [Test boolean flag.] Type: BOOLEAN

  --fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
     [This is a very long flag.]
     Type: STRING
     Value: \"very-long-flag\"

  --flag-with-hyphens [A flag with hyphens.] Type: SINGLE-FLOAT Value: 30.0

  --flag_with_underscores [A flag with underscores.]
     Type: SINGLE-FLOAT
     Value: 10.0

  -f
  --sfloat
     [A short float flag]
     Type: SINGLE-FLOAT
     Value: 10.0

  --float [A float flag] Type: SINGLE-FLOAT Value: 10.0

  --foo [A foo flag] Type: ACE.FLAG-TEST::FOO Value: 0

  --implicit-integer Type: INTEGER Value: 10

  --implicit-keyword [An implicit keyword flag.] Type: KEYWORD Value: :A

  --implicit-string [An implicit string flag.] Type: STRING Value: \"string\"

  -i
  --int
     [An short int flag]
     Type: (OR (INTEGER -100 100) NULL)
     Value: 10

  -I
  --sint
     [An short int flag]
     Type: (OR (INTEGER -100 100) NULL)
     Value: 20

  --integer [An integer flag] Type: (OR (INTEGER -100 100) NULL) Value: 10

  --intflag [no doc] Type: (INTEGER -10 10) Value: 10

  -K
  --key
     [Short keyword flag]
     Type: (OR NULL KEYWORD)

  --keyword [A keyword flag] Type: (OR NULL KEYWORD)

  --keyflag [no doc] Type: KEYWORD Value: :FOO

  --lorem-ipsum
     [Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod
      tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
      quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo
      consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
      dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident,
      sunt in culpa qui officia deserunt mollit anim id est laborum.]
     Type: (OR NULL (MEMBER :LOREM :IPSUM :DOLOR) STRING)
     Value: \"doc\"

  -M
  --mem
     [Short member flag]
     Type: (MEMBER NIL :A :B :C)
     Value: :A

  --member [A member flag] Type: (MEMBER NIL :A :B :C) Value: :A

  --mod [Modulo 10] Type: (MOD 16) Value: 10

  --naked-flag Type: BOOLEAN

  --a-named-flag [A named flag.] Type: INTEGER Value: 20

  --a-named-flag2
  --named-flag2
     [A named flag.]
     Type: INTEGER
     Value: 20

  --nogo [no doc] Type: BOOLEAN

  --null [Always null] Type: NULL

  --number [A number flag.] Type: NUMBER Value: 10

  --parsed-flag [A flag in hex.] Type: INTEGER Value: 0

  --plus [Positive fixnum] Type: (AND FIXNUM UNSIGNED-BYTE) Value: 10

  --print-help-and-newlines
     [This
      doc has embedded
         newlines and extra spaces.]
     Type: INTEGER
     Value: 10

  --rth [no doc] Type: BOOLEAN

  --sfflag [no doc] Type: SINGLE-FLOAT Value: 3.14

  --some-flag [A Flag that accepts inf, -inf, nan, or number.]
     Type: (OR (MEMBER NIL :INF :-INF :NAN) NUMBER)

  -S
  --str
     [A double flag]
     Type: STRING
     Value: \"\"

  --string [A string flag.] Type: (OR NULL STRING)

  --stringflag [no doc] Type: STRING Value: \"foo\"

  --symflag [no doc] Type: SYMBOL Value: :BAR

  --symbol-macro-flag Type: BOOLEAN

  --true [Test boolean flag.] Type: BOOLEAN Value: T

  --very-long-doc
     [FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF]
     Type: STRING
     Value: \"doc\"
")

(deftest test-print-help ()
  (loop
    :with help
      = (with-output-to-string (s)
          (flag:print-help
           :stream s :right-margin 80 :indent 2 :parentheses "[]"
           :include-packages '(:ace.flag-test)))
    :for line :from 1
    :for h :in (string:split help :by #\Newline)
    :for e :in (string:split *print-help-output* :by #\Newline)
    :do
    (setf e (string-right-trim " " e)
          h (string-right-trim " " h))
    (expect (string= e h) "[line: ~D] Expected ~S = ~S" line e h)))

(defmacro with-flags-binding ((&key args (unparsed (gensym))
                               (global-flags t)
                               (normalize t)) &body body)
  "Parses the args as flags and binds their values while executing body."
  `(multiple-value-bind (,unparsed vars values)
       (flag:parse-command-line
        :args ,args :setp nil
        :global-flags ,global-flags
        :normalize ,normalize)
     (declare (ignorable ,unparsed))
     (progv vars values
       ,@body)))

(deftest test-string-flag ()
  (with-flags-binding (:args '("--string" "abc"))
    (expect (equal *string* "abc"))))

(deftest test-parsed-flag ()
  (with-flags-binding (:args '("--parsed-flag" "FF"))
    (expect (equal *parsed-flag* #xFF))))

(deftest test-no-prefixed-flag ()
  (with-flags-binding (:args '("--nointeger"))
    (expect (null *integer*))))

(deftest test-keyword-flag ()
  (multiple-value-bind (type result parsed-p consume-p)
      (parse-variable '*keyword* "keyword")
    (expect (equal type '(or null keyword)))
    (expect (equal result :keyword))
    (expect parsed-p)
    (expect consume-p))

  (with-flags-binding (:args '("--keyword" "keyword"))
    (expect (eq *keyword* :keyword))))


(deftest test-foo-flag ()
  (multiple-value-bind (type result parsed-p consume-p)
      (parse-variable '*foo* "F00")
    (expect (equal type 'foo))
    (expect (equal result #xF00))
    (expect parsed-p)
    (expect consume-p))

  (with-flags-binding (:args '("--foo" "F00"))
    (expect (eq *foo* #xF00))))

(deftest test-flags1 ()
  (with-flags-binding (:args '("--boolean=false"
                               "--boolean2"
                               "--notrue"
                               "--nofalse"
                               "--null"    "null"
                               "--keyword" "keyword"
                               "--member"  "a"
                               "--implicit_keyword" "keyword"
                               "--implicit_string" "string"
                               "--string" "abc"
                               "--very_long_doc" "doc"
                               "--implicit_integer" "1000"
                               "--integer" "20"
                               "--number" "0x20"
                               "--float" "0.0"
                               "--nonaked_flag"
                               "--a-flag" "10"

                               "--unknown-flag1" "unknown-value"
                               "--unknown-flag2"

                               "--double" "0.1"
                               "--double2" "-nan"
                               "--double3" "-inf"
                               "--mod" "5"
                               "--plus" "2000"
                               "--some_flag" "nan"
                               "--ace.flag-test:*parameter*" "20"
                               "--a-named_flag" "100"
                               "--named_flag2" "200"
                               "--parsed_flag" "FF"
                               "--foo" "F00"

                               "--unknown-flag3" "unknown-value"
                               "--unknown-flag4")
                       :unparsed unparsed
                       :global-flags t)
    (expect (equal unparsed '("--unknown-flag1" "unknown-value"
                              "--unknown-flag2"
                              "--unknown-flag3" "unknown-value"
                              "--unknown-flag4")))

    (expect (not *boolean*))
    (expect *boolean2*)
    (expect (not *true*))
    (expect (not *false*))
    (expect (not *null*))
    (expect (eq *keyword* :keyword))
    (expect (eq *member* :a))
    (expect (eq *implicit-keyword* :keyword))
    (expect (equal *implicit-string* "string"))
    (expect (equal *string* "abc"))
    (expect (equal *very-long-doc* "doc"))
    (expect (= *implicit-integer* 1000))
    (expect (= *integer* 20))
    (expect (= *number* #x20))
    (expect (= *float* 0.0))
    (expect (= *double* 0.1d0))
    (expect (number:float-nan-p *double2*))
    (expect (= sb-ext:double-float-negative-infinity *double3*))
    (expect (= *mod* 5))
    (expect (= *plus* 2000))
    (expect (eq *some-flag* :nan))
    (expect (= *parameter* 20))
    (expect (= *named-flag* 100))
    (expect (= *named-flag2* 200))
    (expect (= *parsed-flag* #xFF))
    (expect (= *foo* #xF00))
    (expect (not naked-flag))
    (expect (= flags:a-flag 10))))

(deftest test-flags2 ()
  (with-flags-binding (:args '("--boolean=no"
                               "--boolean2"
                               "--notrue"
                               "--nofalse"
                               "--null=null"
                               "--keyword=keyword"
                               "--member=a"
                               "--implicit_keyword=keyword"
                               "--implicit_string=string"
                               "--string=abc"
                               "--very_long_doc=doc"
                               "--implicit_integer=1000"
                               "--integer=20"
                               "--number=0x20"
                               "--float=0.0"
                               "--nonaked_flag"
                               "--a-flag=10"

                               "--unknown-flag1=unknown-value"
                               "--unknown-flag2"

                               "--double=0.1"
                               "--double2=-nan"
                               "--double3=-inf"
                               "--mod=5"
                               "--plus=2000"
                               "--some_flag=nan"

                               "--ace.flag-test:*parameter*=20"
                               "--a-named_flag=100"
                               "--named_flag2=200"
                               "--parsed_flag=FF"
                               "--foo=F00"

                               "--unknown-flag3=unknown-value"
                               "--unknown-flag4")
                       :unparsed unparsed
                       :global-flags t)
    (expect (equal unparsed '("--unknown-flag1=unknown-value"
                              "--unknown-flag2"
                              "--unknown-flag3=unknown-value"
                              "--unknown-flag4")))

    (expect (not *boolean*))
    (expect *boolean2*)
    (expect (not *true*))
    (expect (not *false*))
    (expect (not *null*))
    (expect (eq *keyword* :keyword))
    (expect (eq *member* :a))
    (expect (eq *implicit-keyword* :keyword))
    (expect (equal *implicit-string* "string"))
    (expect (equal *string* "abc"))
    (expect (equal *very-long-doc* "doc"))
    (expect (= *implicit-integer* 1000))
    (expect (= *integer* 20))
    (expect (= *number* #x20))
    (expect (= *float* 0.0))
    (expect (= *double* 0.1d0))
    (expect (number:float-nan-p *double2*))
    (expect (= sb-ext:double-float-negative-infinity *double3*))
    (expect (= *mod* 5))
    (expect (= *plus* 2000))
    (expect (eq *some-flag* :nan))
    (expect (= *parameter* 20))
    (expect (= *named-flag* 100))
    (expect (= *named-flag2* 200))
    (expect (= *parsed-flag* #xFF))
    (expect (= *foo* #xF00))
    (expect (not naked-flag))
    (expect (= flags:a-flag 10))))

(deftest test-flags3 ()
  (with-flags-binding (:args '("--boolean=no"
                               "--boolean2"
                               "--notrue"
                               "--a-flag=10"

                               "--unknown-flag1=unknown-value"
                               "--unknown-flag2"

                               "--some_flag=nan"

                               "--ace.flag-test:parameter=20"
                               "--a-named_flag=100"
                               "--foo=F00"

                               "--unknown-flag3=unknown-value"
                               "--unknown-flag4")
                       :unparsed unparsed
                       :global-flags nil)
    (expect (equal unparsed '("--unknown-flag1=unknown-value"
                              "--unknown-flag2"
                              "--ace.flag-test:parameter=20"
                              "--unknown-flag3=unknown-value"
                              "--unknown-flag4")))))


(deftest test-flags4 ()
  (let* ((*global-flags* nil)
         (*parameter* 11)
         (unparsed
           (flag:parse-command-line :args '("--unknown-flag1=unknown-value"
                                            "--lisp-global-flags=t"

                                            "--unknown-flag2"
                                            "--ace.flag-test:*parameter*=33"

                                            "--unknown-flag3=unknown-value"
                                            "--unknown-flag4")
                                    :setp t)))
    (expect (equal unparsed '("--unknown-flag1=unknown-value"
                              "--unknown-flag2"
                              "--unknown-flag3=unknown-value"
                              "--unknown-flag4")))

    (expect (= *parameter* 33))))

(deftest test-flags-repeated ()
  (with-flags-binding (:args '("--boolean"
                               "--boolean2=no"
                               "--noboolean2"
                               "--noboolean"
                               "--boolean2=yes"))
    (expect *boolean2*)
    (expect (not *boolean*))))


(deftest test-stop-parsing ()
  (with-flags-binding (:args '("--boolean"
                               "--boolean2=yes"
                               "--"
                               "--noboolean2"
                               "--noboolean"
                               "--boolean2=yes")
                       :unparsed unparsed)
    (expect (equal '("--"
                     "--noboolean2"
                     "--noboolean"
                     "--boolean2=yes")
                   unparsed))
    (expect *boolean*)
    (expect *boolean2*)))

(defun test-wrong-value (&rest args)
  (assert-error (flag:parse-command-line :args args :setp nil :normalize t)))

(deftest test-wrong-values ()
  (test-wrong-value "--boolean" "10")
  (test-wrong-value "--boolean" ":true")
  (test-wrong-value "--boolean2" "0")
  (test-wrong-value "--null"    "true")
  (test-wrong-value "--keyword" "f:10")
  (test-wrong-value "--member"  "f")
  (test-wrong-value "--implicit_keyword" "some:symbol")
  (test-wrong-value "--implicit_integer" "abc")
  (test-wrong-value "--integer" "0.0")
  (test-wrong-value "--number" ":test")
  (test-wrong-value "--float" ":test")
  (test-wrong-value "--naked_flag" "very-true")
  (test-wrong-value "--double" "0f0")
  (test-wrong-value "--double2" "#xFFFF")
  (test-wrong-value "--double3" "nada")
  (test-wrong-value "--mod" "-10")
  (test-wrong-value "--plus" "-2000")
  (test-wrong-value "--some_flag" "nano"))

(defun test-missing-value (&rest args)
  (assert-error (flag:parse-command-line :args args :setp nil :normalize t)))

(deftest test-missing-values ()
  (test-missing-value "--null")
  (test-missing-value "--keyword")
  (test-missing-value "--member")
  (test-missing-value "--implicit_keyword")
  (test-missing-value "--implicit_integer")
  (test-missing-value "--integer")
  (test-missing-value "--number")
  (test-missing-value "--float")
  (test-missing-value "--double")
  (test-missing-value "--double2")
  (test-missing-value "--double3")
  (test-missing-value "--mod")
  (test-missing-value "--plus")
  (test-missing-value "--some_flag"))

(deftest invalid-names-test ()
  (expect-macro-warning (flag:define *f* t "na" :name "an_underscore-and-hyphens" :type symbol))
  (expect-macro-warning (flag:define *f* t "na" :name "3rror" :type symbol))
  (expect-macro-warning (flag:define *f* t "na" :names ("a-dash" "3rror") :type symbol))
  (expect-macro-warning (flag:define *.* t "na" :type symbol))
  (expect-macro-warning (flag:define *f.* t "na" :type symbol))
  (expect-macro-warning (flag:define *funny/flag* t "na" :type symbol)))


(test-define *boo* t "Short test boolean flag." :names ("boo" "B"))

(test-define *key* nil "Short keyword flag" :type (or null keyword) :names ("key" "K"))

(test-define *mem* :a "Short member flag" :type (member nil :a :b :c) :names ("mem" "M"))

(test-define *int* 10 "An short int flag" :type (or (integer -100 100) null) :names ("int" "i"))
(test-define *int2* 20 "An short int flag" :type (or (integer -100 100) null) :names ("sint" "I"))

(test-define *flo* 10.0 "A short float flag" :type single-float :names ("sfloat" "f"))

(test-define *str* "" "A double flag" :type string :names ("str" "S"))

(deftest test-short-flags ()
  (with-flags-binding (:args '("-B=false"
                               "-K" "kEy"
                               "-M" "B"
                               "-i=11"
                               "-I" "22"
                               "-f" "0.0"
                               "-S" "str"))
    (expect (null *boo*))
    (expect (eq *key* :key))
    (expect (eq *mem* :B))
    (expect (= *int* 11))
    (expect (= *int2* 22))
    (expect (= *flo* 0.0))
    (expect (equal *str* "str"))))

(test-define *flag_with_underscores* 10.0 "A flag with underscores.")
(test-define *flag-with-hyphens* 30.0 "A flag with hyphens.")

(deftest underscores-test ()
  (with-flags-binding (:args '("--flag_with_underscores=20.0"
                               "--flag_with_hyphens=40.0"))
    (expect (= *flag_with_underscores* 20.0))
    (expect (= *flag-with-hyphens* 40.0)))
  (with-flags-binding (:args '("--flag-with-underscores=20.0"
                               "--flag-with-hyphens=40.0"))
    (expect (= *flag_with_underscores* 20.0))
    (expect (= *flag-with-hyphens* 40.0)))
  (with-flags-binding (:args '("--flag_with_underscores=20.0"
                               "--flag_with_hyphens=40.0")
                       :normalize nil)
    (expect (= *flag_with_underscores* 20.0))
    (expect (= *flag-with-hyphens* 30.0)))
  (with-flags-binding (:args '("--flag-with-underscores=20.0"
                               "--flag-with-hyphens=40.0")
                       :normalize nil)
    (expect (= *flag_with_underscores* 10.0))
    (expect (= *flag-with-hyphens* 40.0))))


;;; LEGACY tests form com.ace.flag

(define-constant +before+ '("before" "-a" "-" "--no" "") :test #'equal)
(define-constant +after+ '("--after" "-b" "-" "--no" "") :test #'equal)
(define-constant +skip+ '("--" "--boolflag" "--keyflag=a" "--symflag=b" "--stringflag=c"
                   "--intflag=d" "--sfflag=e" "--dfflag=f" "--color=g")
  :test #'equal)
(define-constant +expected-unparsed-flags+ (append +before+ +after+ +skip+) :test #'equal)

(defun parse-command-line* (arguments)
  (let ((command-line (append +before+ arguments +after+ +skip+)))
    (check (equal +expected-unparsed-flags+ (flag:parse-command-line :args command-line)))))

(test-define *boolean-flag* nil "Boolean flag used for testing." :type boolean :name "boolflag")

(deftest boolean-flag ()
  (expect (not *boolean-flag*))
  (expect (string= (documentation '*boolean-flag* 'variable) "Boolean flag used for testing."))
  (let ((*boolean-flag* *boolean-flag*))
    (flet ((test (expected flags)
             (setf *boolean-flag* (not expected))
             (parse-command-line* flags)
             (expect (eq expected *boolean-flag*))))
      (test t '("--boolflag"))
      (test t '("--boolflag=true"))
      ;; Those two are ambiguous and should fail below.
      ;; (test t '("--boolflag" "true"))
      ;; (test nil '("--boolflag" "false"))
      (test nil '("--noboolflag"))
      (test nil '("--boolflag=false")))
    (flet ((test (expected flags)
             (setf *boolean-flag* (not expected))
             (flag:parse-command-line :args flags)
             (expect (eq expected *boolean-flag*))))
      ;; Boolean short forms should work as the last argument.
      (test t '("--boolflag"))
      (test nil '("--noboolflag")))

    ;; The negative short form with an argument should be skipped.
    (let ((flags '("--noboolflag=true")))
      (expect (equal (flag:parse-command-line :args flags) flags)))

    (expect-error (parse-command-line* '("--boolflag=")))
    (expect-error (parse-command-line* '("--boolflag=foobar")))
    (expect-error (parse-command-line* '("--boolflag" "true")))
    (expect-error (parse-command-line* '("--boolflag" "false")))
    (expect-error (parse-command-line* '("--noboolflag" "true")))
    (expect-error (parse-command-line* '("--noboolflag" "false")))))

(test-define *keyword-flag* :foo "no doc" :name "keyflag" :type keyword)

(deftest keyword-flag ()
  (expect (eq *keyword-flag* :foo))
  (let ((*keyword-flag* *keyword-flag*))
    (parse-command-line* '("--keyflag" "dog"))
    (expect (eq *keyword-flag* :dog))
    (parse-command-line* '("--keyflag=cat"))
    (expect (eq *keyword-flag* :cat))
    (parse-command-line* '("--keyflag="))
    (expect (eq *keyword-flag* :||))))

(test-define *symbol-flag* :bar "no doc" :name "symflag" :type symbol)

(deftest symbol-flag ()
  (expect (eq *symbol-flag* :bar))
  (let ((*symbol-flag* *symbol-flag*))
    (parse-command-line* '("--symflag" "ace.flag:define"))
    (expect (eq *symbol-flag* 'ace.flag:define))
    (parse-command-line* '("--symflag=ace.flag-test::unexported-symbol"))
    (expect (eq *symbol-flag* 'unexported-symbol))
    (expect-error (parse-command-line* '("--symflag=")))
    (expect-error (parse-command-line* '("--symflag" ":bad-colons")))
    (expect-error (parse-command-line* '("--symflag" "more:bad:colons")))
    (expect-error (parse-command-line* '("--symflag" "bad-package:foo")))
    (expect-error (flag:parse-command-line :args '("--symflag")))))

(test-define *string-flag* "foo" "no doc" :name "stringflag" :type string)

(deftest string-flag ()
  (expect (string= *string-flag* "foo"))
  (let ((*string-flag* *string-flag*))
    (parse-command-line* '("--stringflag" "dog"))
    (expect (string= *string-flag* "dog"))
    (parse-command-line* '("--stringflag=cat"))
    (expect (string= *string-flag* "cat"))
    (parse-command-line* '("--stringflag="))
    (expect (string= *string-flag* ""))))

(test-define *integer-flag* 10 "no doc" :name "intflag" :type (integer -10 10))

(deftest integer-flag ()
  (expect (= *integer-flag* 10))
  (let ((*integer-flag* *integer-flag*))
    (parse-command-line* '("--intflag" "-2"))
    (expect (= *integer-flag* -2))
    (parse-command-line* '("--intflag=3"))
    (expect (= *integer-flag* 3))
    (expect-error (parse-command-line* '("--intflag=")))
    (expect-error (parse-command-line* '("--intflag=123x456")))))

(test-define *single-float-flag* 3.14f0 "no doc" :name "sfflag" :type single-float)

(deftest single-float-flag ()
  (expect (= *single-float-flag* 3.14f0))
  (let ((*single-float-flag* *single-float-flag*))
    (parse-command-line* '("--sfflag" "0.42"))
    (expect (= *single-float-flag* 0.42f0))
    (parse-command-line* '("--sfflag=-.42e-2"))
    (expect (= *single-float-flag* -.42f-2))
    (expect-error (parse-command-line* '("--sfflag=")))
    (expect-error (parse-command-line* '("--sfflag=-.42x-2")))
    (expect-error (parse-command-line* '("--sfflag=-.42d-2")))))

(test-define *double-float-flag* 0.12345d0 "no doc" :name "dfflag" :type (double-float -1d0 1d0))

(deftest double-float-flag ()
  (expect (= *double-float-flag* 0.12345d0))
  (let ((*double-float-flag* *double-float-flag*))
    (parse-command-line* '("--dfflag" "0.42"))
    (expect (= *double-float-flag* 0.42d0))
    (parse-command-line* '("--dfflag=-.42E-2"))
    (expect (= *double-float-flag* -0.42d-2))
    (expect-error (parse-command-line* '("--dfflag=")))
    (expect-error (parse-command-line* '("--dfflag=-.42x-2")))
    (expect-error (parse-command-line* '("--dfflag=-.42s-2")))))

(deftype color () '(member :red :green :blue))

(defun color-parser (string)
  (let ((color (cond ((string= string "red") :red)
                     ((string= string "green") :green)
                     ((string= string "blue") :blue)
                     ((string= string "orange") :orange))))
    (if color
        (values color t)
        (values nil nil))))

(test-define *color* :red "no doc" :name "color" :type color :parser color-parser)

(deftest color-flag ()
  (expect (eq *color* :red))
  (let ((*color* *color*))
    (parse-command-line* '("--color" "blue"))
    (expect (eq *color* :blue))
    (expect-error (parse-command-line* '("--color=black")))
    (expect-error (parse-command-line* '("--color=orange")))))

(deftest syntax-errors ()
  (expect-macro-warning (flag:define *s* t "na" :name s :type symbol))
  (expect-macro-warning (flag:define *s* t "na" :name "" :type symbol))
  (expect-macro-warning (flag:define *s* t "na" :name "s" :type "symbol"))
  (expect-macro-error (flag:define *s* t "na" :name "s" :type symbol :help h))
  (expect-macro-warning (flag:define *s* t "na" :name "s" :type symbol :parser "p"))
  (expect-macro-warning (flag:define *s* t 'd :name "s" :type symbol))
  (expect-macro-warning (flag:define *s* t "na" :name "s" :type vector)))

(test-define *no-go* nil "no doc" :name "nogo" :type boolean)
(test-define *rth* nil "no doc" :name "rth" :type boolean)

(deftest boolean-semantic-errors ()
  (expect-error (flag:define *b* nil "na" :name "boolflag" :type boolean))
  (expect-error (flag:define *go* nil "na" :name "go" :type boolean))
  (expect-error (flag:define *north* nil "na" :name "north" :type symbol)))

(defvar *symbol-flag* nil)

(define-symbol-macro *symbol-macro-flag* *symbol-flag*)

(flag:define *symbol-macro-flag* nil "no doc" :type boolean :def nil)

(deftest symbol-macro-flag-test ()
  (let ((*symbol-flag* nil))
    (flag:parse-command-line :args '("--symbol-macro-flag"))
    (expect (eq t *symbol-flag*)))
  (let ((*symbol-flag* t))
    (flag:parse-command-line :args '("--nosymbol-macro-flag"))
    (expect (eq nil *symbol-flag*))))
