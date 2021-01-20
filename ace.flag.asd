;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defsystem ace.flag
  :name "Ace Lisp Flag Libraries"
  :description "Command Line Flag Libraries for Common Lisp."
  :long-description
  "Command Line Flag Libraries for Common Lisp."
  :version "1.0"
  :author "Lisp Community"
  :license "MIT"
  :depends-on (:ace.core)
  :serial t
  :components
  ((:module "flag"
    :serial t
    :pathname ""
    :components ((:file "parse")
                 (:file "flag")))))
