;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.params"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://github.com/lisplizards/params"
  :bug-tracker "https://github.com/lisplizards/params/issues"
  :source-control (:git "https://github.com/lisplizards/params.git")
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("package" "types"))
                 (:file "types" :depends-on ("package"))
                 (:file "package"))))
  :description "Easily access association list values"
  :in-order-to ((test-op (test-op "foo.lisp.params/tests"))))

(defsystem "foo.lisp.params/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.params"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.params"
  :perform (test-op (op c) (symbol-call :rove :run c)))
