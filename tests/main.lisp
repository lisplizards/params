;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.params/tests)

(deftest get-param
  (testing
   "Returns the value for the specified key in the association list, or returns NIL"
   (let ((params '(("foo" . "bar")
                   ("quux" . 10)
                   ("baaz" . "quux"))))
     (ok (equal "quux" (foo.lisp.params:get-param params "baaz")))
     (ok (= 10 (foo.lisp.params:get-param params "quux")))
     (ok (null (foo.lisp.params:get-param params "bogus"))))))

(deftest get-nested-param
  (testing
   "With a key-path of depth equal to 1, returns the value for the specified key path, or returns NIL"
   (let ((params '(("foo" . "bar")
                   ("baaz" . "quux"))))
     (ok (equal "bar" (foo.lisp.params:get-nested-param params '("foo"))))
     (ok (null (foo.lisp.params:get-nested-param params '("bogus"))))))

  (testing
   "With a key-path of depth greater than 1, returns the value for the specified key path, or returns NIL"
   (let ((params '(("foo" . (("baaz" . "quux")
                             ("quux" . (("foo" . "bar")))
                             ("bar" . 33)
                             ("foobar" . (1 2 3)))))))
     (ok (equal "quux" (foo.lisp.params:get-nested-param params '("foo" "baaz"))))
     (ok (equal "bar" (foo.lisp.params:get-nested-param params '("foo" "quux" "foo"))))
     (ok (= 33 (foo.lisp.params:get-nested-param params '("foo" "bar"))))
     (ok (equalp '(("foo" . "bar")) (foo.lisp.params:get-nested-param params '("foo" "quux"))))
     (ok (equalp '(1 2 3) (foo.lisp.params:get-nested-param params '("foo" "foobar"))))
     (ok (null (foo.lisp.params:get-nested-param params '("bogus"))))
     (ok (null (foo.lisp.params:get-nested-param params '("foo" "bogus"))))
     (ok (null (foo.lisp.params:get-nested-param params '("foo" "quux" "bogus")))))))

(deftest collect-params
  (testing
   "Returns a list of values for top-level keys collected from the association list"
   (let ((params '(("foo" . "bar")
                   ("baaz" . "quux")
                   ("bar" . 11))))
     (ok (equal () (foo.lisp.params:collect-params params ())))
     (ok (equalp '("bar" 11) (foo.lisp.params:collect-params params '("foo" "bar"))))
     (ok (equal '(nil "quux") (foo.lisp.params:collect-params params '("bogus" "baaz")))))))

(deftest collect-nested-params
    (testing
     "Returns a list of the values for nested keys collected from the association list"
     (let ((params '(("foo" . (("baaz" . "quux")
                               ("quux" . (("foo" . "bar")))
                               ("bar" . 33)
                               ("foobar" . (1 2 3))))
                     ("bar" . (:x :y :z))
                     ("baaz" . (("quux" . (("baaz" . (("bar" . "foobar"))))))))))
       (ok (equal () (foo.lisp.params:collect-nested-params params ())))
       (ok (equalp '("quux" (("foo" . "bar")) (1 2 3) (:x :y :z) "foobar" nil)
                   (foo.lisp.params:collect-nested-params
                    params
                    '(("foo" "baaz")
                      ("foo" "quux")
                      ("foo" "foobar")
                      ("bar")
                      ("baaz" "quux" "baaz" "bar")
                      ("baaz" "quux" "bogus"))))))))
