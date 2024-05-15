;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:foo.lisp.params
  (:use #:cl)
  (:export #:get-param
           #:get-nested-param
           #:collect-params
           #:collect-nested-params))
