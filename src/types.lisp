;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.params)

(deftype string-key-alist ()
  `(and (satisfies listp)
        (satisfies string-key-alist-p)))

(deftype string-list ()
  `(and (satisfies listp)
        (satisfies string-list-p)))

(deftype string-list-list ()
  `(and (satisfies listp)
        (satisfies string-list-list-p)))

(declaim (ftype (function (list) boolean) string-key-alist-p))
(defun string-key-alist-p (lst)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list lst))
  (every #'(lambda (item)
             (and (consp item)
                  (stringp (car item))))
         lst))

(declaim (ftype (function (list) boolean) string-list-p))
(defun string-list-p (lst)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list lst))
  (every #'stringp lst))

(declaim (ftype (function (list) boolean) string-list-list-p))
(defun string-list-list-p (lst)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list lst))
  (every #'string-list-p lst))
