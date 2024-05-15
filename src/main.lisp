;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.params)

(declaim (ftype (function (string-key-alist string) t) get-param))
(defun get-param (params key)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string-key-alist params)
           (type string key))
  (cdr (assoc key params :test #'string=)))

(declaim (ftype (function (string-key-alist string-list) t) get-nested-param))
(defun get-nested-param (params key-path)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string-key-alist params)
           (type string-list key-path))
  (let ((current params)
        (alistp t))
    (declare (type list current)
             (type boolean alistp))
    (dolist (key key-path)
      (declare (type string key)
               (type string-list key-path))
      (let ((value (cdr (assoc key current :test #'string=))))
        (typecase value
          (null (return-from get-nested-param))
          (list (setq current value))
          (t
           (setq alistp nil
                 current (list value))))))
    (if alistp current (car current))))

(declaim (ftype (function (string-key-alist string-list) list) collect-params))
(defun collect-params (params keys)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string-key-alist params)
           (type string-list keys))
  (mapcar
   #'(lambda (key)
       (declare (type string key))
       (cdr (assoc key params :test #'string=)))
   keys))

(declaim (ftype (function (string-key-alist string-list-list) list) collect-nested-params))
(defun collect-nested-params (params key-paths)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type string-key-alist params)
           (type string-list-list key-paths))
  (mapcar
   #'(lambda (key-path)
       (declare (type string-list key-path))
       (get-nested-param params key-path))
   key-paths))
