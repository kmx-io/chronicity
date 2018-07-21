;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; repeater.lisp

;;; See the LICENSE file for licensing information.

(cl:in-package #:chronicity)

;;; Enable cl-interpol reader

#.(cl-interpol:enable-interpol-syntax)

(defclass timezone (tag)
  ())

(defmethod scan-tokens ((tag (eql 'timezone)) tokens)
  (dolist (token tokens tokens)
    (awhen (scan-for-timezone token) (tag it token))))

(defun timezone-type (word)
  (cl-ppcre:register-groups-bind (sign
                                  (#'parse-integer hours minutes))
      ("^([+-])([0-9]{2})([0-9]{2})$" word)
    (declare (type string sign)
             (type fixnum hours minutes))
    (let* ((hours-sec (* 3600 hours))
           (minutes-sec (* 60 minutes))
           (sec (+ hours-sec minutes-sec)))
      (declare (type fixnum hours-sec minutes-sec sec))
      (if (string= "-" sign)
          (- sec)
          sec))))

(defun scan-for-timezone (token)
  (let ((word (token-word token)))
    (and (scan "^[+-][0-9]{4}$" word)
         (create-tag 'timezone (timezone-type word)))))
