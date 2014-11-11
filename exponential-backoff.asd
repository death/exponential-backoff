;;;; +----------------------------------------------------------------+
;;;; | Exponential backoff                                            |
;;;; +----------------------------------------------------------------+

;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:exponential-backoff
  :description "An implementation of the exponential backoff algorithm"
  :author "death <github.com/death>"
  :license "MIT"
  :depends-on ()
  :components
  ((:file "exponential-backoff")))
