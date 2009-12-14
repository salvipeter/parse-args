;; -*- mode: lisp; syntax: common-lisp -*-

(in-package #:cl-user)

(defpackage #:parse-args-asd
  (:use #:common-lisp #:asdf))

(in-package #:parse-args-asd)

(asdf:defsystem #:parse-args
  :author "Peter Salvi"
  :licence "Public Domain"
  :description "getopt-like command line argument parser"
  :depends-on (#:cl-ppcre)
  :components ((:file "parse-args")))
