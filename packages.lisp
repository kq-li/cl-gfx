(in-package :cl-user)

(load "quicklisp/setup")
(ql:quickload '(:yacc :cl-lex) :silent t)

(defpackage :graphics
  (:use :cl :ext :yacc :cl-lex))
