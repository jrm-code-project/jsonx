;;; -*- Lisp -*-

(defpackage "JSONX"
  (:use "ALEXANDRIA" "CL")
  (:export
   "+JSON-EMPTY-LIST+"
   "+JSON-EMPTY-OBJECT+"
   "+JSON-FALSE+"
   "+JSON-NULL+"
   "+JSON-TRUE+"
   "DEHASHIFY"
   "JSON-LITERAL"
   "SET-DECODER-JRM-SEMANTICS"
   "WITH-DECODER-JRM-SEMANTICS"
   ))
