;;; -*- Lisp -*-

(defpackage "JSONX"
  (:use "ALEXANDRIA" "CL")
  (:export
   "+JSON-EMPTY-LIST+"
   "+JSON-EMPTY-OBJECT+"
   "+JSON-FALSE+"
   "+JSON-NULL+"
   "+JSON-TRUE+"
   "JSON-BOOLEAN"
   "SET-DECODER-JRM-SEMANTICS"
   "WITH-DECODER-JRM-SEMANTICS"
   ))
