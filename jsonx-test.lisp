;;; -*- Lisp -*-

(defpackage "JSONX-TEST"
  (:use "CL" "FIVEAM")
  (:export "JSONX-SUITE"))

(in-package "JSONX-TEST")

(def-suite jsonx-suite :description "Tests for JSONX.")
(in-suite jsonx-suite)

(defstruct point x y)

(defun decode (string)
  (jsonx:with-decoder-jrm-semantics
    (cl-json:decode-json-from-string string)))

(defun encode (object)
  (cl-json:encode-json-to-string object))

(test literals-encode
  (is (string= "true" (encode jsonx:+json-true+)))
  (is (string= "false" (encode jsonx:+json-false+)))
  (is (string= "null" (encode jsonx:+json-null+))))

(test empty-constants
  (is (vectorp jsonx:+json-empty-list+))
  (is (zerop (length jsonx:+json-empty-list+)))
  (is (hash-table-p jsonx:+json-empty-object+))
  (is (zerop (hash-table-count jsonx:+json-empty-object+))))

(test decode-literals
  (is (eq jsonx:+json-true+ (decode "true")))
  (is (eq jsonx:+json-false+ (decode "false")))
  (is (eq jsonx:+json-null+ (decode "null"))))

(test decode-containers
  (let ((obj (decode "{\"a\":1,\"b\":[1,2,3],\"c\":{\"d\":\"x\"}}")))
    (is (hash-table-p obj))
    (is (= 1 (gethash :a obj)))
    (is (equalp #(1 2 3) (gethash :b obj)))
    (is (string= "x" (gethash :d (gethash :c obj)))))
  (is (eq jsonx:+json-empty-object+ (decode "{}")))
  (let ((v (decode "[]")))
    (is (vectorp v))
    (is (zerop (length v)))))

(test round-trip
  (let ((s "{\"a\":1,\"c\":true,\"d\":false,\"e\":null,\"f\":[],\"g\":{\"foo\":42,\"bar\":[1,2,3]}}"))
    (is (string= s (encode (decode s))))))

(test encode-misc-objects
  (is (string= (namestring #p"/tmp/x.txt") (decode (encode #p"/tmp/x.txt"))))
  (is (string= "http://example.com/a"
               (decode (encode (quri:uri "http://example.com/a")))))
  (is (string= (prin1-to-string (find-package "CL"))
               (decode (encode (find-package "CL")))))
  (is (string= (prin1-to-string #'car) (decode (encode #'car)))))

(test encode-structure
  (let ((decoded (decode (encode (make-point :x 1 :y 2)))))
    (is (= 1 (gethash :x decoded)))
    (is (= 2 (gethash :y decoded)))))

(test dehashify
  (let ((h (make-hash-table)))
    (setf (gethash :a h) 1)
    (is (equal '((:a . 1)) (jsonx:dehashify h)))
    (let ((nested (jsonx:dehashify (vector h 2))))
      (is (equalp (vector '((:a . 1)) 2) nested))))
  (let ((list (list 1 2 3))
        (str "abc"))
    (is (eq list (jsonx:dehashify list)))
    (is (eq str (jsonx:dehashify str)))))
