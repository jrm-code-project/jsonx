;;; -*- Lisp -*-

;; Note:  This file depends on the CL-JSON library.

(in-package "JSONX")

;; Note:  There is no :reader or :accessor for the VALUE slot.  This is by design.
;;        json-literal objects are atomic and immutable - they just print as their VALUE
;;        when encoded in JSON.
(defclass json-literal ()
  ((value :initarg :value :type string))
  (:documentation "Class representing a JSON literal value such as true, false, or null."))

(defmethod json:encode-json ((object json-literal) &optional stream)
  "Render a JSON literal (true, false, null) to the given STREAM."
  (princ (slot-value object 'value) stream)
  nil)

(defmethod print-object ((object json-literal) stream)
  "Print a JSON literal object (true, false, null) as a Lisp object."
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (slot-value object 'value))))

(defparameter +json-false+
  (make-instance 'json-literal :value "false")
  "Lisp object representing the JSON literal `false`.")

(defparameter +json-null+
  (make-instance 'json-literal :value "null")
  "Lisp object representing the JSON literal `null`.")

(defparameter +json-true+
  (make-instance 'json-literal :value "true")
  "Lisp object representing the JSON literal `true`.")

(defparameter +json-empty-list+
  (vector)
  "Lisp object representing an empty JSON array.")

(defparameter +json-empty-object+
  (make-hash-table)
  "Lisp object representing an empty JSON object ({}).")

(defun set-decoder-jrm-semantics ()
  "Set the JSON decoder semantics to the following:
  * Strings and Numbers are decoded naturally, reals becoming floats.
  * The literal name true is decoded to +json-true+, false to +json-false+ and null to +json-null+.
  * Arrays are decoded to sequences of the type *JSON-ARRAY-TYPE*.
  * Objects are decoded to hash-tables.
  (Note: Object keys are expected to be converted by a pre-configured
  *JSON-IDENTIFIER-NAME-TO-LISP* and interned in *JSON-SYMBOLS-PACKAGE*.)"
  ;; By necessity we must use the internal variables and functions of CL-JSON.
  (setq cl-json::+json-lisp-symbol-tokens+
        `(("false" . ,+json-false+)
          ("null"  . ,+json-null+)
          ("true"  . ,+json-true+)))
  (cl-json:set-custom-vars
   :integer #'cl-json::parse-number
   :real    #'cl-json::parse-number
   :boolean #'cl-json::json-boolean-to-lisp
   :beginning-of-array #'cl-json::init-accumulator
   :array-member #'cl-json::accumulator-add
   :end-of-array #'cl-json::accumulator-get-sequence
   :array-type 'vector
   :beginning-of-object #'cl-json::init-accumulator
   :object-key #'cl-json::accumulator-add-key
   :object-value #'cl-json::accumulator-add-value
   :end-of-object (compose (lambda (accumulated)
                             (if (null accumulated)
                                 +json-empty-object+
                                 (alist-hash-table accumulated)))
                           #'cl-json::accumulator-get)
   :beginning-of-string #'cl-json::init-string-stream-accumulator
   :string-char #'cl-json::string-stream-accumulator-add
   :end-of-string #'cl-json::string-stream-accumulator-get
   :aggregate-scope (union cl-json:*aggregate-scope-variables*
                           '(cl-json::*accumulator* cl-json::*accumulator-last*))
   :internal-decoder #'cl-json:decode-json))

(defmacro with-decoder-jrm-semantics (&body body)
  "Execute BODY in a dynamic environment where the decoder semantics
is such as set by SET-DECODER-JRM-SEMANTICS."
  `(CL-JSON:WITH-SHADOWED-CUSTOM-VARS
     (SET-DECODER-JRM-SEMANTICS)
     ,@body))

;; A simple smoke test to verify that our custom decoder semantics work as intended.
(eval-when (:load-toplevel :execute)
  (with-decoder-jrm-semantics
    (let* ((json-string1 "{\"a\":1,\"c\":true,\"d\":false,\"e\":null,\"f\":[],\"g\":{\"foo\":42,\"bar\":[1,2,3]}}")
           (decoded-obj (cl-json:decode-json-from-string json-string1))
           (json-string2
             (cl-json:encode-json-to-string decoded-obj)))
      (assert (equal json-string1
                     json-string2))
                  ;; Assert specific values and types
            (assert (typep decoded-obj 'hash-table))
            (assert (eq (gethash :c decoded-obj) +json-true+))
            (assert (eq (gethash :d decoded-obj) +json-false+))
            (assert (eq (gethash :e decoded-obj) +json-null+))
            (assert (typep (gethash :f decoded-obj) 'vector))
            (assert (zerop (length (gethash :f decoded-obj))))
            (let ((nested-obj (gethash :g decoded-obj)))
              (assert (typep nested-obj 'hash-table))
              (assert (= (gethash :foo nested-obj) 42))
              (let ((bar-array (gethash :bar nested-obj)))
                (assert (typep bar-array 'vector))
                (assert (equalp bar-array #(1 2 3))))))))

(defun json-alist-entry? (s)
  "Returns true if S is a cons cell whose CAR (key) is either a symbol or a string,
   suitable for an entry in a JSON-like association list."
  (and (consp s)
       (or (stringp (car s))
           (symbolp (car s)))))

(defun is-json-alist? (s)
  "Returns true if S is a list where every element
   is a cons cell whose CAR (key) is either a symbol or a string.
   This is used to determine if a Lisp list can be interpreted as a JSON object."
  (or (null s)
      (and (consp s)
           (every #'json-alist-entry? s))))

(defun encode-json-list-try-alist (s stream)
  "Encodes a Lisp list S to JSON. If S appears to be an association list
  (i.e., IS-JSON-ALIST? returns true), it is encoded as a JSON object.
  Otherwise, it is encoded as a JSON array."
  (if (is-json-alist? s)
      (cl-json::encode-json-alist s stream)
      (cl-json::encode-json-list-guessing-encoder s stream)))

;; Smash the default list encoder of CL-JSON to use our custom function.
;; This globally modifies the behavior of CL-JSON's list encoding.
;; This is done at load time and at execution time to ensure that our new
;; encoder is used in all relevant contexts.
;; (eval-when (:load-toplevel :execute)
;;   (setq cl-json::*json-list-encoder-fn* 'encode-json-list-try-alist))

(defmethod cl-json:encode-json ((object function) &optional stream)
  (cl-json:encode-json (prin1-to-string object) stream))

(defmethod cl-json:encode-json ((object quri:uri) &optional stream)
  "Encodes a QURI:URI object as a JSON string."
  (cl-json:encode-json (quri:render-uri object nil) stream))

(defmethod cl-json:encode-json ((object pathname) &optional stream)
  "Encodes a Common Lisp PATHNAME object as a JSON string,
   using its NAMSTRING representation."
  (cl-json:encode-json (namestring object) stream))

(defun dehashify (object &optional (seen (make-hash-table :test 'eq)))
  "Recursively traverses a Lisp data structure, converting hash tables into association lists.
  - Hash tables are converted to association lists, and their elements are recursively dehashified.
  - Cons cells (lists) have their CAR and CDR recursively dehashified.
  - Vectors have their elements recursively dehashified.
  - Strings and other atomic types (numbers, symbols, etc.) are returned as is.
  The function attempts to preserve structural sharing by returning the original object
  if no sub-elements are modified during the recursive process."
  (if (gethash object seen)
      object
      (progn
        (setf (gethash object seen) t)
        (cond ((consp object)
               ;; Recursively dehashify both CAR and CDR of the cons cell.
               (let ((dehashed-car (dehashify (car object) seen))
                     (dehashed-cdr (dehashify (cdr object) seen)))
                 (if (and (eq dehashed-car (car object))
                          (eq dehashed-cdr (cdr object)))
                     object
                     (cons dehashed-car dehashed-cdr))))
              ((hash-table-p object)
               ;; Convert hash table to an alist and recursively dehashify the result.
               ;; Both key and value of each entry will be dehashified.
               (dehashify (hash-table-alist object) seen))
              ((stringp object) object)
              ((vectorp object)
               (let ((dehashed (map 'vector (lambda (elem) (dehashify elem seen)) object)))
                 (if (every #'eql object dehashed)
                     object
                     dehashed)))
              (t object)))))
