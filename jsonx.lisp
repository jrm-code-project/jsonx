;;; -*- Lisp -*-

(in-package "JSONX")

(defclass json-literal ()
  ((value :initarg :value :type string)))

(defmethod json:encode-json ((object json-literal) &optional stream)
  (princ (slot-value object 'value) stream)
  nil)

(defmethod print-object ((object json-literal) stream)
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
   :end-of-object (compose #'alist-hash-table #'cl-json::accumulator-get)
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

(eval-when (:load-toplevel :execute)
  (with-decoder-jrm-semantics
    (let* ((json-string1 "{\"a\":1,\"c\":true,\"d\":false,\"e\":null,\"f\":[],\"g\":{\"foo\":42,\"bar\":[1,2,3]}}")
           (json-string2
             (cl-json:encode-json-to-string
              (cl-json:decode-json-from-string json-string1))))
      (assert (equal json-string1
                     json-string2))
      )))

(defun json-alist-entry? (s)
  "Returns true if S is a cons cell whose CAR (key) is either a symbol or a string,
   suitable for an entry in a JSON-like association list."
  (and (consp s)
       (or (symbolp (car s))
           (stringp (car s)))))

(defun is-json-alist? (s)
  "Returns true if S is a list where every element
   is a cons cell whose CAR (key) is either a symbol or a string.
   This is used to determine if a Lisp list can be interpreted as a JSON object."
  (and (or (null s)
           (consp s))
       (every #'json-alist-entry? s)))

(defun encode-json-list-try-alist (s stream)
  "Encodes a Lisp list S to JSON. If S appears to be an association list
  (i.e., IS-JSON-ALIST? returns true), it is encoded as a JSON object.
  Otherwise, it is encoded as a JSON array."
  (if (is-json-alist? s)
      (cl-json::encode-json-alist s stream)
      (cl-json::encode-json-list-guessing-encoder s stream)))

(eval-when (:load-toplevel :execute)
  (setq cl-json::*json-list-encoder-fn* 'encode-json-list-try-alist))

(defmethod cl-json:encode-json ((object quri:uri) &optional stream)
  "Encodes a QURI:URI object as a JSON string by rendering its URI string
   representation surrounded by double quotes."
  (write-char #\" stream)
  (quri:render-uri object stream)
  (write-char #\" stream)
  nil)

(defmethod cl-json:encode-json ((object pathname) &optional stream)
  "Encodes a Common Lisp PATHNAME object as a JSON string,
   using its NAMSTRING representation."
  (cl-json::write-json-string (namestring object) stream))

(defun dehashify (object)
  (cond ((hash-table-p object)
         (mapcar #'dehashify (hash-table-alist object)))
        ((consp object)
         (let ((dehashed-car (dehashify (car object)))
               (dehashed-cdr (dehashify (cdr object))))
           (if (and (eq dehashed-car (car object))
                    (eq dehashed-cdr (cdr object)))
               object
               (cons dehashed-car dehashed-cdr))))
        ((stringp object) object)
        ((vectorp object)
         (let ((dehashed (map 'vector #'dehashify object)))
           (if (every #'eql object dehashed)
               object
               dehashed)))
        (t object)))
