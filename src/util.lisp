;;;; util.lisp
(in-package :cl-user)
(uiop:define-package :conduit.util
  (:use :cl)
  (:import-from #:local-time)
  (:import-from #:alexandria
                #:starts-with)
  (:export #:format-timestamp
           #:parse-timestamp
           #:unix-now
           #:parse-boolean
           #:with-options
           #:with-json
           #:decode-json-value))

(in-package :conduit.util)

(defun format-timestamp (timestamp)
  "Return an ISO-8601 formatted string representing TIMESTAMP."
  (local-time:format-timestring nil timestamp :format local-time:+iso-8601-format+))

(defun parse-timestamp (timestamp-designator)
  "Return a timestamp corresponding to TIMESTAMP-DESIGNATOR."
  (typecase timestamp-designator
    (null nil)
    (fixnum (local-time:universal-to-timestamp timestamp-designator))
    (string (local-time:parse-timestring timestamp-designator))
    (otherwise timestamp-designator)))

(defun unix-now ()
  "Return the current unix timestamp."
  (local-time:timestamp-to-unix (local-time:now)))

(defun parse-boolean (boolean-designator)
  "Return a boolean corresponding to BOOLEAN-DESIGNATOR."
  (typecase boolean-designator
    (boolean boolean-designator)
    (fixnum (not (zerop boolean-designator)))
    (string (starts-with #\t boolean-designator :test #'char= :key #'char-downcase))
    (otherwise boolean-designator)))

(defun keywordize (name)
  (intern (string name) :keyword))

(defun name->option-keywords (name)
  (let ((name (string-downcase (string name)))
        (keywords '()))
    (push (keywordize name) keywords)
    (pushnew (keywordize (string-upcase name)) keywords)
    (pushnew (keywordize (str:camel-case name)) keywords)
    (pushnew (keywordize (str:snake-case name)) keywords)
    keywords))

(defun parse-option-value (options name &optional default)
  (multiple-value-bind (indicator value)
      (get-properties options (name->option-keywords name))
    (values (or value default) indicator)))

(defun expand-parse-option-value (options-var name default)
  `(or ,@(loop for kwd in (name->option-keywords name)
               collect `(getf ,options-var ,kwd))
       ,default))

(defmacro with-options (vars options &body body)
  (let ((options-var (gensym "options")))
    `(let ((,options-var ,options))
       (let ,(mapcar (lambda (var)
                       (multiple-value-bind (symbol name default)
                           (etypecase var
                             (symbol (values var var nil))
                             (list (values (first var) (second var) (third var))))
                         `(,symbol ,(expand-parse-option-value options-var name default))))
              vars)
         ,@body))))

(defmacro with-json (vars json &body body)
  `(with-options ,vars ,json ,@body))

(defun decode-json-value (options class-name &key wrapped-in)
  (let ((class (find-class class-name))
        (options (if wrapped-in
                     (parse-option-value options wrapped-in)
                   options)))
    (c2mop:ensure-finalized class)
    (apply #'make-instance class-name
           (loop for slot in (c2mop:class-slots class)
                 for name = (c2mop:slot-definition-name slot)
                 collect (keywordize name)
                 collect (parse-option-value options name)))))
