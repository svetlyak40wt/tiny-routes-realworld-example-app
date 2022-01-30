;;;; util.lisp
(in-package :cl-user)
(uiop:define-package :conduit.util
  (:use :cl)
  (:local-nicknames (:time :local-time)
                    (:a :alexandria))
  (:export #:format-timestamp
           #:parse-timestamp
           #:unix-now
           #:parse-boolean
           #:canonicalize-tags
           #:with-options
           #:with-json
           #:decode-value))

(in-package :conduit.util)

(defun format-timestamp (timestamp)
  "Return an ISO-8601 formatted string representing TIMESTAMP."
  (time:format-timestring nil timestamp :format time:+iso-8601-format+))

(defun parse-timestamp (timestamp-designator)
  "Return a timestamp corresponding to TIMESTAMP-DESIGNATOR."
  (typecase timestamp-designator
    (null nil)
    (fixnum (time:universal-to-timestamp timestamp-designator))
    (string (or (time:parse-timestring timestamp-designator :fail-on-error nil)
                (time:parse-timestring timestamp-designator :fail-on-error nil :date-time-separator #\Space)))
    (otherwise timestamp-designator)))

(defun parse-integer-safely (integer-designator &optional default)
  (typecase integer-designator
    (null default)
    (integer integer-designator)
    (string (handler-case (parse-integer integer-designator)
              (error (condition) (values default condition))))
    (otherwise integer-designator)))

(defun unix-now ()
  "Return the current unix timestamp."
  (time:timestamp-to-unix (time:now)))

(defun parse-boolean (boolean-designator)
  "Return a boolean corresponding to BOOLEAN-DESIGNATOR."
  (typecase boolean-designator
    (boolean boolean-designator)
    (fixnum (not (zerop boolean-designator)))
    (string (a:starts-with #\t boolean-designator :test #'char= :key #'char-downcase))
    (otherwise boolean-designator)))

(defun canonicalize-tags (tags-designator)
  (let ((tags (etypecase tags-designator
                (string (uiop:split-string tags-designator :separator '(#\,)))
                (list tags-designator))))
    (sort tags #'string<)))

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

(defun decode-value (class options &key wrapped-in)
  (let ((options (if wrapped-in (getf options wrapped-in) options)))
    (apply #'make-instance class :allow-other-keys t options)))
