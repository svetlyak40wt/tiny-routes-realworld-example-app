(in-package :conduit)

(defparameter *password-min-length* 8)

(defparameter *password-max-length* 255)

(defparameter *username-min-length* 2)

(defparameter *username-max-length* 50)

(defparameter *email-regex*
  (cl-ppcre:create-scanner "^[a-z0-9.]+\@[a-z0-9.-]+$"))

(defun check-type* (value name type)
  (unless (typep value type)
    (signal-validation-error "Value for `~a' must be of type ~a" name type))
  value)

(defun check-string (value name)
  (check-type* value name 'string))

(defun check-integer (value name)
  (check-type* value name 'integer))

(defun check-regex (value name regex)
  (unless (cl-ppcre:scan regex (check-string value name))
    (signal-validation-error "Value for `~a' fails to match expected format" name))
  value)

(defun check-length-between (value name min-length max-length)
  (check-type min-length integer)
  (check-type max-length integer)
  (let ((length (length value)))
    (when (or (< length min-length)
              (> length max-length))
      (signal-validation-error "Value for `~a' must have length between ~d and ~d and has ~d"
                               name min-length max-length length))
    value))

(defun check-string-length-between (value name min-length max-length)
  (check-string value name)
  (check-length-between value name min-length max-length))

(defun check-email (value &optional (name "email"))
  (check-regex value name *email-regex*))

(defun check-username (value &optional (name "username"))
  (check-string-length-between value name *username-min-length* *username-max-length*))

(defun check-password (value &optional (name "password"))
  (check-string-length-between value name *password-min-length* *password-max-length*))

(defun check-id (value &optional (name "id"))
  (check-integer value name))
