;;;; validators.lisp
(in-package :cl-user)
(uiop:define-package :conduit.validators
  (:use :cl)
  (:local-nicknames (:errors :conduit.errors))
  (:export #:check-type*
           #:check-string
           #:check-integer
           #:check-email
           #:check-username
           #:check-password
           #:check-id
           #:check-profile
           #:check-slug
           #:check-title
           #:check-description
           #:check-article-body
           #:check-comment-body))

(in-package :conduit.validators)

(defparameter *password-min-length* 8)
(defparameter *password-max-length* 255)

(defparameter *username-min-length* 2)
(defparameter *username-max-length* 50)

(defparameter *article-title-min-length* 8)
(defparameter *article-title-max-length* 64)

(defparameter *article-description-min-length* 8)
(defparameter *article-description-max-length* 512)

(defparameter *email-regex*
  (cl-ppcre:create-scanner "^[a-z0-9.]+\@[a-z0-9.-]+$"))

(defparameter *slug-regex*
  (cl-ppcre:create-scanner "^[A-Za-z0-9.-]{4,}"))

(defun check-type* (value name type)
  (unless (typep value type)
    (errors:signal-validation-error "Value for `~a' must be of type ~a" name type))
  value)

(defun check-string (value name)
  (check-type* value name 'string))

(defun check-integer (value name)
  (check-type* value name 'integer))

(defun check-regex (value name regex)
  (unless (cl-ppcre:scan regex (check-string value name))
    (errors:signal-validation-error "Value for `~a' fails to match expected format" name))
  value)

(defun check-length-between (value name min-length max-length)
  (check-type min-length integer)
  (check-type max-length integer)
  (let ((length (length value)))
    (when (or (< length min-length)
              (> length max-length))
      (errors:signal-validation-error "Value for `~a' must have length between ~d and ~d and has ~d"
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

(defun check-profile (value &optional (name "profile"))
  (check-type* value name 'profile))

(defun check-slug (value &optional (name "slug"))
  (check-regex value name *slug-regex*))

(defun check-title (value &optional (name "title"))
  (check-string-length-between value name *article-title-min-length* *article-title-max-length*))

(defun check-description (value &optional (name "description"))
  (check-string-length-between value name *article-description-min-length* *article-description-max-length*))

(defun check-article-body (value &optional (name "body"))
  (check-string value name))

(defun check-comment-body (value &optional (name "body"))
  (check-string value name))
