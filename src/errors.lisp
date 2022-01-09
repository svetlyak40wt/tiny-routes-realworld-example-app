;;;; errors.lisp
(in-package :cl-user)
(uiop:define-package :conduit.errors
  (:use :cl)
  (:export #:app-error
           #:validation-error
           #:signal-validation-error
           #:error-message))

(in-package :conduit.errors)

(define-condition app-error (simple-error)
  ()
  (:documentation "The top-level application error."))

(define-condition validation-error (app-error)
  ()
  (:documentation "A validation error."))

(defun signal-validation-error (format-control &rest format-arguments)
  (error 'validation-error
         :format-control format-control
         :format-arguments format-arguments))
