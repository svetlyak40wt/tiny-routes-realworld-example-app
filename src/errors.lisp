(in-package :conduit)

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

(defun error-message (condition)
  (apply #'format nil
         (simple-condition-format-control condition)
         (simple-condition-format-arguments condition)))
