;;;; auth.lisp
(in-package :cl-user)
(uiop:define-package :conduit.auth
  (:use :cl :conduit.jwt)
  (:local-nicknames (:log :conduit.logger)
                    (:util :conduit.util)
                    (:types :conduit.types))
  (:import-from :bcrypt)
  (:export #:hash-encode-password
           #:valid-password-p
           #:generate-auth-token
           #:verify-auth-token
           #:initialize-auth))

(in-package :conduit.auth)

(defvar *key* nil
  "The private key used to sign auth tokens.")

(defparameter *auth-token-duration* 86000
  "The number of seconds for which an auth token remains valid.")

(defun hash-encode-password (password)
  (bcrypt:encode (bcrypt:make-password password)))

(defun valid-password-p (password password-hash)
  (bcrypt:password= password password-hash))

(defun generate-auth-token (user)
  (check-type user types:user)
  (let ((headers (list :|typ| "JWT" :|alg| "HS256"
                       :|exp| (+ (util:unix-now) *auth-token-duration*)))
        (claims (list :|id| (types:id user))))
    (make-jwt *key* headers claims)))

(defun verify-auth-token (token)
  (verify-jwt *key* token))

(defun initialize-auth (key)
  (check-type key string)
  (setf *key* (ironclad:ascii-string-to-byte-array key))
  (log:info :auth "Successfully initialize auth"))
