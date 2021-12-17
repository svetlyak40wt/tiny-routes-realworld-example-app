(in-package :conduit)

(defvar *key* nil
  "The private key used to sign auth tokens.")

(defparameter *auth-token-duration* 86000
  "The number of seconds for which an auth token remains valid.")

(defun hash-encode-password (password)
  (bcrypt:encode (bcrypt:make-password password)))

(defun valid-password-p (password password-hash)
  (bcrypt:password= password password-hash))

(defun generate-auth-token (user)
  (let ((headers (list :|typ| "JWT" :|alg| "HS256"
                       :|exp| (+ (unix-now) *auth-token-duration*)))
        (claims (list :|id| (id user))))
    (make-jwt *key* headers claims)))

(defun verify-auth-token (token)
  (verify-jwt *key* token))

(defun initialize-auth (key)
  (setf *key* (ironclad:ascii-string-to-byte-array key)))
