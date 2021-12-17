(in-package :conduit)

(defun add-padding (input)
  (let ((mod (mod (length input) 4)))
    (if (= mod 0)
        input
        (concatenate 'string input (make-string (- 4 mod) :initial-element #\.)))))

(defun remove-padding (input)
  (string-right-trim '(#\.) input))

(defun base64-url-decode (input)
  (base64:base64-string-to-string (add-padding input) :uri t))

(defun base64-url-encode-string (string)
  (remove-padding (base64:string-to-base64-string string :uri t)))

(defun base64-url-encode-octets (octets)
  (remove-padding (base64:usb8-array-to-base64-string octets :uri t)))

(defun hmac-sign (key message)
  (let ((hmac (ironclad:make-hmac key :sha256))
        (msg (ironclad:ascii-string-to-byte-array message)))
    (ironclad:update-hmac hmac msg)
    (ironclad:hmac-digest hmac)))

(defun make-jwt (key headers claims)
  (let ((encoded-headers (base64-url-encode-string (jojo:to-json headers)))
        (encoded-claims (base64-url-encode-string (jojo:to-json claims))))
    (let ((message (format nil "~a.~a" encoded-headers encoded-claims)))
      (format nil "~a.~a" message
              (base64-url-encode-octets (hmac-sign key message))))))

(defun parse-jwt (jwt)
  (destructuring-bind (&optional headers claims signature &rest rest)
      (uiop:split-string jwt :separator '(#\.))
    (unless (and headers claims signature (null rest))
      (signal-validation-error "Unable to parse jwt: ~a" jwt))
    (values
     (jojo:parse (base64-url-decode headers))
     (jojo:parse (base64-url-decode claims))
     (ironclad:ascii-string-to-byte-array (base64-url-decode signature))
     (format nil "~a.~a" headers claims))))

(defun hmac-verify (key message signature)
  (equalp (hmac-sign key message) signature))

(defun verify-jwt (key jwt)
  (multiple-value-bind (headers claims signature message) (parse-jwt jwt)
    ;; 1) check signature
    (unless (equalp (hmac-sign key message) signature)
      (signal-validation-error "Invalid jwt signature"))
    ;; 2) check the exp
    (let ((exp (getf headers :|exp|)))
      (when (and exp (< exp (unix-now)))
        (signal-validation-error "Expired jwt token")))
    (values claims headers)))
