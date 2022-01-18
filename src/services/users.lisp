;;;; users.lisp
(in-package :cl-user)
(uiop:define-package :conduit.services.users
  (:use :cl :conduit.types)
  (:import-from :conduit.util
                #:with-options)
  (:import-from :conduit.auth
                #:generate-auth-token
                #:hash-encode-password)
  (:export #:login
           #:register-user
           #:current-user
           #:update-user))

(in-package :conduit.services.users)

(defun authenticate-user (user token)
  (with-slots (id username email password-hash bio image created-at updated-at) user
    (make-authenticated-user
     id token username email password-hash :bio bio :image image
     :created-at created-at :updated-at updated-at)))

(defun login (email password)
  (when (and email password)
    (let* ((user (make-user 10 "test-user" email (hash-encode-password password))))
      (authenticate-user user (generate-auth-token user)))))

(defun register-user (rendition)
  (check-type rendition user-registration-rendition)
  (with-slots (username email password bio image) rendition
    (let* ((password-hash (hash-encode-password password))
           (user (make-user 11 username email password-hash
                            :bio bio :image image))
           (token (generate-auth-token user)))
      (authenticate-user user token))))

(defun current-user (auth)
  (with-options (id token) auth
    (when (and id token)
      (let* ((user (make-user id "test-user" "test@mail" (hash-encode-password "TEST"))))
        (authenticate-user user token)))))

(defun update-user (auth rendition)
  (check-type rendition user-update-rendition)
  (with-options (id token) auth
    (when (and id token)
      (with-slots (username email password bio image) rendition
        (let* ((password-hash (hash-encode-password (or password "TEST")))
               (user (make-user id
                                (or username "test-user")
                                (or email "test@mail")
                                password-hash
                                :bio bio :image image)))
          (authenticate-user user token))))))
