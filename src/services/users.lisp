;;;; users.lisp
(in-package :cl-user)
(uiop:define-package :conduit.services.users
  (:use :cl :conduit.types)
  (:local-nicknames (:db :conduit.db)
                    (:log :conduit.logger)
                    (:auth :conduit.auth))
  (:import-from :conduit.util
                #:with-options)
  (:import-from :alexandria
                #:when-let)
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
  (log:info :users "Attempting to login user with email ~a" email)
  (let ((user (and email password (db:user-by-email email))))
    (cond ((null user)
           (log:info :users "No such user for email ~a" email)
           nil)
          ((auth:valid-password-p password (password-hash user))
           (log:info :users "Found user ~a" user)
           (authenticate-user user (auth:generate-auth-token user)))
          (t
           (log:info :users "Invalid credentials for user ~a" email) nil))))

(defun register-user (rendition)
  (check-type rendition user-registration-rendition)
  (log:info :users "Attempting to register user via rendition ~a" rendition)
  (when-let ((user (db:insert-user rendition)))
    (log:info :users "Registered ~a" user)
    (authenticate-user user (auth:generate-auth-token user))))

(defun current-user (auth)
  (with-options (id token) auth
    (log:info :users "Attempting to get current user with id ~a" id)
    (when-let ((user (db:user-by-id id)))
      (log:info :users "Found current user ~a" user)
      (authenticate-user user token))))

(defun update-user (auth rendition)
  (check-type rendition user-update-rendition)
  (with-options (id token) auth
    (log:info :users "Attempting to update user with id ~a" id)
    (when-let ((user (db:update-user id rendition)))
      (log:info :users "Updated user ~a" user)
      (authenticate-user user token))))
