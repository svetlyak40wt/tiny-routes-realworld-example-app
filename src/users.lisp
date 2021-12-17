(in-package :conduit)

(defun %make-authenticated-user (user token)
  (with-slots (id username email password-hash bio image created-at updated-at) user
    (make-authenticated-user
     id token username email password-hash :bio bio :image image
     :created-at created-at :updated-at updated-at)))

(defun users/login-user (email password)
  (when (and email password)
    (let* ((user (make-user 10 "test-user" email (hash-encode-password password))))
      (%make-authenticated-user user (generate-auth-token user)))))

(defun users/register-user (rendition)
  (check-type rendition user-registration-rendition)
  (with-slots (username email password bio image) rendition
    (let* ((password-hash (hash-encode-password password))
           (user (make-user 11 username email password-hash
                            :bio bio :image image))
           (token (generate-auth-token user)))
      (%make-authenticated-user user token))))

(defun users/current-user (id token)
  (when (and id token)
    (let* ((user (make-user 10 "test-user" "test@mail" (hash-encode-password "TEST"))))
      (%make-authenticated-user user token))))
