(in-package :conduit)

(defclass user ()
  ((email
    :initarg :email
    :type string
    :reader user-email)
   (token
    :initarg :token
    :type string
    :reader user-token)
   (username
    :initarg :username
    :type string
    :reader user-username)
   (bio
    :initarg :bio
    :initform nil
    :type (or null string)
    :reader user-bio)
   (image
    :initarg :image
    :initform nil
    :type (or null string)
    :reader user-image))
  (:documentation "A representation of a user."))

(defun make-user (email token username &key bio image)
  (check-type email string)
  (check-type token string)
  (check-type username string)
  (make-instance 'user
                 :email email
                 :token token
                 :username username
                 :bio bio
                 :image image))

(defmethod jojo:%to-json ((object user))
  (with-slots (email token username bio image) object
    (jojo:with-object
      (jojo:write-key-value "email" email)
      (jojo:write-key-value "token" token)
      (jojo:write-key-value "username" username)
      (jojo:write-key-value "bio" (or bio :null))
      (jojo:write-key-value "image" (or image :null)))))
