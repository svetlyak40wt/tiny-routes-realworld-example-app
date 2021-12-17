(in-package :conduit)

(defclass user-registration-rendition ()
  ((email
    :initarg :email
    :type string
    :reader user-registration-email)
   (username
    :initarg :username
    :type string
    :reader user-registration-username)
   (password
    :initarg :password
    :type string
    :reader user-registration-password)
   (bio
    :initarg :bio
    :type (or null string)
    :reader user-registration-bio)
   (image
    :initarg :image
    :type (or null string)
    :reader user-registration-image))
  (:documentation "A representation of a user registration rendition."))

(defun make-user-registration-rendition (email username password &optional bio image)
  (check-type email string)
  (check-type username string)
  (check-type password string)
  (make-instance 'user-registration-rendition
                 :email email
                 :username username
                 :password password
                 :bio bio
                 :image image))

(defun parse-user-registration-rendition (options)
  (make-user-registration-rendition
   (getf options :|email|)
   (getf options :|username|)
   (getf options :|password|)
   (getf options :|bio|)
   (getf options :|image|)))

(defmethod print-object ((object user-registration-rendition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (email username) object
      (format stream ":email ~s :username ~s " email username))))

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
