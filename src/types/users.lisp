(in-package :conduit)

;; user-registration-rendition
(defclass user-registration-rendition ()
  ((username
    :initarg :username
    :type string
    :reader user-registration-username)
   (email
    :initarg :email
    :type string
    :reader user-registration-email)
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

(defun make-user-registration-rendition (username email password &optional bio image)
  (make-instance 'user-registration-rendition
                 :username (check-username username)
                 :email (check-email email)
                 :password (check-password password)
                 :bio bio
                 :image image))

(defun parse-user-registration-rendition (options)
  (make-user-registration-rendition
   (getf options :|username|)
   (getf options :|email|)
   (getf options :|password|)
   (getf options :|bio|)
   (getf options :|image|)))

(defmethod print-object ((object user-registration-rendition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (username email) object
      (format stream ":username ~s :email ~s" username email))))

;; user
(defclass user (entity-mixin)
  ((username
    :initarg :username
    :type string
    :reader user-username)
   (email
    :initarg :email
    :type string
    :reader user-email)
   (password-hash
    :initarg :password-hash
    :type string
    :reader user-password-hash)
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

(defun make-user (id username email password-hash &key bio image created-at updated-at)
  (let* ((created-at (or created-at (local-time:now)))
         (updated-at (or updated-at created-at)))
    (make-instance 'user
                   :id (check-id id)
                   :username (check-username username)
                   :email (check-email email)
                   :password-hash password-hash
                   :bio bio
                   :image image
                   :created-at (parse-timestamp created-at)
                   :updated-at (parse-timestamp updated-at))))

(defun parse-user (options)
  (make-user
   (getf options :|id|)
   (getf options :|username|)
   (getf options :|email|)
   (getf options :|password_hash|)
   :bio (getf options :|bio|)
   :image (getf options :|image|)
   :created-at (getf options :|created_at|)
   :updated-at (getf options :|updated_at|)))

(defmethod print-object ((object user) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (id email username) object
      (format stream ":id ~s :email ~s :username ~s" id email username))))

;; authenticated-user
(defclass authenticated-user (user authenticated-mixin)
  ()
  (:documentation "A representation of an authenticated user."))

(defun make-authenticated-user (id token username email password-hash &key bio image created-at updated-at)
  (let* ((created-at (or created-at (local-time:now)))
         (updated-at (or updated-at created-at)))
    (make-instance 'authenticated-user
                   :id (check-id id)
                   :token token
                   :username (check-username username)
                   :email (check-email email)
                   :password-hash password-hash
                   :bio bio
                   :image image
                   :created-at created-at
                   :updated-at updated-at)))

(defmethod jojo:%to-json ((object authenticated-user))
  (with-slots (email token username bio image) object
    (jojo:with-object
        (jojo:write-key-value "email" email)
      (jojo:write-key-value "token" token)
      (jojo:write-key-value "username" username)
      (jojo:write-key-value "bio" (or bio :null))
      (jojo:write-key-value "image" (or image :null)))))
