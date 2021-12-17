(in-package :conduit)

(defclass profile ()
  ((username
    :initarg :username
    :type string
    :reader profile-username)
   (bio
    :initarg :bio
    :initform nil
    :type (or null string)
    :reader profile-bio)
   (image
    :initarg :image
    :initform nil
    :type (or null string)
    :reader profile-image)
   (following
    :initarg :following
    :initform nil
    :type boolean
    :reader profile-following))
  (:documentation "A representation of a profile."))

(defun make-profile (username &key bio image following)
  (make-instance 'profile
                 :username (check-username username)
                 :bio bio
                 :image image
                 :following following))

(defun parse-profile (options)
  (make-profile
   (getf options :|username|)
   :bio (getf options :|bio|)
   :image (getf options :|image|)
   :following (getf options :|following|)))

(defmethod print-object ((object profile) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (username) object
      (format stream ":username ~s" username))))

(defmethod jojo:%to-json ((object profile))
  (with-slots (username bio image following) object
    (jojo:with-object
      (jojo:write-key-value "username" username)
      (jojo:write-key-value "bio" (or bio :null))
      (jojo:write-key-value "image" (or image :null))
      (jojo:write-key-value "following" (or following :false)))))
