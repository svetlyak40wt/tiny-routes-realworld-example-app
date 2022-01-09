;;;; types.lisp
(in-package :cl-user)
(uiop:define-package :conduit.types
  (:use :cl)
  (:import-from :conduit.util
                #:format-timestamp
                #:parse-timestamp)
  (:import-from :conduit.validators
                #:check-username
                #:check-email
                #:check-password
                #:check-id
                #:check-article-body
                #:check-description
                #:check-profile
                #:check-slug
                #:check-comment-body
                #:check-title)
  (:export #:id
           #:created-at
           #:updated-at
           #:token)
  (:export #:user-registration-rendition
           #:username
           #:email
           #:password
           #:bio
           #:image
           #:make-user-registration-rendition
           #:parse-user-registration-rendition
           #:user
           #:password-hash
           #:make-user
           #:parse-user
           #:authenticated-user
           #:make-authenticated-user
           #:user-update-rendition
           #:make-user-update-rendition
           #:parse-user-update-rendition)
  (:export #:profile
           #:following
           #:make-profile
           #:parse-profile
           #:article
           #:slug
           #:author
           #:title
           #:description
           #:body
           #:tags
           #:favorited
           #:favorites-count
           #:make-article
           #:article-query
           #:limit
           #:offset
           #:make-article-query
           #:parse-article-query
           #:article-rendition
           #:make-article-rendition
           #:parse-article-rendition
           #:article-update-rendition
           #:make-article-update-rendition
           #:parse-article-update-rendition)
  (:export #:comment
           #:make-comment
           #:parse-comment
           #:comment-rendition
           #:make-comment-rendition
           #:parse-comment-rendition))

(in-package :conduit.types)

;;; mixins
(defclass entity-mixin ()
  ((id :initarg :id :type id :reader id)
   (created-at :initarg :created-at :type timestamp :reader created-at)
   (updated-at :initarg :updated-at :type timestamp :reader updated-at))
  (:documentation "A mixin for identifiable entities."))

(defclass authenticated-mixin ()
  ((token :initarg :token :type string :reader auth-token))
  (:documentation "A mixin for authenticated objects."))

;;; users
(defclass user-registration-rendition ()
  ((username :initarg :username :type string :reader username)
   (email :initarg :email :type string :reader email)
   (password :initarg :password :type string :reader password)
   (bio :initarg :bio :type (or null string) :reader bio)
   (image :initarg :image :type (or null string) :reader image))
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

(defclass user (entity-mixin)
  ((username :initarg :username :type string :reader username)
   (email :initarg :email :type string :reader email)
   (password-hash :initarg :password-hash :type string :reader password-hash)
   (bio :initarg :bio :initform nil :type (or null string) :reader bio)
   (image :initarg :image :initform nil :type (or null string) :reader image))
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

(defclass user-update-rendition ()
  ((username :initarg :username :type (or null string) :reader username)
   (email :initarg :email :type (or null string) :reader email)
   (password :initarg :password :type (or null string) :reader password)
   (bio :initarg :bio :type (or null string) :reader bio)
   (image :initarg :image :type (or null string) :reader image))
  (:documentation "A representation of a user update rendition."))

(defun make-user-update-rendition (&optional username email password bio image)
  (make-instance 'user-update-rendition
                 :username (and username (check-username username))
                 :email (and email (check-email email))
                 :password (and password (check-password password))
                 :bio bio
                 :image image))

(defun parse-user-update-rendition (options)
  (make-user-update-rendition
   (getf options :|username|)
   (getf options :|email|)
   (getf options :|password|)
   (getf options :|bio|)
   (getf options :|image|)))

(defmethod print-object ((object user-update-rendition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (username email) object
      (format stream ":username ~s :email ~s" username email))))

;;; profiles
(defclass profile ()
  ((username :initarg :username :type string :reader username)
   (bio :initarg :bio :initform nil :type (or null string) :reader bio)
   (image :initarg :image :initform nil :type (or null string) :reader image)
   (following :initarg :following :initform nil :type boolean :reader following))
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

;;; articles
(defclass article (entity-mixin)
  ((slug :initarg :slug :type string :reader slug)
   (author :initarg :author :type profile :reader author)
   (title :initarg :title :type string :reader title)
   (description :initarg :description :type string :reader description)
   (body :initarg :body :type string :reader body)
   (tags :initarg :tags :type list :reader tags)
   (favorited :initarg :favorited :initform nil :type boolean :reader favorited)
   (favorites-count :initarg :favorites-count :initform 0 :type fixnum :reader favorites-count))
  (:documentation "A representation of an article."))

(defun make-article (id author slug title description
                     &key (body "") tags favorited (favorites-count 0)
                       created-at updated-at)
  (let* ((created-at (or created-at (local-time:now)))
         (updated-at (or updated-at created-at)))
    (make-instance 'article
                   :id (check-id id)
                   :author (check-profile author "author")
                   :slug (check-slug slug)
                   :title (check-title title)
                   :description (check-description description)
                   :body (check-article-body body)
                   :tags tags
                   :favorited favorited
                   :favorites-count favorites-count
                   :created-at (parse-timestamp created-at)
                   :updated-at (parse-timestamp updated-at))))

(defmethod print-object ((object article) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (id title author) object
      (format stream ":id ~s :title ~s :author-name ~s"
              id title (username author)))))

(defmethod jojo:%to-json ((timestamp local-time:timestamp))
  (jojo:%to-json (format-timestamp timestamp)))

(defmethod jojo:%to-json ((object article))
  (with-slots (slug title description body tags created-at updated-at
               favorited favorites-count author)
      object
    (jojo:with-object
      (jojo:write-key-value "slug" slug)
      (jojo:write-key-value "title" title)
      (jojo:write-key-value "description" description)
      (jojo:write-key-value "body" body)
      (jojo:write-key-value "tagList" tags)
      (jojo:write-key-value "createdAt" created-at)
      (jojo:write-key-value "updatedAt" updated-at)
      (jojo:write-key-value "favorited" (or favorited :false))
      (jojo:write-key-value "favoritesCount" favorites-count)
      (jojo:write-key-value "author" author))))

(defclass article-query ()
  ((tag :initarg :tag :type (or null string) :reader tag)
   (author :initarg :author :type (or null string) :reader author)
   (favorited :initarg :favorited :type (or null string) :reader favorited)
   (limit :initarg :limit :type fixnum :reader limit)
   (offset :initarg :offset :type fixnum :reader offset))
  (:documentation "A representation of an article query"))

(defun make-article-query (&key tag author favorited (limit 20) (offset 0))
  (make-instance 'article-query
                 :tag tag
                 :author author
                 :favorited favorited
                 :limit (or (and limit (parse-integer limit :junk-allowed t)) 20)
                 :offset (or (and offset (parse-integer offset :junk-allowed t)) 0)))

(defun parse-article-query (options)
  (make-article-query
   :tag (getf options :|tag|)
   :author (getf options :|author|)
   :favorited (getf options :|favorited|)
   :limit (getf options :|limit|)
   :offset (getf options :|offset|)))

(defmethod print-object ((object article-query) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (tag author favorited limit offset) object
      (format stream ":tag ~s :author ~a :favorited ~s :limit ~d :offset ~d"
              tag author favorited limit offset))))

(defmethod jojo:%to-json ((object article-query))
  (with-slots (tag author favorited limit offset) object
    (jojo:with-object
      (when tag (jojo:write-key-value "tag" tag))
      (when author (jojo:write-key-value "author" author))
      (when favorited (jojo:write-key-value "favorited" favorited))
      (jojo:write-key-value "limit" limit)
      (jojo:write-key-value "offset" offset))))

(defclass article-rendition ()
  ((title :initarg :title :type string :reader title)
   (description :initarg :description :type string :reader description)
   (body :initarg :body :type string :reader body)
   (tags :initarg :tags :type string :reader tags))
  (:documentation "A representation of an article rendition."))

(defun make-article-rendition (title description body tags)
  (make-instance 'article-rendition
                 :title (check-title title)
                 :description (check-description description)
                 :body (check-article-body body)
                 :tags tags))

(defun parse-article-rendition (options)
  (make-article-rendition
   (getf options :|title|)
   (getf options :|description|)
   (getf options :|body|)
   (or (getf options :|tags|)
       (getf options :|tagList|))))

(defmethod print-object ((object article-rendition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (title description) object
      (format stream ":title ~s :description ~s" title description))))

(defclass article-update-rendition ()
  ((title :initarg :title :type (or null string) :reader title)
   (description :initarg :description :type (or null string) :reader description)
   (body :initarg :body :type (or null string) :reader body))
  (:documentation "A representation of an article update rendition."))

(defun make-article-update-rendition (title description body)
  (make-instance 'article-update-rendition
                 :title (and title (check-title title))
                 :description (and description (check-description description))
                 :body (and body (check-article-body body))))

(defun parse-article-update-rendition (options)
  (make-article-update-rendition
   (getf options :|title|)
   (getf options :|description|)
   (getf options :|body|)))

(defmethod print-object ((object article-update-rendition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (title description) object
      (format stream ":title ~s :description ~s" title description))))

;;; comments
(defclass comment (entity-mixin)
  ((author :initarg :author :type profile :reader author)
   (body :initarg :body :type string :reader body))
  (:documentation "A representation of a comment."))

(defun make-comment (id author &key (body "") created-at updated-at)
  (let* ((created-at (or created-at (local-time:now)))
         (updated-at (or updated-at created-at)))
    (make-instance 'comment
                   :id (check-id id)
                   :body (check-comment-body body)
                   :author (check-profile author "author")
                   :created-at (parse-timestamp created-at)
                   :updated-at (parse-timestamp updated-at))))

(defun parse-comment (options)
  (make-comment
   (getf options :|id|)
   (getf options :|author|)
   :body (getf options :|body|)
   :created-at (getf options :|created_at|)
   :updated-at (getf options :|updated_at|)))

(defmethod print-object ((object comment) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (id author) object
      (format stream ":id ~s :author-name ~s"
              id (username author)))))

(defmethod jojo:%to-json ((object comment))
  (with-slots (id author body created-at updated-at) object
    (jojo:with-object
      (jojo:write-key-value "id" id)
      (jojo:write-key-value "body" body)
      (jojo:write-key-value "createdAt" created-at)
      (jojo:write-key-value "updatedAt" updated-at)
      (jojo:write-key-value "author" author))))

(defclass comment-rendition ()
  ((body :initarg :body :type string :reader body))
  (:documentation "A representation of a comment rendition."))

(defun make-comment-rendition (body)
  (make-instance 'comment-rendition :body (check-comment-body body)))

(defun parse-comment-rendition (options)
  (make-comment-rendition (getf options :|body|)))

(defmethod print-object ((object comment-rendition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (body) object
      (format stream ":body ~s" (subseq body 0 (min 10 (length body)))))))
