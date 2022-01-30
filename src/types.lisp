;;;; types.lisp
(in-package :cl-user)
(uiop:define-package :conduit.types
  (:use :cl)
  (:local-nicknames (:util :conduit.util))
  (:import-from :local-time)
  (:import-from :conduit.validators
                #:check-type*
                #:check-username
                #:check-email
                #:check-password
                #:check-id
                #:check-article-body
                #:check-description
                #:check-slug
                #:check-comment-body
                #:check-title)
  (:export #:id
           #:created-at
           #:updated-at)
  (:export #:authenticated-mixin
           #:token)
  (:export #:user-registration-rendition
           #:username
           #:email
           #:password
           #:bio
           #:image
           #:user
           #:password-hash
           #:authenticated-user
           #:user-update-rendition)
  (:export #:profile
           #:following
           #:article
           #:slug
           #:author
           #:title
           #:description
           #:body
           #:tags
           #:favorited
           #:favorites-count
           #:feed-query
           #:limit
           #:offset
           #:article-query
           #:tag
           #:favorited
           #:article-rendition
           #:article-update-rendition)
  (:export #:comment
           #:comment-rendition))

(in-package :conduit.types)

;;; mixins
(defclass entity-mixin ()
  ((id :initarg :id :initarg :|id|
                :type integer :reader id)
   (created-at :initarg :created-at :initarg :|created_at|
                        :type local-time:timestamp :reader created-at)
   (updated-at :initarg :updated-at :initarg :|updated_at|
                        :type local-time:timestamp :reader updated-at))
  (:default-initargs :created-at (local-time:now) :updated-at (local-time:now))
  (:documentation "A mixin for identifiable entities."))

(defmethod initialize-instance :after ((object entity-mixin) &key)
  (with-slots (id created-at updated-at) object
    (setf id (check-id id)
          created-at (util:parse-timestamp created-at)
          updated-at (util:parse-timestamp updated-at))))

(defclass authenticated-mixin ()
  ((token :type string :reader token))
  (:default-initargs :token "")
  (:documentation "A mixin for authenticated objects."))

;;; users
(defclass user-registration-rendition ()
  ((username :initarg :username :initarg :|username| :type string :reader username)
   (email :initarg :email :initarg :|email| :type string :reader email)
   (password :initarg :password :initarg :|password| :type string :reader password)
   (bio :initarg :bio :initarg :|bio| :type (or null string) :reader bio)
   (image :initarg :image :initarg :|image| :type (or null string) :reader image))
  (:default-initargs :bio nil :image nil)
  (:documentation "A representation of a user registration rendition."))

(defmethod initialize-instance :after ((object user-registration-rendition) &key)
  (with-slots (username email password) object
    (setf username (check-username username)
          email (check-email email)
          password (check-password password))))

(defmethod print-object ((object user-registration-rendition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (username email) object
      (format stream ":username ~s :email ~s" username email))))

(defclass user (entity-mixin)
  ((id :initarg :|user_id|)
   (username :initarg :username :initarg :|username| :type string :reader username)
   (email :initarg :email :initarg :|email| :type string :reader email)
   (password-hash :initarg :password-hash :initarg :|password_hash| :type string :reader password-hash)
   (bio :initarg :bio :initarg :|bio| :type (or null string) :reader bio)
   (image :initarg :image :initarg :|image| :type (or null string) :reader image)
   (created-at :initarg :|user_created_at|)
   (updated-at :initarg :|user_updated_at|))
  (:default-initargs :bio nil :image nil)
  (:documentation "A representation of a user."))

(defmethod initialize-instance :after ((object user) &key)
  (with-slots (username email) object
    (setf username (check-username username)
          email (check-email email))))

(defmethod print-object ((object user) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (id email username) object
      (format stream ":id ~s :email ~s :username ~s" id email username))))

(defclass authenticated-user (user authenticated-mixin)
  ((token :initarg :token :initarg :|token|))
  (:documentation "A representation of an authenticated user."))

(defmethod jojo:%to-json ((object authenticated-user))
  (with-slots (email token username bio image) object
    (jojo:with-object
      (jojo:write-key-value "email" email)
      (jojo:write-key-value "token" token)
      (jojo:write-key-value "username" username)
      (jojo:write-key-value "bio" (or bio :null))
      (jojo:write-key-value "image" (or image :null)))))

(defclass user-update-rendition ()
  ((username :initarg :username :initarg :|username| :type (or null string) :reader username)
   (email :initarg :email :initarg :|email| :type (or null string) :reader email)
   (password :initarg :password :initarg :|password| :type (or null string) :reader password)
   (bio :initarg :bio :initarg :|bio| :type (or null string) :reader bio)
   (image :initarg :image :initarg :|image| :type (or null string) :reader image))
  (:default-initargs :username nil :email nil :password nil :bio nil :image nil)
  (:documentation "A representation of a user update rendition."))

(defmethod initialize-instance :after ((object user-update-rendition) &key)
  (with-slots (username email password) object
    (setf username (and username (check-username username))
          email (and email (check-email email))
          password (and password (check-password password)))))

(defmethod print-object ((object user-update-rendition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (username email) object
      (format stream ":username ~s :email ~s" username email))))

;;; profiles
(defclass profile (entity-mixin)
  ((id :initarg :|profile_id|)
   (username :initarg :username :initarg :|username| :type string :reader username)
   (bio :initarg :bio :initarg :|bio| :type (or null string) :reader bio)
   (image :initarg :image :initarg :|image| :type (or null string) :reader image)
   (following :initarg :following :initarg :|following| :type boolean :accessor following)
   (created-at :initarg :|profile_created_at|)
   (updated-at :initarg :|profile_updated_at|))
  (:default-initargs :bio nil :image nil :following nil)
  (:documentation "A representation of a profile."))

(defmethod initialize-instance :after ((object profile) &key)
  (with-slots (username following) object
    (setf username (check-username username)
          following (util:parse-boolean following))))

(defmethod print-object ((object profile) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (username following) object
      (format stream ":username ~s :following ~s" username following))))

(defmethod jojo:%to-json ((object profile))
  (with-slots (username bio image following) object
    (jojo:with-object
      (jojo:write-key-value "username" username)
      (jojo:write-key-value "bio" (or bio :null))
      (jojo:write-key-value "image" (or image :null))
      (jojo:write-key-value "following" (or following :false)))))

;;; articles
(defclass article (entity-mixin)
  ((id :initarg :|article_id|)
   (slug :initarg :slug :initarg :|slug| :type string :reader slug)
   (author :initarg :author :initarg :|author| :type profile :accessor author)
   (title :initarg :title :initarg :|title| :type string :reader title)
   (description :initarg :description :initarg :|description| :type string :reader description)
   (body :initarg :body :initarg :|body| :type string :reader body)
   (tags :initarg :tags :initarg :|tags| :type list :reader tags)
   (favorited :initarg :favorited :initarg :|favorited| :type boolean :reader favorited)
   (favorites-count :initarg :favorites-count :initarg :|favorites_count| :type fixnum :accessor favorites-count)
   (created-at :initarg :|article_created_at|)
   (updated-at :initarg :|article_updated_at|))
  (:default-initargs :body "" :tags () :favorited nil :favorites-count 0)
  (:documentation "A representation of an article."))

(defmethod initialize-instance :after ((object article) &rest initargs)
  (unless (slot-boundp object 'author)
    (setf (author object) (apply #'make-instance 'profile :allow-other-keys t initargs)))
  (with-slots (author slug title description body tags favorited) object
    (setf slug (check-slug slug)
          author (check-type* author "author" 'profile)
          title (check-title title)
          description (check-description description)
          body (check-article-body body)
          tags (util:canonicalize-tags tags)
          favorited(util:parse-boolean favorited))))

(defmethod print-object ((object article) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (id title author) object
      (format stream ":id ~s :title ~s :author-name ~s"
              id title (username author)))))

(defmethod jojo:%to-json ((timestamp local-time:timestamp))
  (jojo:%to-json (util:format-timestamp timestamp)))

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

(defclass feed-query ()
  ((limit :initarg :limit :initarg :|limit| :type fixnum :reader limit)
   (offset :initarg :offset :initarg :|offset| :type fixnum :reader offset))
  (:default-initargs :limit 20 :offset 0)
  (:documentation "A representation of a feed query."))

(defmethod print-object ((object feed-query) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (limit offset) object
      (format stream ":limit ~d :offset ~d" limit offset))))

(defclass article-query (feed-query)
  ((tag :initarg :tag :initarg :|tag| :type (or null string) :reader tag)
   (author :initarg :author :initarg :|author| :type (or null string) :reader author)
   (favorited :initarg :favorited :initarg :|favorited| :type (or null string) :reader favorited))
  (:default-initargs :tag nil :author nil :favorited nil)
  (:documentation "A representation of an article query."))

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
  ((title :initarg :title :initarg :|title| :type string :reader title)
   (description :initarg :description :initarg :|description| :type string :reader description)
   (body :initarg :body :initarg :|body| :type string :reader body)
   (tags :initarg :tags :initarg :|tags| :initarg :|tagList| :type list :reader tags))
  (:default-initargs :body "" :tags ())
  (:documentation "A representation of an article rendition."))

(defmethod initialize-instance :after ((object article-rendition) &key)
  (with-slots (title description body tags) object
    (setf title (check-title title)
          description (check-description description)
          body (check-article-body body)
          tags (util:canonicalize-tags tags))))

(defmethod print-object ((object article-rendition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (title description) object
      (format stream ":title ~s :description ~s" title description))))

(defclass article-update-rendition ()
  ((title :initarg :title :initarg :|title| :type (or null string) :reader title)
   (description :initarg :description :initarg :|description| :type (or null string) :reader description)
   (body :initarg :body :initarg :|body| :type (or null string) :reader body))
  (:default-initargs :title nil :description nil :body nil)
  (:documentation "A representation of an article update rendition."))

(defmethod initialize-instance :after ((object article-update-rendition) &key)
  (with-slots (title description body) object
    (setf title (and title (check-title title))
          description (and description (check-description description))
          body (and body (check-article-body body)))))

(defmethod print-object ((object article-update-rendition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (title description) object
      (format stream ":title ~s :description ~s" title description))))

;;; comments
(defclass comment (entity-mixin)
  ((id :initarg :|comment_id|)
   (author :initarg :author :initarg :|author| :type profile :accessor author)
   (body :initarg :body :initarg :|body| :type string :reader body)
   (created-at :initarg :|comment_created_at|)
   (updated-at :initarg :|comment_updated_at|))
  (:default-initargs :body "")
  (:documentation "A representation of a comment."))

(defmethod initialize-instance :after ((object comment) &rest initargs)
  (unless (slot-boundp object 'author)
    (setf (author object) (apply #'make-instance 'profile :allow-other-keys t initargs)))
  (with-slots (author body) object
    (setf author (check-type* author "author" 'profile)
          body (check-comment-body body))))

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
  ((body :initarg :body :initarg :|body| :type string :reader body))
  (:default-initargs :body "")
  (:documentation "A representation of a comment rendition."))

(defmethod initialize-instance :after ((object comment-rendition) &key)
  (with-slots (body) object
    (setf body (check-comment-body body))))

(defmethod print-object ((object comment-rendition) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (with-slots (body) object
      (format stream ":body ~s" (subseq body 0 (min 10 (length body)))))))
