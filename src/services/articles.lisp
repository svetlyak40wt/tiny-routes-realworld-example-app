;;;; articles.lisp
(in-package :cl-user)
(uiop:define-package :conduit.services.articles
  (:use :cl :conduit.types)
  (:import-from :conduit.services.profiles
                #:profile-by-id)
  (:export #:get-articles
           #:article-by-slug
           #:article-feed
           #:create-article
           #:update-article
           #:delete-article
           #:get-comments-by-article-slug
           #:create-comment
           #:delete-comment
           #:favorite-article
           #:unfavorite-article
           #:get-tags))

(in-package :conduit.services.articles)

(defun get-articles (article-query)
  (declare (ignore article-query))
  (let ((author (make-profile "jruiz")))
    (list (make-article 19 author "slug" "another title" "description" :body ""))))

(defun article-by-slug (slug)
  (let ((author (make-profile "jruiz")))
    (list (make-article 19 author slug "another title" "description" :body ""))))

(defun article-feed (auth)
  (declare (ignore auth))
  (let ((author (make-profile "jruiz")))
    (list (make-article 19 author "slug" "another title" "description" :body ""))))

(defun create-article (auth rendition)
  (declare (ignore auth))
  (with-slots (title description body tags) rendition
    (let ((author (make-profile "jruiz"))
          (slug (cl-slug:slugify title)))
      (make-article 21 author slug title description :body body :tags tags))))

(defun update-article (auth slug rendition)
  (declare (ignore auth slug))
  (with-slots (title description body) rendition
    (let* ((author (make-profile "jruiz"))
           (title (or title "def"))
           (slug (cl-slug:slugify title)))
      (make-article 21 author slug title
                    (or description "some description")
                    :body (or body "some body")))))

(defun delete-article (auth slug)
  (declare (ignore auth))
  (let ((author (make-profile "jruiz")))
    (list (make-article 19 author slug "another title" "description" :body ""))))

(defun get-comments-by-article-slug (slug)
  (declare (ignore slug))
  (let ((author (make-profile "somebody")))
    (list (make-comment 101 author :body "Who said that?")
          (make-comment 102 author :body "It was me!"))))

(defun create-comment (id slug rendition)
  (declare (ignore slug))
  (let ((author (profile-by-id id))
        (body (body rendition)))
    (make-comment 103 author :body body)))

(defun delete-comment (id slug comment-id)
  (declare (ignore slug))
  (let ((author (profile-by-id id)))
    (make-comment comment-id author :body "Test")))

(defun favorite-article (id slug)
  (declare (ignore id))
  (let ((author (make-profile "jruiz")))
    (list (make-article 19 author slug "another title" "description" :body "" :favorited t))))

(defun unfavorite-article (id slug)
  (declare (ignore id))
  (let ((author (make-profile "jruiz")))
    (list (make-article 19 author slug "another title" "description" :body "" :favorited nil))))

(defun get-tags ()
  (list "reactjs" "angularjs"))
