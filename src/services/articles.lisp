;;;; articles.lisp
(in-package :cl-user)
(uiop:define-package :conduit.services.articles
  (:use :cl :conduit.types)
  (:local-nicknames (:db :conduit.db)
                    (:log :conduit.logger)
                    (:profiles :conduit.services.profiles))
  (:import-from :conduit.util
                #:with-options)
  (:import-from :alexandria
                #:when-let)
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

(defun article-by-slug (slug)
  (log:info :articles "Attempting to find article by slug ~s" slug)
  (when-let ((article (db:article-by-slug slug)))
    (log:info :articles "Found article by slug ~a: ~a" slug article)
    article))

(defun get-articles (auth article-query)
  (check-type article-query article-query)
  (with-options ((user-id id)) auth
    (log:info :articles "Attempting to get articles for user ~a via query ~a" user-id article-query)
    (let ((articles (db:get-articles article-query user-id)))
      articles)))

(defun article-feed (auth feed-query)
  (check-type feed-query feed-query)
  (with-options ((user-id id)) auth
    (log:info "Attempting to get feed articles for user ~a via query ~a" user-id feed-query)
    (let ((articles (db:get-feed feed-query user-id)))
      articles)))

(defun create-article (auth rendition)
  (check-type rendition article-rendition)
  (with-options ((user-id id)) auth
    (log:info :articles "Attempting to create article for user ~a via rendition ~a" user-id rendition)
    (when-let ((article (db:insert-article rendition user-id)))
      (log:info :articles "Inserted article ~a" article)
      article)))

(defun update-article (auth slug rendition)
  (check-type rendition article-update-rendition)
  (with-options ((user-id id)) auth
    (log:info :articles "Attempting to update article ~a via rendition ~a for user ~a" slug rendition user-id)
    (when-let ((article (db:update-article slug rendition user-id)))
      (log:info :articles "Successfully updated article ~a" article)
      article)))

(defun delete-article (auth slug)
  (with-options ((user-id id)) auth
    (log:info :articles "Attempting to delete article ~a for user ~a" slug user-id)
    (when-let ((article (db:delete-article slug user-id)))
      (log:info :articles "Successfully deleted article ~a" article)
      article)))

(defun get-comments-by-article-slug (auth slug)
  (with-options ((user-id id)) auth
    (log:info :articles "Attempting to get comments for article ~a and user ~a" slug user-id)
    (let ((comments (db:comments-by-article-slug slug user-id)))
      comments)))

(defun create-comment (auth slug rendition)
  (check-type rendition comment-rendition)
  (with-options ((user-id id)) auth
    (log:info :articles "Attempting to create comment for article ~a via rendition ~a for user ~a"
              slug rendition user-id)
    (when-let ((comment (db:insert-comment slug rendition user-id)))
      (log:info :articles "Successfully created comment ~a" comment)
      comment)))

(defun delete-comment (auth slug comment-id)
  (with-options ((user-id id)) auth
    (log:info :articles "Attempting to delete comment ~a in article ~a for user ~a"
              comment-id slug user-id)
    (when-let ((comment (db:delete-comment slug comment-id user-id)))
      (log:info :articles "Successfully deleted comment ~a" comment)
      comment)))

(defun favorite-article (auth slug)
  (with-options ((user-id id)) auth
    (log:info :articles "Attempting to favorite article ~a for user ~a" slug user-id)
    (when-let ((article (db:article-by-slug slug)))
      (with-slots ((article-id id) favorited favorites-count) article
        (db:favorite-article article-id user-id)
        (setf favorited t)
        (incf favorites-count))
      article)))

(defun unfavorite-article (auth slug)
  (with-options ((user-id id)) auth
    (log:info :articles "Attempting to unfavorite article ~a for user ~a" slug user-id)
    (when-let ((article (db:article-by-slug slug)))
      (with-slots ((article-id id) favorited favorites-count) article
        (db:unfavorite-article article-id user-id)
        (setf favorited nil)
        (decf favorites-count))
      article)))

(defun get-tags ()
  (log:info :articles "Attempting to get tags")
  (let ((tags (db:get-tags)))
    (log:info :articles "Found tags")
    tags))
