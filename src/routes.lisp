;;;; routes.lisp
(in-package :cl-user)
(uiop:define-package :conduit.routes
  (:use :cl :conduit.types)
  (:local-nicknames (:users :conduit.services.users)
                    (:articles :conduit.services.articles)
                    (:profiles :conduit.services.profiles))
  (:import-from :conduit.util
                #:with-json
                #:decode-value
                #:parse-integer-safely)
  (:import-from :uiop
                #:if-let)
  (:import-from :conduit.middleware
                #:auth-get
                #:error-response
                #:json-body
                #:wrap-request-json-body
                #:wrap-response-json-body
                #:wrap-condition
                #:wrap-auth
                #:wrap-query-parameters
                #:wrap-logging)
  (:import-from :tiny-routes
                #:define-routes
                #:define-route
                #:define-get
                #:define-post
                #:define-put
                #:define-delete
                #:with-request
                #:with-path-parameters
                #:pipe)
  (:export #:app-routes))

(in-package :conduit.routes)

(defun ok (body)
  (tiny:ok body))

(defun forbidden (body)
  (tiny:forbidden (error-response body)))

(defun not-found (body)
  (tiny:not-found (error-response body)))

(defun unprocessable-entity (body)
  (tiny:unprocessable-entity (error-response body)))

(define-routes public-user-routes
  ;;; Authentication
  (define-post "/users/login" (request)
    (with-request (json-body) request
      (with-json (user) json-body
        (with-json (email password) user
          (if-let ((user (users:login email password)))
            (ok (list :|user| user))
            (forbidden "Invalid credentials"))))))

  ;;; Registration
  (define-post "/users" (request)
    (with-request (json-body) request
      (let ((rendition (decode-value 'user-registration-rendition json-body :wrapped-in :|user|)))
        (if-let ((user (users:register-user rendition)))
          (ok (list :|user| user))
          (unprocessable-entity "Unable to register user"))))))

(define-routes private-user-routes
  ;;; Get current user
  (define-get "/user" (request)
    (with-request (auth) request
      (if-let ((user (users:current-user auth)))
        (ok (list :|user| user))
        (not-found "No such user found"))))

  ;;; Update user
  (define-put "/user" (request)
    (with-request (auth json-body) request
      (let ((rendition (decode-value 'user-update-rendition json-body :wrapped-in :|user|)))
        (if-let ((updated-user (users:update-user auth rendition)))
          (ok (list :|user| updated-user))
          (unprocessable-entity "Unable to update user"))))))

(define-routes public-or-private-profile-routes
  ;;; Get profile
  (define-get "/profiles/:username" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (username) path-parameters
        (if-let ((profile (profiles:profile-by-username auth username)))
          (ok (list :|profile| profile))
          (not-found "No such profile found"))))))

(define-routes private-profile-routes
  ;;; Follow user
  (define-post "/profiles/:username/follow" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (username) path-parameters
        (if-let ((profile (profiles:follow-profile auth username)))
          (ok (list :|profile| profile))
          (not-found "No such profile found")))))

  ;;; Unfollow user
  (define-delete "/profiles/:username/follow" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (username) path-parameters
        (if-let ((profile (profiles:unfollow-profile auth username)))
          (ok (list :|profile| profile))
          (not-found "No such profile found"))))))

(define-routes public-article-routes
  (define-get "/articles/:slug" (request)
    (with-request (path-parameters) request
      (with-path-parameters (slug) path-parameters
        (if-let ((article (articles:article-by-slug slug)))
          (ok (list :|article| article))
          (not-found "No such article found")))))

  (define-get "/tags" ()
    (let ((tags (articles:get-tags)))
      (ok (list :|tags| tags)))))

(define-routes public-or-provide-article-routes
  ;;; List articles
  (define-get "/articles" (request)
    (with-request (auth query-parameters) request
      (let* ((article-query (decode-value 'article-query query-parameters))
             (articles (articles:get-articles auth article-query)))
        (ok (list :|articles| articles :|articlesCount| (length articles))))))

  ;;; Get comments from an article
  (define-get "/articles/:slug/comments" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (slug) path-parameters
        (let ((comments (articles:get-comments-by-article-slug auth slug)))
          (ok (list :|comments| comments)))))))

(define-routes private-article-routes
  ;;; Feed articles
  (define-get "/articles/feed" (request)
    (with-request (auth query-parameters) request
      (let* ((feed-query (decode-value 'feed-query query-parameters))
             (articles (articles:article-feed auth feed-query)))
        (ok (list :|articles| articles :|articlesCount| (length articles))))))

  ;;; Create article
  (define-post "/articles" (request)
    (with-request (auth json-body) request
      (let* ((rendition (decode-value 'article-rendition json-body :wrapped-in :|article|))
             (article (articles:create-article auth rendition)))
        (ok (list :|article| article)))))

  ;;; Update article
  (define-put "/articles/:slug" (request)
    (with-request (auth path-parameters json-body) request
      (with-path-parameters (slug) path-parameters
        (let* ((rendition (decode-value 'article-update-rendition json-body :wrapped-in :|article|))
               (article (articles:update-article auth slug rendition)))
          (ok (list :|article| article))))))

  ;;; Delete article
  (define-delete "/articles/:slug" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (slug) path-parameters
        (let ((article (articles:delete-article auth slug)))
          (ok (list :|article| article))))))

  ;;; Add comments to article
  (define-post "/articles/:slug/comments" (request)
    (with-request (auth path-parameters json-body) request
      (with-path-parameters (slug) path-parameters
        (let* ((rendition (decode-value 'comment-rendition json-body :wrapped-in :|comment|))
               (comment (articles:create-comment auth slug rendition)))
          (ok (list :|comment| comment))))))

  ;;; Delete comment
  (define-delete "/articles/:slug/comments/:comment-id" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (slug comment-id) path-parameters
        (let* ((comment-id (parse-integer-safely comment-id -1))
               (comment (articles:delete-comment auth slug comment-id)))
          (ok (list :|comment| comment))))))

  ;;; Favorite article
  (define-post "/articles/:slug/favorite" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (slug) path-parameters
        (let ((favorited-article (articles:favorite-article auth slug)))
          (ok (list :|article| favorited-article))))))

  ;;; Unfavorite article
  (define-delete "/articles/:slug/favorite" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (slug) path-parameters
        (let ((unfavorited-article (articles:unfavorite-article auth slug)))
          (ok (list :|article| unfavorited-article)))))))

(define-routes api-routes
  public-user-routes

  (pipe public-or-private-profile-routes
    (wrap-auth nil))
  (pipe public-or-provide-article-routes
    (wrap-auth nil))

  (pipe private-user-routes
    (wrap-auth t))
  (pipe private-profile-routes
    (wrap-auth t))
  (pipe private-article-routes
    (wrap-auth t))

  public-article-routes

  (define-route () (not-found "NOT_FOUND")))

(define-routes app-routes
  (pipe api-routes
    (wrap-request-json-body)
    (wrap-condition)
    (wrap-response-json-body)
    (wrap-logging)
    (wrap-query-parameters)))
