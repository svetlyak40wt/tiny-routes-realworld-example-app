;;;; routes.lisp
(in-package :cl-user)
(uiop:define-package :conduit.routes
  (:use :cl :conduit.types)
  (:local-nicknames (:users :conduit.services.users)
                    (:articles :conduit.services.articles)
                    (:profiles :conduit.services.profiles))
  (:import-from :conduit.util
                #:with-json
                #:decode-json-value)
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
                #:wrap-query-parameters)
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
  (define-post "/api/users/login" (request)
    (with-request (json-body) request
      (with-json (user) json-body
        (with-json (email password) user
          (if-let ((user (users:login email password)))
            (ok (list :|user| user))
            (forbidden "Invalid credentials"))))))

  (define-post "/api/users" (request)
    (with-request (json-body) request
      (let ((rendition (decode-json-value json-body 'user-registration-rendition :wrapped-in :|user|)))
        (if-let ((user (users:register-user rendition)))
          (ok (list :|user| user))
          (unprocessable-entity "Unable to register user"))))))

(define-routes private-user-routes
  (define-get "/api/user" (request)
    (with-request (auth) request
      (if-let ((user (users:current-user auth)))
        (ok (list :|user| user))
        (not-found "No such user found"))))

  (define-put "/api/users" (request)
    (with-request (auth json-body) request
      (let ((rendition (decode-json-value json-body 'user-update-rendition :wrapped-in :|user|)))
        (if-let ((updated-user (users:update-user auth rendition)))
          (ok (list :|user| updated-user))
          (unprocessable-entity "Unable to update user"))))))

(define-routes public-profile-routes
  (define-get "/api/profiles/:username" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (username) path-parameters
        (if-let ((profile (profiles:profile-by-username auth username)))
          (ok (list :|profile| profile))
          (not-found "No such profile found"))))))

(define-routes private-profile-routes
  (define-post "/api/profiles/:username/follow" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (username) path-parameters
        (if-let ((profile (profiles:follow-profile auth username)))
          (ok (list :|profile| profile))
          (not-found "No such profile found")))))

  (define-delete "/api/profiles/:username/follow" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (username) path-parameters
        (if-let ((profile (profiles:unfollow-profile auth username)))
          (ok (list :|profile| profile))
          (not-found "No such profile found"))))))

(define-routes public-article-routes
  (define-get "/api/articles/:slug" (request)
    (with-request (path-parameters) request
      (with-path-parameters (slug) path-parameters
        (if-let ((article (articles:article-by-slug slug)))
          (ok (list :|article| article))
          (not-found "No such article found")))))

  (define-get "/api/articles" (request)
    (with-request (query-parameters) request
      (let* ((article-query (parse-article-query query-parameters))
             (articles (articles:get-articles article-query)))
        (ok (list :|articles| articles :|article-query| article-query)))))

  (define-get "/api/articles/:slug/comments" (request)
    (with-request (path-parameters) request
      (with-path-parameters (slug) path-parameters
        (let ((comments (articles:get-comments-by-article-slug slug)))
          (ok (list :|comments| comments))))))

  (define-get "/api/tags" ()
    (let ((tags (articles:get-tags)))
      (ok (list :|tags| tags)))))

(define-routes private-article-routes
  (define-get "/api/articles/feed" (request)
    (with-request (auth) request
      (let ((articles (articles:article-feed auth)))
        (ok (list :|articles| articles)))))

  (define-put "/api/articles/:slug" (request)
    (with-request (auth path-parameters json-body) request
      (with-path-parameters (slug) path-parameters
        (let* ((rendition (decode-json-value json-body 'article-update-rendition :wrapped-in :|article|))
               (article (articles:update-article auth slug rendition)))
          (ok (list :|article| article))))))

  (define-post "/api/articles" (request)
    (with-request (auth json-body) request
      (let* ((rendition (decode-json-value json-body 'article-rendition :wrapped-in :|article|))
             (article (articles:create-article auth rendition)))
        (ok (list :|article| article)))))

  (define-delete "/api/articles/:slug" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (slug) path-parameters
        (let ((article (articles:delete-article auth slug)))
          (ok (list :|article| article))))))

  (define-post "/api/articles/:slug/comments" (request)
    (with-request (auth path-parameters json-body) request
      (with-path-parameters (slug) path-parameters
        (let* ((rendition (decode-json-value json-body 'comment-rendition :wrapped-in :|comment|))
               (comment (articles:create-comment auth slug rendition)))
          (ok (list :|comment| comment))))))

  (define-delete "/api/articles/:slug/comments/:comment-id" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (slug comment-id) path-parameters
        (let ((comment (articles:delete-comment auth slug (parse-integer (string comment-id)))))
          (ok (list :|comment| comment))))))

  (define-post "/api/articles/:slug/favorite" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (slug) path-parameters
        (let ((favorited-article (articles:favorite-article auth slug)))
          (ok (list :|article| favorited-article))))))

  (define-post "/api/articles/:slug/unfavorite" (request)
    (with-request (auth path-parameters) request
      (with-path-parameters (slug) path-parameters
        (let ((unfavorited-article (articles:unfavorite-article auth slug)))
          (ok (list :|article| unfavorited-article)))))))

(define-routes api-routes
  public-user-routes
  public-profile-routes

  (pipe private-user-routes
    (wrap-auth))
  (pipe private-profile-routes
    (wrap-auth))
  (pipe private-article-routes
    (wrap-auth))

  (pipe public-article-routes
    (wrap-query-parameters))

  (define-route () (not-found "NOT_FOUND")))

(define-routes app-routes
  (pipe api-routes
    (wrap-request-json-body)
    (wrap-condition)
    (wrap-response-json-body)))
