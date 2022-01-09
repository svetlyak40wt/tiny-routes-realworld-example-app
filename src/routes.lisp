;;;; routes.lisp
(in-package :cl-user)
(uiop:define-package :conduit.routes
  (:use :cl :conduit.types)
  (:local-nicknames (:users :conduit.services.users)
                    (:articles :conduit.services.articles)
                    (:profiles :conduit.services.profiles))
  (:import-from :conduit.util
                #:with-json)
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
                #:query-params
                #:wrap-query-params)
  (:import-from :tiny-routes
                #:define-routes
                #:define-route
                #:define-get
                #:define-post
                #:define-put
                #:define-delete
                #:pipe)
  (:export #:api-routes))

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
    (let* ((json-body (json-body request))
           (user (getf json-body :|user|))
           (user (users:login (getf user :|email|) (getf user :|password|))))
      (if user
          (ok (list :|user| user))
          (forbidden "Invalid credentials"))))

  (define-post "/api/users" (request)
    (let* ((json-body (json-body request))
           (rendition (parse-user-registration-rendition (getf json-body :|user|)))
           (user (users:register-user rendition)))
      (if user
          (ok (list :|user| user))
          (unprocessable-entity "Unable to register user")))))

(define-routes private-user-routes
  (define-get "/api/user" (request)
    (let* ((id (auth-get request :|id| ""))
           (token (tiny:request-get request :token))
           (user (users:current-user id token)))
      (if user
          (ok (list :|user| user))
          (not-found "No such user found"))))

  (define-put "/api/users" (request)
    (let* ((id (auth-get request :|id| ""))
           (token (tiny:request-get request :token))
           (json-body (json-body request))
           (rendition (parse-user-update-rendition (getf json-body :|user|)))
           (updated-user (users:update-user id token rendition)))
      (if updated-user
          (ok (list :|user| updated-user))
          (unprocessable-entity "Unable to update user")))))

(define-routes public-profile-routes
  (define-get "/api/profiles/:USERNAME" (request)
    (let* ((id (auth-get request :|id| ""))
           (username (tiny:path-param request :username  "none"))
           (profile (profiles:profile-by-username id username)))
      (if profile
          (ok (list :|profile| profile))
          (not-found "No such profile found")))))

(define-routes private-profile-routes
  (define-post "/api/profiles/:USERNAME/follow" (request)
    (let* ((id (auth-get request :|id| ""))
           (username (tiny:path-param request :username "none"))
           (profile (profiles:follow-profile id username)))
      (if profile
          (ok (list :|profile| profile))
          (not-found "No such profile found"))))

  (define-delete "/api/profiles/:USERNAME/follow" (request)
    (let* ((id (auth-get request :|id| ""))
           (username (tiny:path-param request :username "none"))
           (profile (profiles:unfollow-profile id username)))
      (if profile
          (ok (list :|profile| profile))
          (not-found "No such profile found")))))

(define-routes public-article-routes
  (define-get "/api/articles/:SLUG" (request)
    (let* ((slug (tiny:path-param request :slug))
           (article (articles:article-by-slug slug)))
      (if article
          (ok (list :|article| article))
          (not-found "No such article found"))))

  (define-get "/api/articles" (request)
    (let* ((params (query-params request))
           (article-query (parse-article-query params))
           (articles (articles:get-articles article-query)))
      (ok (list :|articles| articles :|article-query| article-query))))

  (define-get "/api/articles/:SLUG/comments" (request)
    (let* ((slug (tiny:path-param request :slug))
           (comments (articles:get-comments-by-article-slug slug)))
      (ok (list :|comments| comments))))

  (define-get "/api/tags" ()
    (let ((tags (articles:get-tags)))
      (ok (list :|tags| tags)))))

(define-routes private-article-routes
  (define-get "/api/articles/feed" (request)
    (let* ((id (auth-get request :|id| ""))
           (articles (articles:article-feed id)))
      (ok (list :|articles| articles))))

  (define-put "/api/articles/:SLUG" (request)
    (let* ((id (auth-get request :|id| ""))
           (slug (tiny:path-param request :slug))
           (rendition (parse-article-update-rendition (getf (json-body request) :|article|)))
           (article (articles:update-article id slug rendition)))
      (ok (list :|article| article))))

  (define-post "/api/articles" (request)
    (let* ((id (auth-get request :|id| ""))
           (rendition (parse-article-rendition (getf (json-body request) :|article|)))
           (article (articles:create-article id rendition)))
      (ok (list :|article| article))))

  (define-delete "/api/articles/:SLUG" (request)
    (let* ((id (auth-get request :|id| ""))
           (slug (tiny:path-param request :slug))
           (article (articles:delete-article id slug)))
      (ok (list :|article| article))))

  (define-post "/api/articles/:SLUG/comments" (request)
    (let* ((id (auth-get request :|id| ""))
           (slug (tiny:path-param request :slug))
           (rendition (parse-comment-rendition (getf (json-body request) :|comment|)))
           (comment (articles:create-comment id slug rendition)))
      (ok (list :|comment| comment))))

  (define-delete "/api/articles/:SLUG/comments/:comment-id" (request)
    (let* ((id (auth-get request :|id| ""))
           (slug (tiny:path-param request :slug))
           (comment-id (parse-integer (tiny:path-param request "comment-id")))
           (comment (articles:delete-comment id slug comment-id)))
      (ok (list :|comment| comment))))

  (define-post "/api/articles/:SLUG/favorite" (request)
    (let* ((id (auth-get request :|id| ""))
           (slug (tiny:path-param request :slug))
           (favorited-article (articles:favorite-article id slug)))
      (ok (list :|article| favorited-article))))

  (define-post "/api/articles/:SLUG/unfavorite" (request)
    (let* ((id (auth-get request :|id| ""))
           (slug (tiny:path-param request :slug))
           (unfavorited-article (articles:unfavorite-article id slug)))
      (ok (list :|article| unfavorited-article)))))

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
    (wrap-query-params))

  (define-route () (not-found "NOT_FOUND")))

(define-routes app-routes
  (pipe api-routes
    (wrap-request-json-body)
    (wrap-condition)
    (wrap-response-json-body)))
