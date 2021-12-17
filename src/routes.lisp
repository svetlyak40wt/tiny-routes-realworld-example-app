(in-package :conduit)

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
           (user (users/login-user (getf user :|email|) (getf user :|password|))))
      (if user
          (ok (list :|user| user))
          (forbidden "Invalid credentials"))))

  (define-post "/api/users" (request)
    (let* ((json-body (json-body request))
           (rendition (parse-user-registration-rendition (getf json-body :|user|)))
           (user (users/register-user rendition)))
      (if user
          (ok (list :|user| user))
          (unprocessable-entity "Unable to register user")))))

(define-routes private-user-routes
  (define-get "/api/user" (request)
    (let* ((id (claims-get request :|id| ""))
           (token (tiny:request-get request :token))
           (user (users/current-user id token)))
      (if user
          (ok (list :|user| user))
          (not-found "No such user found"))))

  (define-put "/api/users" (request)
    (let* ((id (claims-get request :|id| ""))
           (token (tiny:request-get request :token))
           (json-body (json-body request))
           (rendition (parse-user-update-rendition (getf json-body :|user|)))
           (updated-user (users/update-user id token rendition)))
      (if updated-user
          (ok (list :|user| updated-user))
          (unprocessable-entity "Unable to update user")))))

(define-routes public-profile-routes
  (define-get "/api/profiles/:username" (request)
    (let* ((id (claims-get request :|id| ""))
           (username (tiny:request-path-param request "username" "none"))
           (profile (profiles/profile-by-username id username)))
      (if profile
          (ok (list :|profile| profile))
          (not-found "No such profile found")))))

(define-routes private-profile-routes
  (define-post "/api/profiles/:username/follow" (request)
    (let* ((id (claims-get request :|id| ""))
           (username (tiny:request-path-param request "username" "none"))
           (profile (profiles/follow-profile id username)))
      (if profile
          (ok (list :|profile| profile))
          (not-found "No such profile found"))))

  (define-delete "/api/profiles/:username/follow" (request)
    (let* ((id (claims-get request :|id| ""))
           (username (tiny:request-path-param request "username" "none"))
           (profile (profiles/unfollow-profile id username)))
      (if profile
          (ok (list :|profile| profile))
          (not-found "No such profile found")))))

(define-routes api-routes
  public-user-routes
  (tiny:pipe private-user-routes
    (wrap-auth))
  public-profile-routes
  (tiny:pipe private-profile-routes
    (wrap-auth))
  (define-route () (not-found "NOT_FOUND")))

(define-routes app-routes
  (tiny:pipe api-routes
    (wrap-request-json-body)
    (wrap-condition)
    (wrap-response-json-body)))
