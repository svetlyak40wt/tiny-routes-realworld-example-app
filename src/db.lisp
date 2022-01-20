;;;; db.lisp
(in-package :cl-user)
(uiop:define-package :conduit.db
  (:use :cl :conduit.types)
  (:local-nicknames (:auth :conduit.auth)
                    (:log :conduit.logger))
  (:import-from :cl-dbi)
  (:export #:initialize-db
           #:user-by-id
           #:user-by-email
           #:insert-user
           #:update-user))

(in-package :conduit.db)

(defvar *db* nil
  "The application's database instance.")

(defun disconnect-db ()
  (when *db*
    (dbi:disconnect *db*)
    (setf *db* nil)))

;; query
(defun query (sql &rest args)
  (dbi:execute (dbi:prepare *db* sql) args))

(defun query-single (row-mapper sql &rest args)
  (let ((row (dbi:fetch (apply #'query sql args))))
    (when row
      (funcall row-mapper row))))

(defun query-all (row-mapper sql &rest args)
  (let ((query (apply #'query sql args)))
    (loop for row = (dbi:fetch query)
          while row
          collect (funcall row-mapper row))));

(defun user-by-id (id)
  (let ((user (query-single #'parse-user "SELECT * FROM users WHERE id = ?" id)))
    (log:info "Found user with id ~a: ~a" id user)
    user))

(defun user-by-email (email)
  (let ((user (query-single #'parse-user "SELECT * FROM users WHERE email = ?" email)))
    (log:info "Found user with email ~a: ~a" email user)
    user))

(defun insert-user (rendition)
  (log:info :db "Attempting to insert user from rendition ~a" rendition)
  (with-slots (username email password bio image) rendition
    (query-single #'parse-user
                  "INSERT INTO users (username, email, password_hash, bio, image) VALUES (?,?,?,?,?) RETURNING *"
                  username email (auth:hash-encode-password password) bio image)))

(defun update-user (id rendition)
  (log:info :db "Attempting to update user with id ~a via rendition ~a" id rendition)
  (with-slots (username email password bio image) rendition
    (query-single #'parse-user
                  "UPDATE users SET username=COALESCE(?,username), email=COALESCE(?,email), password_hash=COALESCE(?,password_hash),
                   bio=COALESCE(?,bio), image=COALESCE(?,image), last_updated_at=current_timestamp WHERE id = ? RETURNING *"
                  username email (and password (auth:hash-encode-password password)) bio image id)))

(defun initialize-db (database-name)
  (disconnect-db)
  (setf *db* (dbi:connect :sqlite3 :database-name database-name)))
