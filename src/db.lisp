;;;; db.lisp
(in-package :cl-user)
(uiop:define-package :conduit.db
  (:use :cl :conduit.types)
  (:local-nicknames (:auth :conduit.auth)
                    (:log :conduit.logger)
                    (:util :conduit.util))
  (:import-from :cl-dbi)
  (:import-from :alexandria
                #:when-let
                #:when-let*)
  (:export #:user-by-email
           #:user-by-id
           #:insert-user
           #:update-user
           #:profile-by-username
           #:user-follows-p
           #:follow-user
           #:unfollow-user
           #:article-by-slug
           #:get-articles
           #:get-feed
           #:insert-article
           #:update-article
           #:delete-article
           #:favorited-article-p
           #:favorite-article
           #:unfavorite-article
           #:comment-by-id
           #:comments-by-article-slug
           #:insert-comment
           #:delete-comment
           #:get-tags
           #:initialize-db))

(in-package :conduit.db)

(defvar *db* nil
  "The application's database instance.")

(defvar *sql-database-table-definitions* (make-hash-table :test #'eq)
  "The hash table containing all SQL database table definitions.")

(defmacro define-database-table (name definition &optional documentation)
  `(progn
     (defparameter ,name ,definition ,@(list documentation))
     (setf (gethash ',name *sql-database-table-definitions*) ,definition)
     ',name))

(define-database-table *sql-create-users-table* "
  CREATE TABLE IF NOT EXISTS users (
    user_id        INTEGER PRIMARY KEY AUTOINCREMENT,
    username       VARCHAR(80) NOT NULL,
    email          VARCHAR(255) NOT NULL,
    password_hash  VARCHAR(255) NOT NULL,
    bio            VARCHAR(1024),
    image          VARCHAR(255),
    created_at     TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at     TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT     user_username_unique UNIQUE (username),
    CONSTRAINT     user_email_unique UNIQUE (email))")

(define-database-table *sql-create-articles-table* "
  CREATE TABLE IF NOT EXISTS articles (
    article_id   INTEGER PRIMARY KEY AUTOINCREMENT,
    slug         VARCHAR(255) NOT NULL,
    title        VARCHAR(255) NOT NULL,
    author_id    INTEGER NOT NULL,
    description  VARCHAR(255) NOT NULL,
    body         TEXT NOT NULL,
    created_at   TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at   TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY  (author_id) REFERENCES users(id),
    CONSTRAINT   article_slug_unique UNIQUE(slug))")

(define-database-table *sql-create-tags-table* "
  CREATE TABLE IF NOT EXISTS tags (
    tag_id      INTEGER PRIMARY KEY AUTOINCREMENT,
    name        VARCHAR(255) NOT NULL,
    CONSTRAINT  tag_name_unique UNIQUE(name))")

(define-database-table *sql-create-article-tag-association-table* "
  CREATE TABLE IF NOT EXISTS article_tag_associations (
    article_tag_id  INTEGER PRIMARY KEY AUTOINCREMENT,
    article_id      INTEGER NOT NULL,
    tag_id          INTEGER NOT NULL,
    FOREIGN KEY     (article_id) REFERENCES articles(id),
    FOREIGN KEY     (tag_id) REFERENCES tags(id),
    CONSTRAINT      article_tag_id_unique UNIQUE (article_id, tag_id))")

(define-database-table *sql-create-comments-table* "
  CREATE TABLE IF NOT EXISTS comments (
    comment_id   INTEGER PRIMARY KEY AUTOINCREMENT,
    body         VARCHAR(4096) NOT NULL,
    article_id   INTEGER NOT NULL,
    author_id    INTEGER NOT NULL,
    created_at   TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    updated_at   TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY  (article_id) REFERENCES articles(id),
    FOREIGN KEY  (author_id) REFERENCES users(id))")

(define-database-table *sql-create-user-follow-associations-table* "
  CREATE TABLE IF NOT EXISTS user_follow_associations (
    user_follow_id  INTEGER PRIMARY KEY AUTOINCREMENT,
    follower_id     INTEGER NOT NULL,
    followed_id     INTEGER NOT NULL,
    FOREIGN KEY     (follower_id) REFERENCES users(id),
    FOREIGN KEY     (followed_id) REFERENCES users(id),
    CONSTRAINT      follow_associations_unique UNIQUE (follower_id, followed_id))")

(define-database-table *sql-create-article-favorite-associations-table* "
  CREATE TABLE IF NOT EXISTS article_favorite_associations (
    article_favorite_id  INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id              INTEGER NOT NULL,
    article_id           INTEGER NOT NULL,
    FOREIGN KEY          (user_id) REFERENCES users(id),
    FOREIGN KEY          (article_id) REFERENCES articles(id),
    CONSTRAINT           favorite_associations_unique UNIQUE (user_id, article_id))")

(define-database-table *sql-create-profiles-view* "
  CREATE VIEW IF NOT EXISTS v_profiles
  AS
  SELECT
    u.user_id AS profile_id,
    u.username,
    u.bio,
    u.image,
    u.created_at AS profile_created_at,
    u.updated_at AS profile_updated_at,
    group_concat(ufa.follower_id) AS followed_by
  FROM
    users u
    LEFT JOIN user_follow_associations ufa ON ufa.followed_id = u.user_id
  GROUP BY u.user_id")

(define-database-table *sql-create-articles-view* "
  CREATE VIEW IF NOT EXISTS v_articles
  AS
  SELECT
    a.article_id,
    a.slug,
    a.title,
    a.description,
    a.body,
    group_concat (DISTINCT t.name) AS tags,
    a.created_at AS article_created_at,
    a.updated_at AS article_updated_at,
    group_concat (DISTINCT afa.user_id) AS favorited_by,
    count(DISTINCT afa.user_id) AS favorites_count,
    p.profile_id,
    p.username,
    p.bio,
    p.image,
    p.profile_created_at,
    p.profile_updated_at,
    p.followed_by
  FROM
    articles a
    INNER JOIN v_profiles p ON p.profile_id = a.author_id
    LEFT JOIN article_tag_associations ta ON ta.article_id = a.article_id
    LEFT JOIN tags t ON t.tag_id = ta.tag_id
    LEFT JOIN article_favorite_associations afa ON afa.article_id = a.article_id
  GROUP BY a.article_id")

(define-database-table *sql-create-article-comments-view* "
  CREATE VIEW IF NOT EXISTS v_article_comments AS
  SELECT
    c.comment_id,
    c.body,
    c.created_at AS comment_created_at,
    c.updated_at AS comment_updated_at,
    a.article_id,
    a.slug,
    p.profile_id,
    p.username,
    p.bio,
    p.image,
    p.profile_created_at,
    p.profile_updated_at,
    p.followed_by
  FROM
    comments c
    INNER JOIN v_profiles p ON p.profile_id = c.author_id
    INNER JOIN articles a ON a.article_id = c.article_id
  GROUP BY
    c.comment_id")

;;; util
(defun execute (sql &rest args)
  (dbi:do-sql *db* sql args))

(defun fetch (query)
  (dbi:fetch query))

(defun query (sql &rest args)
  (dbi:execute (dbi:prepare *db* sql) args))

(defun query-one (sql &rest args)
  (dbi:fetch (apply #'query sql args)))

(defun query-one* (class sql &rest args)
  (when-let ((row (apply #'query-one sql args)))
    (util:decode-value class row)))

(defun query-all (row-mapper sql &rest args)
  (let ((query (apply #'query sql args)))
    (loop for row = (dbi:fetch query)
          while row collect (funcall row-mapper row))))

(defun query-all* (class sql &rest args)
  (let ((query (apply #'query sql args)))
    (loop for row = (dbi:fetch query)
          while row collect (util:decode-value class row))))

;;; users
(defun user-by-email (email)
  (log:info :db "Attempting to find user by email ~s" email)
  (let ((user (query-one* 'user "SELECT * FROM users WHERE email = ?" email)))
    (log:info :db "Found user with email ~s: ~s" email user)
    user))

(defun user-by-id (id)
  (log:info :db "Attempting to find user by user id ~a" id)
  (let ((user (query-one* 'user "SELECT * FROM users WHERE user_id = ?" id)))
    (log:info :db "Found user with id ~a: ~a" id user)
    user))

(defun insert-user (rendition)
  (log:info :db "Attempting to insert user via rendition ~a" rendition)
  (with-slots (username email password bio image) rendition
    (let ((user (query-one*
                 'user
                 "INSERT INTO users (username, email, password_hash, bio, image)
                  VALUES (?,?,?,?,?) RETURNING *"
                 username email (auth:hash-encode-password password) bio image)))
      (log:info :db "Inserted user ~a" user)
      user)))

(defun update-user (rendition user-id)
  (log:info :db "Attempting to update user with id ~a via rendition ~a" user-id rendition)
  (with-slots (username email password bio image) rendition
    (let ((user (query-one*
                 'user
                 "UPDATE users SET username=COALESCE(?,username), email=COALESCE(?,email),
                  password_hash=COALESCE(?,password_hash), bio=COALESCE(?,bio),
                  image=COALESCE(?,image), updated_at=current_timestamp WHERE user_id = ? RETURNING *"
                 username email (and password (auth:hash-encode-password password)) bio image user-id)))
      (log:info "Updated user ~a" user)
      user)))

;;; profiles
(defun profile-id-by-username (username)
  (log:info "Attempting to get profile id for username ~a" username)
  (when-let ((row (query-one "SELECT profile_id FROM v_profiles WHERE username = ?" username)))
    (getf row :|profile_id|)))

(defun profile-by-id (profile-id &optional user-id)
  (log:info :db "Attempting to get profile for id ~a for user id ~a" profile-id user-id)
  (let* ((sql "SELECT *,
                 CASE WHEN ','||followed_by||',' LIKE '%,~a,%' THEN 'T' ELSE 'F' END AS following
               FROM v_profiles
               WHERE profile_id = ?")
         (sql (format nil sql user-id))
         (profile (query-one* 'profile sql profile-id)))
    (log:info :db "Found profile with id ~a: ~a" profile-id profile)
    profile))

(defun profile-by-username (username &optional user-id)
  (log:info :db "Attempting to get profile for username ~a for user id ~a" username user-id)
  (let* ((sql "SELECT *,
                 CASE WHEN ','||followed_by||',' LIKE '%,~a,%' THEN 'T' ELSE 'F' END AS following
               FROM v_profiles
               WHERE username = ?")
         (sql (format nil sql user-id))
         (profile (query-one* 'profile sql username)))
    (log:info :db "Found profile with username ~a: ~a" username profile)
    profile))

(defun user-follows-p (follower-id followed-id)
  (log:info :db "Attempting to determine if ~a follows ~a" follower-id followed-id)
  (let ((followsp (and (query-one "SELECT follower_id FROM user_follow_associations
                                   WHERE follower_id = ? AND followed_id = ?" follower-id followed-id)
                       t)))
    (log:info :db "Determined user ~a ~:[does NOT follow~;follows~] user ~a"
              followed-id followsp followed-id)
    followsp))

(defun follow-user (follower-id to-follow-id)
  (log:info :db "Attempting to insert follow association between ~a and ~a" follower-id to-follow-id)
  (unless (user-follows-p follower-id to-follow-id)
    (execute "INSERT INTO user_follow_associations(follower_id, followed_id) VALUES (?,?)"
             follower-id to-follow-id)))

(defun unfollow-user (follower-id followed-id)
  (log:info :db "Attempting to delete follow association between ~a and ~a" follower-id followed-id)
  (when (user-follows-p follower-id followed-id)
    (execute "DELETE FROM user_follow_associations WHERE follower_id = ? AND followed_id = ?"
             follower-id followed-id)))

;;; articles
(defun article-id-by-slug (slug)
  (log:info :db "Attempting to get article if for slug ~a" slug)
  (when-let ((row (query-one "SELECT article_id FROM articles WHERE slug = ?" slug)))
    (getf row :|article_id|)))

(defun article-by-id (article-id &optional user-id)
  (log:info :db "Attempting to get article for id ~a for user id ~a" article-id user-id)
  (let* ((sql "SELECT *,
                 CASE WHEN ','||followed_by||',' LIKE '%,~a,%' THEN 'T' ELSE 'F' END AS following,
                 CASE WHEN ','||favorited_by||',' LIKE '%,~a,%' THEN 'T' ELSE 'F' END AS favorited
               FROM v_articles a
               WHERE article_id = ?")
         (sql (format nil sql user-id user-id))
         (article (query-one* 'article sql article-id)))
    (log:info :db "Found article with id ~a: ~a" article-id article)
    article))

(defun article-by-slug (slug &optional user-id)
  (log:info :db "Attempting to get article for slug ~a for user id ~a" slug user-id)
  (let* ((sql "SELECT *,
                 CASE WHEN ','||followed_by||',' LIKE '%,~a,%' THEN 'T' ELSE 'F' END AS following,
                 CASE WHEN ','||favorited_by||',' LIKE '%,~a,%' THEN 'T' ELSE 'F' END AS favorited
               FROM v_articles a
               WHERE slug = ?")
         (sql (format nil sql user-id user-id))
         (article (query-one* 'article sql slug)))
    (log:info :db "Found article with slug ~a: ~a" slug article)
    article))

(defun canonicalize-article-query-clauses (article-query)
  (with-slots (tag author favorited limit offset) article-query
    (let ((clauses '()))
      (when tag
        (push (format nil "','||tags||',' LIKE '%,~a,%'" tag) clauses))
      (when author
        (push (format nil "username = '~a'" author) clauses))
      (when favorited
        (when-let ((favorited-profile-id (profile-id-by-username favorited)))
          (push (format nil "','||favorited_by||',' LIKE '%,~a,%'" favorited-profile-id) clauses)))
      (if clauses
          (format nil "WHERE ~{~a~^ AND ~} LIMIT ~d OFFSET ~d" (nreverse clauses) limit offset)
          (format nil "LIMIT ~d OFFSET ~d" limit offset)))))

(defun get-articles (article-query &optional user-id)
  (log:info :db "Attempting to get articles for query ~a for user id ~a" article-query user-id)
  (let* ((sql "SELECT *,
                 CASE WHEN ','||followed_by||',' LIKE '%,~a,%' THEN 'T' ELSE 'F' END AS following,
                 CASE WHEN ','||favorited_by||',' LIKE '%,~a,%' THEN 'T' ELSE 'F' END AS favorited
               FROM v_articles a ~a")
         (sql (format nil sql user-id user-id (canonicalize-article-query-clauses article-query)))
         (articles (query-all* 'article sql)))
    (log:info :db "Found ~d article~:p: ~s" (length articles) (mapcar #'id articles))
    articles))

(defun get-feed (feed-query user-id)
  (log:info :db "Attempting to get feed for query ~a for user id ~a" feed-query user-id)
  (let* ((sql "SELECT *,
                 CASE WHEN ','||followed_by||',' LIKE '%,~a,%' THEN 'T' ELSE 'F' END AS following,
                 CASE WHEN ','||favorited_by||',' LIKE '%,~a,%' THEN 'T' ELSE 'F' END AS favorited
               FROM v_articles a
               WHERE following = 'T' ")
         (sql (format nil sql user-id user-id))
         (articles (query-all* 'article sql)))
    (log:info :db "Found ~d feed article~:p: ~s" (length articles) (mapcar #'id articles))
    articles))

(defun insert-article-tag (article-id tag-name)
  (let ((tag (or (query-one "SELECT * from tags where name = ?" tag-name)
                 (query-one "INSERT INTO tags (name) VALUES (?) RETURNING *" tag-name))))
    (util:with-options (tag-id) tag
      (execute "INSERT INTO article_tag_associations (article_id, tag_id)
                VALUES (?,?)" article-id tag-id))))

(defun insert-article (rendition author-id)
  (log:info :db "Attempting to insert article for user ~a via rendition ~a" author-id rendition)
  ;; First, insert the article itself
  (with-slots (title description body tags) rendition
    (let* ((slug (cl-slug:slugify title))
           (row (query-one "INSERT INTO articles (slug, title, author_id, description, body)
                           VALUES (?,?,?,?,?) returning *" slug title author-id description body)))
      (let* ((profile (profile-by-id author-id))
             (article (util:decode-value 'article (append (list :author profile :tags tags) row))))
        ;; Second, insert corresponding tags
        (util:with-options (article-id) row
          (dolist (tag tags)
            (insert-article-tag article-id tag)))
        ;; Third, return the article instance
        article))))

(defun update-article (slug rendition user-id)
  (log:info :db "Attempting to update article ~a for user ~a via rendition ~a" slug user-id rendition)
  (with-slots (title description body) rendition
    (let ((new-slug (or (and title (cl-slug:slugify title)) slug)))
      (when-let ((row (query-one "UPDATE articles SET slug=COALESCE(?,slug), title=COALESCE(?,title),
                                  description=COALESCE(?,description), body=COALESCE(?,body)
                                  WHERE slug = ? RETURNING article_id" new-slug title description body slug)))
        (log:info :db "Updated article ~a" new-slug)
        (article-by-slug new-slug user-id)))))

(defun delete-article (slug user-id)
  (log:info :db "Attempting to delete article ~a for user ~a" slug user-id)
  (when-let ((article (article-by-slug slug user-id))
             (row (query-one "DELETE FROM articles WHERE slug = ? RETURNING slug" slug)))
    (log:info :db "Deleted article ~a" slug)
    article))

(defun favorited-article-p (article-id user-id)
  (log:info :db "Attempting to determine if user ~a favorited article ~a" user-id article-id)
  (let ((favoritedp (and (query-one "SELECT * FROM article_favorite_associations
                                     WHERE user_id = ? AND article_id = ?" user-id article-id)
                         t)))
    (log:info :db "Determined user ~a ~:[did NOT favorite~;favorited~] article ~a"
              user-id favoritedp article-id)
    favoritedp))

(defun favorite-article (article-id user-id)
  (log:info :db "Attempting to favorite article ~a for user ~a" article-id user-id)
  (unless (favorited-article-p article-id user-id)
    (execute "INSERT INTO article_favorite_associations (user_id, article_id) VALUES (?,?)" user-id article-id)))

(defun unfavorite-article (article-id user-id)
  (log:info :db "Attempting to unfavorite article ~a for user ~a" article-id user-id)
  (when (favorited-article-p article-id user-id)
    (execute "DELETE FROM article_favorite_associations WHERE user_id = ? AND article_id = ?" user-id article-id)))

;;; article-comments
(defun comment-by-id (comment-id &optional user-id)
  (log:info :db "Attempting to get comment for id ~a for user id ~a" comment-id user-id)
  (let* ((sql "SELECT *,
                 CASE WHEN ','||followed_by||',' LIKE '%,~a,%' THEN 'T' ELSE 'F' END AS following
               FROM v_article_comments
               WHERE comment_id = ?")
         (sql (format nil sql user-id))
         (comment (query-one* 'comment sql comment-id)))
    (log:info :db "Found comment with id ~a: ~a" comment-id comment)
    comment))

(defun comments-by-article-slug (slug &optional user-id)
  (log:info :db "Attempting to get comments for slug ~a for user id ~a" slug user-id)
  (let* ((sql "SELECT *,
                 CASE WHEN ','||followed_by||',' LIKE '%,~a,%' THEN 'T' ELSE 'F' END AS following
               FROM v_article_comments
               WHERE slug = ?")
         (sql (format nil sql user-id))
         (comments (query-all* 'comment sql slug)))
    (log:info :db "Found ~d comment~:p: ~s" (length comments) (mapcar #'id comments))
    comments))

(defun insert-comment (slug rendition author-id)
  (log:info :db "Attempting to insert comment for article ~a user ~a via rendition ~a" slug author-id rendition)
  (when-let ((article-id (article-id-by-slug slug)))
    (with-slots (body) rendition
      (let* ((row (query-one "INSERT INTO comments(body, article_id, author_id) VALUES (?,?,?) RETURNING comment_id"
                             body article-id author-id))
             (comment (comment-by-id (getf row :|comment_id|) author-id)))
        comment))))

(defun delete-comment (slug comment-id user-id)
  ;; TODO: Determine if only comment owner should delete comment
  (log:info :db "Attempting to delete comment ~a for article ~a and user ~a" comment-id slug user-id)
  (when-let* ((article-id (article-id-by-slug slug))
              (comment (comment-by-id comment-id user-id))
              (row (query-one "DELETE FROM comments WHERE comment_id = ? AND article_id = ?
                                RETURNING comment_id" comment-id article-id)))
    (log:info :db "Deleted comment ~a" comment-id)
    comment))

;;; tags
(defun get-tags ()
  (log:info :db "Attempting to retrieve tags")
  (flet ((get-tag (row) (getf row :|name|)))
    (let ((tags (query-all #'get-tag "SELECT name FROM tags")))
      (log:info :db "Found ~d tag~:p: ~s" (length tags) tags)
      tags)))

;;; top level
(defun ensure-database-tables ()
  (loop for sql being the hash-value of *sql-database-table-definitions*
        do (execute sql))
  (log:info :db "Ensured all database tables are created"))

(defun disconnect-db ()
  (when *db*
    (dbi:disconnect *db*)
    (setf *db* nil)))

(defun initialize-db (database-name)
  (disconnect-db)
  (setf *db* (dbi:connect :sqlite3 :database-name database-name))
  (ensure-database-tables)
  (log:info :db "Successfully initialized database"))
