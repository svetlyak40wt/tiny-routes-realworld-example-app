(in-package :conduit)

(defun articles/get-articles (article-query)
  (declare (ignore article-query))
  (let ((author (make-profile "jruiz")))
    (list (make-article 19 author "slug" "another title" "description" :body ""))))

(defun articles/article-by-slug (slug)
  (let ((author (make-profile "jruiz")))
    (list (make-article 19 author slug "another title" "description" :body ""))))

(defun articles/article-feed (id)
  (declare (ignore id))
  (let ((author (make-profile "jruiz")))
    (list (make-article 19 author "slug" "another title" "description" :body ""))))

(defun articles/create-article (id rendition)
  (declare (ignore id))
  (with-slots (title description body tags) rendition
    (let ((author (make-profile "jruiz"))
          (slug (cl-slug:slugify title)))
      (make-article 21 author slug title description :body body :tags tags))))

(defun articles/update-article (id slug rendition)
  (declare (ignore id slug))
  (with-slots (title description body) rendition
    (let* ((author (make-profile "jruiz"))
           (title (or title "def"))
           (slug (cl-slug:slugify title)))
      (make-article 21 author slug title
                    (or description "some description")
                    :body (or body "some body")))))

(defun articles/delete-article (id slug)
  (declare (ignore id))
  (let ((author (make-profile "jruiz")))
    (list (make-article 19 author slug "another title" "description" :body ""))))

(defun articles/get-comments-by-article-slug (slug)
  (declare (ignore slug))
  (let ((author (make-profile "somebody")))
    (list (make-comment 101 author :body "Who said that?")
          (make-comment 102 author :body "It was me!"))))
