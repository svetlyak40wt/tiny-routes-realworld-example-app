(in-package :conduit)

(defun profiles/profile-by-username (id username)
  (declare (ignore id))
  (and username (make-profile username)))

(defun profiles/profile-by-id (id)
  (and id (make-profile "jruiz")))

(defun profiles/follow-profile (id username)
  (declare (ignore id))
  (and username (make-profile username :following t)))

(defun profiles/unfollow-profile (id username)
  (declare (ignore id))
  (and username (make-profile username :following nil)))
