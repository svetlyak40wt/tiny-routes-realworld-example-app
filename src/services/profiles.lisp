;;;; profiles.lisp
(in-package :cl-user)
(uiop:define-package :conduit.services.profiles
  (:use :cl)
  (:import-from :conduit.types
                #:make-profile)
  (:export #:profile-by-username
           #:profile-by-id
           #:follow-profile
           #:unfollow-profile))

(in-package :conduit.services.profiles)

(defun profile-by-username (auth username)
  (declare (ignore auth))
  (and username (make-profile username)))

(defun profile-by-id (auth)
  (and auth (make-profile "jruiz")))

(defun follow-profile (auth username)
  (declare (ignore auth))
  (and username (make-profile username :following t)))

(defun unfollow-profile (auth username)
  (declare (ignore auth))
  (and username (make-profile username :following nil)))
