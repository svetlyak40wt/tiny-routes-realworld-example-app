;;;; profiles.lisp
(in-package :cl-user)
(uiop:define-package :conduit.services.profiles
  (:use :cl :conduit.types)
  (:local-nicknames (:db :conduit.db)
                    (:log :conduit.logger))
  (:import-from :conduit.util
                #:with-options)
  (:import-from :alexandria
                #:when-let)
  (:export #:profile-by-username
           #:follow-profile
           #:unfollow-profile))

(in-package :conduit.services.profiles)

(defun profile-by-username (auth username)
  (with-options ((user-id id)) auth
    (log:info :profiles "Attempting to get profile by username ~a for user ~a" username user-id)
    (when-let ((profile (db:profile-by-username username user-id)))
      (log:info :profiles "Found profile ~a" profile)
      profile)))

(defun follow-profile (auth username)
  (with-options ((user-id id)) auth
    (log:info :profiles "Attempting to follow profile ~a for user ~a" username user-id)
    (when-let ((profile (db:profile-by-username username user-id)))
      (with-slots ((profile-id id) following) profile
        (db:follow-user user-id profile-id)
        (setf following t))
      profile)))

(defun unfollow-profile (auth username)
  (with-options ((user-id id)) auth
    (log:info :profiles "Attempting to unfollow profile ~a for user ~a" username user-id)
    (when-let ((profile (db:profile-by-username username)))
      (with-slots ((profile-id id) following) profile
        (db:unfollow-user user-id profile-id)
        (setf following nil))
      (log:info :profiles "Unfollowed profile ~a for user ~a" username user-id)
      profile)))
