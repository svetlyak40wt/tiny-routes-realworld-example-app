(in-package :conduit)

(defun users/login-user (email password)
  (and email password (make-user email "TOKEN" "test-user")))
