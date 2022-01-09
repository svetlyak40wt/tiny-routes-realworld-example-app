;;;; conduit.lisp
(in-package :cl-user)
(uiop:define-package :conduit
  (:use :cl)
  (:import-from :clack)
  (:import-from :conduit.auth
                #:initialize-auth)
  (:import-from :conduit.routes
                #:app-routes))

(in-package :conduit)

(defvar *http-server* nil
  "The application's HTTP server.")

(defun stop-http-server ()
  (when *http-server*
    (clack:stop *http-server*)
    (setf *http-server* nil)))

(defun start-http-server (handler &optional port)
  (stop-http-server)
  (setf *http-server*
        (clack:clackup handler :port (or port 8080))))

(defun start-app ()
  (initialize-auth "to-be-replaced-with-secret-key-text")
  (start-http-server app-routes)
  t)

(start-app)
