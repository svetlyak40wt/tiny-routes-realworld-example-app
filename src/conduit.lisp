;;;; conduit.lisp
(in-package :cl-user)
(uiop:define-package :conduit
  (:use :cl)
  (:local-nicknames (:log :conduit.logger)
                    (:auth :conduit.auth)
                    (:db :conduit.db)
                    (:routes :conduit.routes))
  (:import-from :clack)
  (:export #:start-app))

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
  (log:initialize-logger)
  (auth:initialize-auth "to-be-replaced-with-secret-key-text")
  (db:initialize-db "conduit.db")
  (start-http-server routes:app-routes)
  t)

(start-app)
