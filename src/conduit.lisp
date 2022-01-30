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
  (let ((port (or port 8080)))
    (stop-http-server)
    (setf *http-server*
          (clack:clackup handler :port port))
    (log:info "Successfully initialized server on port ~a" port)))

(defun start-app ()
  (log:initialize-logger)
  (auth:initialize-auth "to-be-replaced-with-secret-key-text")
  (db:initialize-db (asdf:system-relative-pathname :conduit "db/conduit.db"))
  (start-http-server routes:app-routes)
  t)

(start-app)
