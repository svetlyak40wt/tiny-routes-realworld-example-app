;;;; conduit.lisp
(in-package :cl-user)
(uiop:define-package :conduit
  (:use :cl)
  (:local-nicknames (:log :conduit.logger)
                    (:auth :conduit.auth)
                    (:db :conduit.db)
                    (:routes :conduit.routes))
  (:import-from :clack)
  (:export #:start-app
           #:host
           #:port
           #:debug
           #:db-path
           #:secret))

(in-package :conduit)

(defvar *http-server* nil
  "The application's HTTP server.")

(defun stop-http-server ()
  (when *http-server*
    (clack:stop *http-server*)
    (setf *http-server* nil)))

(defun start-http-server (handler &key host port debug)
  (let ((port (or port 8080)))
    (stop-http-server)
    (setf *http-server*
          (clack:clackup handler :address host :port port :debug debug))
    (log:info "Successfully initialized server on port ~a" port)))

(defun start-app (&key (host "127.0.0.1") (port 8080) (debug t) (db-path "db/conduit.db") (secret "changeit"))
  (log:initialize-logger)
  (auth:initialize-auth secret)
  (db:initialize-db
   (ensure-directories-exist (asdf:system-relative-pathname :conduit db-path)))
  (start-http-server routes:app-routes :host host :port port :debug debug)
  t)
