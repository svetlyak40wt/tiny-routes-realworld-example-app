;;;; logger.lisp
(in-package :cl-user)
(uiop:define-package :conduit.logger
  (:use :cl)
  (:shadow #:error #:warn #:debug)
  ;; nickname v was removed from verbose and
  ;; verbose package was renamed to org.shirakumo.verbose
  ;; https://github.com/Shinmera/verbose/commit/356f362d7385b72cd3fad75d2b8d14834a74f8da
  ;; (:import-from :verbose)
  
  (:local-nicknames (#:v #:org.shirakumo.verbose))

  (:export #:debug
           #:info
           #:warn
           #:error
           #:initialize-logger))

(in-package :conduit.logger)

(defparameter *log-category* :conduit
  "The default log category when none is provided in the log.")

(defparameter *log-timestamp-format*
  (append
   local-time:+iso-8601-date-format+
   (list #\Space)
   '((:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:msec 3))))

(defmacro log-message (level datum &rest args)
  (if (symbolp datum)
      `(v:log ,level ,datum ,@args)
      `(v:log ,level *log-category* ,datum ,@args)))

(defmacro debug (datum &rest args)
  `(log-message :debug ,datum ,@args))

(defmacro info (datum &rest args)
  `(log-message :info ,datum ,@args))

(defmacro warn (datum &rest args)
  `(log-message :warn ,datum ,@args))

(defmacro error (datum &rest args)
  `(log-message :error ,datum ,@args))

(defun initialize-logger ()
  (setf v:*timestamp-format* *log-timestamp-format*)
  (info :logger "Successfully initialized logger"))
