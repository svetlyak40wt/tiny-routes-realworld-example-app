;;;; util.lisp
(in-package :cl-user)
(uiop:define-package :conduit.util
  (:use :cl)
  (:import-from #:local-time)
  (:import-from #:alexandria
                #:starts-with)
  (:export #:format-timestamp
           #:parse-timestamp
           #:unix-now
           #:parse-boolean))

(in-package :conduit.util)

(defun format-timestamp (timestamp)
  "Return an ISO-8601 formatted string representing TIMESTAMP."
  (local-time:format-timestring nil timestamp :format local-time:+iso-8601-format+))

(defun parse-timestamp (timestamp-designator)
  "Return a timestamp corresponding to TIMESTAMP-DESIGNATOR."
  (typecase timestamp-designator
    (null nil)
    (fixnum (local-time:universal-to-timestamp timestamp-designator))
    (string (local-time:parse-timestring timestamp-designator))
    (otherwise timestamp-designator)))

(defun unix-now ()
  "Return the current unix timestamp."
  (local-time:timestamp-to-unix (local-time:now)))

(defun parse-boolean (boolean-designator)
  "Return a boolean corresponding to BOOLEAN-DESIGNATOR."
  (typecase boolean-designator
    (boolean boolean-designator)
    (fixnum (not (zerop boolean-designator)))
    (string (starts-with #\t boolean-designator :test #'char= :key #'char-downcase))
    (otherwise boolean-designator)))
