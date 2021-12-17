(in-package :conduit)

(defun format-timestamp (timestamp)
  "Return an ISO-8601 formatted string representing TIMESTAMP."
  (local-time:format-timestring nil timestamp :format local-time:+iso-8601-format+))

(defun parse-timestamp (timestamp-designator)
  (typecase timestamp-designator
    (null nil)
    (fixnum (local-time:universal-to-timestamp timestamp-designator))
    (string (local-time:parse-timestring timestamp-designator))
    (otherwise timestamp-designator)))

(defun unix-now ()
  (local-time:timestamp-to-unix (local-time:now)))
