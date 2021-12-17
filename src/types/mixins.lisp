(in-package :conduit)

(defclass entity-mixin ()
  ((id
    :initarg :id
    :type id
    :reader id)
   (created-at
    :initarg :created-at
    :type timestamp
    :reader created-at)
   (updated-at
    :initarg :updated-at
    :type timestamp
    :reader updated-at))
  (:documentation "A mixin for identifiable entities."))

(defclass authenticated-mixin ()
  ((token
    :initarg :token
    :type string
    :reader auth-token))
  (:documentation "A mixin for authenticated objects."))
