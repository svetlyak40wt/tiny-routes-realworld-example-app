;;;; conduit.asd

(asdf:defsystem #:conduit
  :description "A reference backend implementation of the RealWorld example app leveraging Clack and tiny-routes."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :license  "MIT"
  :version "0.0.1"
  :pathname "src/"
  :serial t
  :depends-on
  (:clack
   :tiny-routes)
  :components
  ((:file "package")
   (:file "conduit")))
