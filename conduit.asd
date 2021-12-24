;;;; conduit.asd

(asdf:defsystem #:conduit
  :description "A reference backend implementation of the RealWorld example app leveraging Clack and tiny-routes."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :license  "MIT"
  :version "0.0.1"
  :pathname "src/"
  :serial t
  :depends-on
  (:alexandria
   :cl-base64
   :cl-bcrypt
   :cl-ppcre
   :cl-slug
   :clack
   :ironclad
   :jonathan
   :local-time
   :tiny-routes)
  :components
  ((:file "package")
   (:file "errors")
   (:file "util")
   (:file "jwt")
   (:file "auth")
   (:file "validators")
   (:file "types")
   (:module "services"
    :serial t
    :components ((:file "users")
                 (:file "profiles")
                 (:file "articles")))
   (:file "middleware")
   (:file "routes")
   (:file "conduit")))
