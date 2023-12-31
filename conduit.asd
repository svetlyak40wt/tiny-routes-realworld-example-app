;;;; conduit.asd

(asdf:defsystem #:conduit
  :description "A reference backend implementation of the RealWorld example app leveraging Clack and tiny-routes."
  :author "Johnny Ruiz <johnny@ruiz-usa.com>"
  :license  "MIT"
  :version "0.1.0"
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
   :tiny-routes
   :str
   :closer-mop
   :cl-dbi
   :verbose)
  :components
  ((:file "logger")
   (:file "errors")
   (:file "util")
   (:file "validators")
   (:file "types")
   (:file "jwt")
   (:file "auth")
   (:file "db")
   (:module "services"
    :serial t
    :components ((:file "users")
                 (:file "profiles")
                 (:file "articles")))
   (:file "middleware")
   (:file "routes")
   (:file "conduit")))
