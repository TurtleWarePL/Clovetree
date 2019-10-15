(in-package #:asdf-user)

(defsystem "clovetree"
  :depends-on ("alexandria" "local-time" "mcclim")
  :pathname "Source"
  :components ((:file "packages")
               (:file "utilities")
               (:file "classes")
               (:file "protocols")
               (:file "interface")))
