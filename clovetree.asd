(in-package #:asdf-user)

(defsystem "clovetree"
  :depends-on ("alexandria" "local-time" "mcclim")
  :pathname "Source"
  :serial t
  :components ((:file "packages")
               (:file "utilities")
               (:file "protocols")
               (:file "classes")
               (:file "presentations")
               (:file "interface")))

(defsystem "clovetree/test"
  :depends-on ("clovetree")
  :pathname "Tests"
  :components ((:file "example-data")))
