(defsystem "se-orthodontiste"
  :description "Système Expert Orthodontique"
  :version "1.0.0"
  :author "LoudiernTharon"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "engine")
                 (:file "rules" :depends-on ("engine"))
                 (:file "main" :depends-on ("engine" "rules")))))
  :build-operation "program-op"
  :build-pathname "se-orthodontiste"
  :entry-point "se-orthodontiste.main:main")
