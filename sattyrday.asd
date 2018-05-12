(asdf:defsystem :sattyrday
  :description "Small TTY-based projects, written on Saturdays."

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (

               :boots
               :deploy
               :iterate
               :losh

               )

  :serial t
  :components
  ((:module "vendor" :serial t
    :components ((:file "quickutils-package")
                 (:file "quickutils")))
   (:file "package")))


(asdf:defsystem :sattyrday/001-pong
  :depends-on (:sattyrday)

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "pong"
  :entry-point "pong:toplevel"

  :components ((:module "src/001-pong" :serial t :components ((:file "main")))))
