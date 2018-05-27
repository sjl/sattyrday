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

(asdf:defsystem :sattyrday/002-afk
  :depends-on (:sattyrday :chancery :bobbin)

  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "afk"
  :entry-point "afk:toplevel"

  :components ((:module "src/002-afk" :serial t :components ((:file "main")))))
