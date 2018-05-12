(ql:quickload :sattyrday)

(setf deploy:*status-output* nil)

(let ((deploy:*status-output* t)
      (system-to-build (read))) ; whatever
  (ql:quickload system-to-build)
  (asdf:make system-to-build :force t))
