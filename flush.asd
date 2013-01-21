(asdf:defsystem "flush"
  :description "A stack-based esoteric programming language"
  :depends-on (#:cl-ppcre)
  :components ((:file "flush")
               (:file "builtins" :depends-on ("flush"))))
