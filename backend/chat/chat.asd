(defsystem "chat"
  :class :package-inferred-system
  :depends-on ("common"
               "chat/server"))


(asdf:register-system-packages "log4cl" '(#:log))
