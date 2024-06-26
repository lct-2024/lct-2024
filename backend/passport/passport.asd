#-asdf3.1 (error "passport requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "passport"
  :description "Passport to store data about users and companies."
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("passport/core"
               "passport/api/users"
               "passport/api/scopes"
               "passport/api/deploy")
  :in-order-to ((test-op (test-op "passport-tests"))))


(asdf:register-system-packages "bordeaux-threads" '(#:bordeaux-threads-2))
