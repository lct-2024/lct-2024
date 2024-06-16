#-asdf3.1 (error "ats requires ASDF 3.1 because for lower versions pathname does not work for package-inferred systems.")
(defsystem "ats"
  :description "Applicants Tracking System"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :class :40ants-asdf-system
  :defsystem-depends-on ("40ants-asdf-system")
  :pathname "src"
  :depends-on ("common"
               "ats/core"
               "ats/api/jobs"
               "ats/api/applicants"
               "ats/api/projects"
               "ats/api/cities"
               "ats/api/specialities"
               "ats/api/programming-languages"
               "ats/api/news"
               "ats/api/skills"
               "ats/api/score"
               "ats/api/analytics"
               "ats/api/subscriptions"
               "ats/algorithms/resume-score"
               "ats/algorithms/new-job-processing")
  :in-order-to ((test-op (test-op "ats-tests"))))


(asdf:register-system-packages "dexador" '("DEX"))
(asdf:register-system-packages "log4cl" '(#:log))
