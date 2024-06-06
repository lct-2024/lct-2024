(defsystem "passport-ci"
  :author "Alexander Artemenko <svetlyak.40wt@gmail.com>"
  :license "Unlicense"
  :class :package-inferred-system
  :description "Provides CI settings for passport."
  :pathname "src"
  :depends-on ("40ants-ci"
               "passport-ci/ci"))
