(uiop:define-package #:ats/models/programming-language
  (:use #:cl)
  (:import-from #:dex)
  (:import-from #:serapeum
                #:fmt
                #:soft-list-of
                #:dict)
  (:import-from #:mito
                #:create-dao
                #:insert-dao
                #:deftable
                #:object-id
                #:dao-table-class)
  (:import-from #:40ants-pg/query
                #:sql-fetch-all)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:str
                #:split
                #:trim))
(in-package #:ats/models/programming-language)


(deftable programming-language ()
  ((title :initarg :title
          :type string
          :col-type :text
          :accessor programming-language-title))
  (:table-name "ats.programming_language"))


(defmethod print-object ((programming-language programming-language) stream)
  (print-unreadable-object (programming-language stream :type t)
    (format stream "ID=~A TITLE=~A"
            (object-id programming-language)
            (programming-language-title programming-language))))


(defun suggest-programming-languages (query)
  (with-connection ()
    (mito:select-by-sql 'programming-language
                        "
select *
from ats.programming_language
where title COLLATE \"ru_RU\" ilike ?
order by title COLLATE \"ru_RU\""
                        :binds (list (fmt "%~A%" query)))))


(defun load-some-programming-languages ()
  (with-connection ()
    (loop with data = (list "Python"
                            "Java"
                            "JavaScript"
                            "C#"
                            "C++"
                            "Ruby"
                            "PHP"
                            "Swift"
                            "Kotlin"
                            "TypeScript"
                            "Go"
                            "R"
                            "Scala"
                            "Rust"
                            "Perl"
                            "Haskell"
                            "Objective-C"
                            "Lua"
                            "Shell Script"
                            "SQL"
                            "Matlab"
                            "Assembly"
                            "Groovy"
                            "Clojure"
                            "Visual Basic"
                            "Dart"
                            "F#"
                            "COBOL"
                            "Lisp"
                            "Prolog"
                            "Fortran"
                            "Ada"
                            "Julia"
                            "Scheme"
                            "TCL"
                            "Erlang"
                            "Powershell"
                            "Apex"
                            "Delphi"
                            "Elixir"
                            "ActionScript"
                            "ABAP"
                            "Smalltalk"
                            "VHDL"
                            "ColdFusion"
                            "AWK"
                            "COOL"
                            "Dart"
                            "OCaml"
                            "Scratch")
          for title in (remove-duplicates
                        (sort data
                              #'string<)
                        :test #'string-equal)
          do (create-dao 'programming-language
                         :title (trim title)))))


(defun get-programming-language-ids (languages)
  (loop for name in languages
        for lang = (or (first
                        (mito:select-by-sql 'programming-language
                                            "select * from ats.programming_language where title collate \"ru_RU\" ilike ?"
                                            :binds (list (trim name))))
                       (mito:create-dao 'programming-language
                                        :title (trim name)))
        collect (mito:object-id lang)))
