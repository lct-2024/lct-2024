(uiop:define-package #:passport/client
  (:use #:cl)
  (:import-from #:40ants-openrpc/client
                #:generate-client)
  (:import-from #:common/rpc
                #:cached-url-as))
(in-package #:passport/client)



(generate-client passport
                 "http://localhost:10111/")
