(uiop:define-package #:ats/algorithms/new-job-processing
  (:use #:cl)
  (:import-from #:ats/models/subscription
                #:subscription-filters
                #:subscription-applicant-id
                #:subscription)
  (:import-from #:mito
                #:find-dao
                #:select-dao
                #:retrieve-dao)
  (:import-from #:ats/models/applicant
                #:applicant)
  (:import-from #:ats/api/applicants
                #:notify-applicant)
  (:import-from #:serapeum
                #:eval-always
                #:fmt)
  (:import-from #:ats/models/job
                #:*new-job-hooks*
                #:job-title)
  (:import-from #:40ants-pg/connection
                #:with-connection)
  (:import-from #:bordeaux-threads-2
                #:make-thread)
  (:export
   #:process-new-job-in-thread))
(in-package #:ats/algorithms/new-job-processing)


(defun match (job subscription)
  (let ((filters (subscription-filters subscription))
        (fields (list (cons "category"
                            #'ats/models/job::job-category)
                      (cons "city"
                            #'ats/models/job::job-city)
                      (cons "project"
                            (lambda (job)
                              (ats/models/project::project-title
                               (find-dao 'ats/models/project::project
                                         :id (ats/models/job::job-project-id job)))))
                      (cons "speciality"
                            (lambda (job)
                              (ats/models/speciality::speciality-title
                               (find-dao 'ats/models/speciality::speciality
                                         :id (ats/models/job::job-speciality-id job))))))))
    (loop for (field-name . getter) in fields
          for expected-value = (gethash field-name filters)
          for real-value = (funcall getter job)
          thereis (and expected-value
                       (equal expected-value real-value)))))


(defun process-new-job (job)

  (let ((message (fmt "Появилась новая вакансия: ~S"
                      (job-title job))))
    (with-connection ()
      (loop for subscription in (select-dao 'subscription)
            when (progn (log:info "Checking if new job matching to subscription" job subscription)
                        (match job subscription))
            do (log:info "Notifying applicant")
               (notify-applicant (subscription-applicant-id subscription)
                                 message)))))


(defun process-new-job-in-thread (job)
  (make-thread (lambda ()
                 (process-new-job job))
               :name "New Job Processing")
  (values))


(eval-always
  (pushnew 'process-new-job-in-thread
           *new-job-hooks*))
