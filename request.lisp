(in-package :gql)

(defgeneric request (query)
  (:documentation "Make a request using whichever framework you want."))

(defclass* query
  query
  variables)


;; Example code

;; (defun make-query (query &optional variables)
;;   (com.inuoe.jzon:stringify
;;    (make-instance 'query :query query)))

;; (defmethod request (query)
;;   (format t "~%~a~%"
;;           (flexi-streams:octets-to-string
;;            (drakma:http-request
;;             "https://git.sr.ht/query"
;;             :method :post
;;             :content (make-query (gql query))
;;             :external-format-out 'utf-8
;;             :additional-headers `(("Content-Type" . "application/json")    
;;                                   ("Authorization" . ,(concatenate 'string "Bearer "
;;                                                                    (uiop:getenv "srhttoken"))))))))
