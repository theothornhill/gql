(in-package :gql)

(defun every-definition-executable-p (definitions)
  ;; https://spec.graphql.org/draft/#sec-Executable-Definitions
  (loop
    :for definition :in definitions
    :unless (or (eq (kind definition) 'operation-definition)
                (eq (kind definition) 'fragment-definition))
      :do (make-error "Each definition must be executable." definition)))

(defun operation-name-unique-p (definitions)
  ;; https://spec.graphql.org/draft/#sec-Operation-Name-Uniqueness
  (loop
    :with operations = (make-hash-table :test #'equalp)
    :for definition :in definitions
    :for name-node = (name definition)
    :for name = (and name-node (name name-node))
    :do (setf (gethash name operations)
              (append (gethash name operations) (list definition)))
    :finally
       (loop
         :for v :being :each :hash-value :of operations
         :when (> (length v) 1)
           ;; What format to push here? Maybe we should push some more metadata
           ;; as to what happened?
           :do (make-error "Each operation must have a unique name." v))))

(defun anonymous-operation-definition-p (definitions)
  ;; https://spec.graphql.org/draft/#sec-Anonymous-Operation-Definitions
  (loop
    :for definition :in definitions
    :for name-node = (name definition)
    :when (and (null name-node)
               (> (length definitions) 1))
      :do (make-error "An anonymous definition must be alone." definition)))

(defun subscription-operation-valid-p ()
  ;; https://spec.graphql.org/draft/#sec-Subscription-Operation-Definitions
  (loop
    :for subscription :in (get-subscriptions)
    :for subscription-type = (operation-type subscription)
    :for selection-set = (selection-set subscription)
    :for grouped-field-set = (collect-fields subscription-type
                                             (selections selection-set)
                                             nil
                                             nil)
    :unless (= (hash-table-count grouped-field-set) 1)
      :do (make-error "A subscription must have exactly one entry." subscription)
    :when (introspection-field-p grouped-field-set)
      :do (make-error "Root field must not begin with \"__\"  which is reserved by GraphQL introspection." subscription)))
