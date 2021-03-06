(in-package :gql)

(defun every-definition-executable-p (definitions)
  ;; https://spec.graphql.org/draft/#sec-Executable-Definitions
  (loop
    :for definition :in definitions
    :unless (or (eq (kind definition) 'operation-definition)
                (eq (kind definition) 'fragment-definition))
      :do (push-error "Each definition must be executable." definition)))

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
           :do (push-error "Each operation must have a unique name." v))))

(defun single-anonymous-operation-definition-p (definitions)
  ;; https://spec.graphql.org/draft/#sec-Anonymous-Operation-Definitions
  (loop
    :with anonymous = nil
    :for definition :in definitions
    :for name-node = (name definition)
    :when (null name-node)
      :do (push definition anonymous)
          (when (and (> (length definitions) 1) anonymous)
            (push-error "An anonymous definition must be alone." definition)
            (return))))

(defun subscription-operation-valid-p ()
  ;; https://spec.graphql.org/draft/#sec-Subscription-Operation-Definitions
  (loop
    :for subscription :in (subscription-type *schema*)
    :for subscription-type = (operation-type subscription)
    :for selection-set = (selection-set subscription)
    :for grouped-field-set = (collect-fields subscription-type
                                             (selections selection-set)
                                             (make-hash-table)
                                             nil)
    :unless (= (hash-table-count grouped-field-set) 1)
      :do (push-error "A subscription must have exactly one entry." subscription)
    :when (introspection-field-p grouped-field-set)
      :do (push-error "Root field must not begin with \"__\"  which is reserved by GraphQL introspection." subscription)))
