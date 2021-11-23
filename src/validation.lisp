(in-package :gql)


(defgeneric validate (node &key &allow-other-keys)
  (:documentation "TODO"))


(defclass* errors
  nodes
  message)

(defun make-error (message nodes)
  (let ((error-nodes (if (listp nodes) nodes (list nodes))))
    (push (make-instance 'errors
                         :message message
                         :nodes error-nodes)
          *errors*)))

(defmacro defvalidator (node keys &body body)
  "Convenience macro to define new validator methods.
Specializes on the NODE-TYPE, so if more granular control is needed, either
expand this macro or just use a normal DEFMETHOD."
  `(defmethod validate ((node ,node) &key ,@keys &allow-other-keys)
     ,@body))

(defvalidator document ()
  (let ((definitions (definitions node)))
    ;; https://spec.graphql.org/draft/#sec-Executable-Definitions
    (unless (every
             (lambda (definition)
               (or (eq (kind definition) 'operation-definition)
                   (eq (kind definition) 'fragment-definition)))
             definitions)
      (make-error "Each definition must be executable." node))

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
             :do (make-error "Each operation must have a unique name." v)))

    ;; https://spec.graphql.org/draft/#sec-Anonymous-Operation-Definitions
    (loop
      :for definition :in definitions
      :for name-node = (name definition)
      :when (and (null name-node)
                 (> (length definitions) 1))
        :do (make-error "An anonymous definition must be alone." definition))

    ;; https://spec.graphql.org/draft/#sec-Subscription-Operation-Definitions
    (loop
      :for subscription :in (get-subscriptions node)
      :for subscription-type = (operation-type subscription)
      :for selection-set = (selection-set subscription)
      :for fragments = (fragment-definitions node)
      :for grouped-field-set = (collect-fields fragments
                                               subscription-type
                                               selection-set
                                               nil
                                               nil)
      :unless (= (hash-table-count grouped-field-set) 1)
        :do (make-error "A subscription must have exactly one entry." subscription)
      :when (introspection-field-p grouped-field-set)
        :do (make-error "Root field must not begin with \"__\"  which is reserved by GraphQL introspection." subscription))
    
    (values *data* *errors*)))
