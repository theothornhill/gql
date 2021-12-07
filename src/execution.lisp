(in-package #:gql)

(defgeneric resolve (object-type object-value field-name arg-values)
  (:documentation "A function to resolve arbitrary values."))

(defmethod resolve (object-type object-value field-name arg-values)
  ;; TODO: Ok, so now we get the corresponding type in the hash table, then
  ;; funcall the function mapped to by field name.
  (let ((objtype (gethash (nameof object-type) *resolvers*)))
    (if (> (hash-table-count arg-values) 0)
        (funcall (gethash field-name objtype) object-value arg-values)
        (funcall (gethash field-name objtype) object-value))))

(defun fragment-type-applies-p (object-type fragment-type)
  ;; TODO: https://spec.graphql.org/draft/#DoesFragmentTypeApply()
  (let ((type-definition (gethash object-type *all-types*)))
    (typecase type-definition
      (object-type-definition
       (string= (nameof type-definition)
                (nameof fragment-type)))
      (interface-type-definition
       ;; TODO: Not good enough.  We need to see if we are an implementation.
       (string= (nameof type-definition)
                (nameof fragment-type)))
      (union-type-definition
       (with-slots (union-members) type-definition
         (let ((members (mapcar (lambda (x) (nameof x)) union-members)))
           (member (nameof fragment-type) members :test #'string=))))
      (t nil))))

(defun collect-fields (object-type
                       selection-set
                       variable-values
                       &optional
                         (visited-fragments nil))
  ;; TODO: https://spec.graphql.org/draft/#CollectFields() #10
  (labels ((sethash (item key table)
               (let ((items (if (listp item) item (list item))))
                 (setf (gethash key table) (append (gethash key table) items)))))
    (loop
      :with fragments = (get-types 'fragment-definition *schema*)
      :with grouped-fields = (make-hash-table :test #'equal)
      :for selection :in selection-set
      :do (unless (skippable-field-p (directives selection))
            (with-slots (kind name) selection
              (ecase kind
                (field (sethash selection (nameof selection) grouped-fields))
                (fragment-spread
                 (with-slots (fragment-name) selection
                   (with-slots (name) fragment-name
                     (unless (member name visited-fragments :test #'equal)
                       (push name visited-fragments)
                       (let ((fragment (gethash name fragments)))
                         (when fragment
                           (with-slots (type-condition) fragment
                             (when (fragment-type-applies-p object-type type-condition)
                               (with-slots (selection-set) fragment
                                 (maphash (lambda (key value) (sethash value key grouped-fields))
                                          (collect-fields object-type (selections selection-set) variable-values visited-fragments)))))))))))
                (inline-fragment
                 (with-slots (type-condition) selection
                   (unless (and (not (null type-condition))
                                (not (fragment-type-applies-p object-type type-condition)))
                     (with-slots (selection-set) selection
                       (maphash (lambda (key value) (sethash value key grouped-fields))
                                (collect-fields object-type (selections selection-set) variable-values visited-fragments)))))))))
      :finally (return grouped-fields))))

(defun get-operation (document &optional operation-name)
  ;; TODO: https://spec.graphql.org/draft/#GetOperation()
  (cond
    ((null operation-name)
     (let ((operation
             (remove-if-not (lambda (x) (typep x 'operation-definition))
                            (definitions document))))
       (if (= 1 (length operation))
           (car operation)
           (gql-error "Need to raise a request error: https://spec.graphql.org/draft/#GetOperation()"))))
    (t
     (let ((operation
             (gethash operation-name (get-types 'operation-definition document))))
       (if operation operation
           (gql-error "Need to raise a request error: https://spec.graphql.org/draft/#GetOperation()"))))))

(defun input-type-p (type)
  ;; TODO: https://spec.graphql.org/draft/#IsInputType()
  (if (typep (kind type) 'wrapper-type)
      (input-type-p (ty type))
      (let ((possible-type (gethash (nameof type) *all-types*)))
        (if possible-type
            (typep (kind possible-type) 'input-types)
            (typep (nameof type) 'built-in-scalar)))))

(defun output-type-p (type)
  ;; TODO: https://spec.graphql.org/draft/#IsOutputType()
  (if (typep (kind type) 'wrapper-type)
      (output-type-p (ty type))
      (let ((possible-type (gethash (nameof type) *all-types*)))
        (if possible-type
            (typep (kind possible-type) 'output-types)
            (typep (nameof type) 'built-in-scalar)))))

(declaim (ftype (function (operation-definition hash-table t) hash-table) execute-query))
(defun execute-query (query variable-values initial-value)
  ;; TODO: https://spec.graphql.org/draft/#sec-Query
  (let ((query-type (gethash "Query" *all-types*)))
    (check-type query-type object-type-definition)
    (with-slots (selection-set) query
      (let ((results (make-hash-table :test #'equal)))
        (setf (gethash "data" results)
              (execute-selection-set (selections selection-set) query-type initial-value variable-values))
        (setf (gethash "errors" results) *errors*)
        results))))

(declaim (ftype (function (operation-definition hash-table t) hash-table) execute-mutation))
(defun execute-mutation (mutation variable-values initial-value)
  ;; TODO: https://spec.graphql.org/draft/#ExecuteMutation()
  (let ((mutation-type (gethash "Mutation" *all-types*)))
    (check-type mutation-type object-type-definition)
    (with-slots (selection-set) mutation
      (let ((results (make-hash-table :test #'equal)))
        (setf (gethash "data" results)
              (execute-selection-set (selections selection-set) mutation-type initial-value variable-values))
        (setf (gethash "errors" results) *errors*)
        results))))

(defun subscribe (subscription variable-values initial-value)
  ;; TODO: https://spec.graphql.org/draft/#Subscribe()
  (let ((source-stream (create-source-event-stream subscription variable-values initial-value)))
    (map-source-to-response-event source-stream subscription variable-values)))

(defun unsubscribe (response-stream)
  ;; TODO: https://spec.graphql.org/draft/#Unsubscribe()
  (declare (ignorable response-stream))
  (gql-error "TODO: unsubscribe not implemented"))

(defun execute-selection-set (selection-set object-type object-value variable-values)
  ;; TODO: https://spec.graphql.org/draft/#sec-Executing-Selection-Sets
  (let ((results (make-hash-table :test #'equal)))
    (maphash
     (lambda (response-key fields)
       (with-slots (ty) (get-field-definition (car fields) object-type)
         (when ty
           (setf (gethash response-key results)
                 (execute-field object-type object-value ty fields variable-values)))))
     (collect-fields object-type selection-set variable-values))
    results))

(defun coerce-argument-values (object-type field variable-values)
  ;; TODO: https://spec.graphql.org/draft/#sec-Coercing-Field-Arguments
  (loop
    :with coerced-values = (make-hash-table :test #'equal)
    :for argument-values = (arguments field)
    :for argument-definition :in (args (get-field-definition field object-type))
    :do (let* ((argument-name (nameof argument-definition))
               (argument-type (ty argument-definition))
               (default-value (default-value argument-definition))
               (argument (find-if (lambda (obj) (string= (nameof obj) argument-name)) argument-values))
               (has-value-p (and argument t))
               (argument-value (and has-value-p (value argument)))
               (value))
          (if (eq (kind argument-value) 'var)
              (multiple-value-bind (val val-p)
                  (gethash (nameof argument-value) variable-values)
                (setf value val has-value-p val-p))
              (setf value argument-value))
          (cond
            (;; including 'null' as a default value
             (and (null has-value-p) default-value)
             (setf (gethash argument-name coerced-values) default-value))
            ((and (eq (kind argument-type) 'non-null-type)
                  (or (null has-value-p)
                      (null value)))
             (gql-error "Raise a field error here!"))
            (has-value-p
             (cond
               ((null value)
                ;; TODO: Is 'null ok here?
                (setf (gethash argument-name coerced-values) 'null))
               ((eq (kind argument-value) 'var)
                (setf (gethash argument-name coerced-values) value))
               (t
                (setf (gethash argument-name coerced-values)
                      (coerce-result argument-type value)))))))
    :finally (return coerced-values)))


(defun resolve-field-value (object-type object-value field-name arg-values)
  ;; TODO: https://spec.graphql.org/draft/#ResolveFieldValue()
  ;;
  ;; This function should access the hash table *resolvers* created by the
  ;; implementors of the api.  It is good form to make sure that all the fields
  ;; are covered.
  (resolve object-type object-value field-name arg-values))

(defun complete-value (field-type fields result variable-values)
  ;; TODO: https://spec.graphql.org/draft/#CompleteValue()
  (when result
    (typecase field-type
      (non-null-type
       (let ((completed-result
               (complete-value (ty field-type) fields result variable-values)))
         (if completed-result completed-result
             (gql-error "Need to raise a field error here"))))
      (list-type
       (if (null (listp result)) ;; TODO: What type is result really?
           (gql-error "Need to raise a field error here")
           (mapcar
            (lambda (result-item)
              (complete-value (ty field-type) fields result-item variable-values))
            result)))
      (named-type
       (let ((field-definition (gethash (nameof field-type) *all-types*)))
         ;; TODO: Maybe check for presentness rather than nil?
         (if (typep (nameof field-type) 'built-in-scalar)
             (coerce-result field-type result)
             (etypecase field-definition
               ((or scalar-type-definition
                    enum-type-definition)
                (coerce-result field-type result))
               ((or object-type-definition
                    interface-type-definition
                    union-type-definition)
                (execute-selection-set
                 (merge-selection-sets fields)
                 (if (typep field-definition 'object-type-definition)
                     field-definition
                     (resolve-abstract-type field-definition result))
                 result
                 variable-values)))))))))

(defun coerce-result (leaf-type value)
  ;; TODO: https://spec.graphql.org/draft/#CoerceResult()
  ;; TODO: #28
  (let ((leaf-type-name (if (typep (kind leaf-type) 'wrapper-type)
                            (nameof (ty leaf-type))
                            (nameof leaf-type))))
    (etypecase value
      ;; TODO: This should report a field error if out of coerce range.
      (integer
       (or (and (string= leaf-type-name "Int") (coerce value '(signed-byte 32)))
           "Field error for int"))
      ;; TODO: This should report a field error if non-finite internal values (NaN
      ;; and Infinity.
      ((or single-float double-float)
       (or (and (string= leaf-type-name "Float") (coerce value 'double-float))
           "Field error for float"))
      ;; TODO: We may return "true" for t and "1" for integer 1.
      (string
       (or (and (or (string= leaf-type-name "String")
                    (string= leaf-type-name "ID"))
                value)
           "Field error for string"))
      (bool
       (or (and (string= leaf-type-name "Boolean")
                (if (equal value 'true) "true" "false"))
           "Field error for boolean"))
      (t "We really screwed up result coercing here!"))))

(defun resolve-abstract-type (abstract-type object-value)
  ;; TODO: https://spec.graphql.org/draft/#ResolveAbstractType()
  ;; TODO: #29
  (check-type object-value gql-object)
  (etypecase abstract-type
    (interface-type-definition
     ;; TODO: Should this error handle somehow?
     (gethash (type-name object-value) *all-types*))
    (union-type-definition nil)))

(defun execute-field (object-type object-value field-type fields variable-values)
  ;; TODO: https://spec.graphql.org/draft/#sec-Executing-Fields
  (let* ((field (car fields))
         (field-name (name-or-alias field)) ;; TODO: Is nameof correct here??
         (arg-values (coerce-argument-values object-type field variable-values))
         (resolved-value
           (resolve-field-value object-type object-value field-name arg-values)))
    (complete-value field-type fields resolved-value variable-values)))

(declaim (ftype (function (operation-definition hash-table) hash-table) coerce-vars))
(defun coerce-vars (operation variable-values)
  ;; TODO: https://spec.graphql.org/draft/#CoerceVariableValues()
  (with-slots (variable-definitions) operation
    (loop
      :with coerced-vars = (make-hash-table :test #'equal)
      :for variable :in variable-definitions
      :for var-name = (nameof (var variable))
      :for var-type = (var-type variable)
      :when (input-type-p var-type)
        :do (with-slots (default-value) variable
              (multiple-value-bind (val val-p) (gethash var-name variable-values)
                (cond
                  ((and (null val-p) default-value)
                   (setf (gethash var-name coerced-vars) default-value))
                  ((and (typep var-type 'non-null-type)
                        (or (null val-p) (null val)))
                   (gql-error "Need to raise a request error for coerce-vars"))
                  (val-p
                   (if (null val)
                       ;; TODO: Is 'null ok here?
                       (setf (gethash var-name coerced-vars) 'null)
                       ;; TODO: Handle the errors that can percolate up
                       (setf (gethash var-name coerced-vars)
                             (coerce-result var-type val)))))))
      :finally (return coerced-vars))))

(defun execute-request (document operation-name variable-values initial-value)
  ;; https://spec.graphql.org/draft/#sec-Executing-Requests
  (let* ((*errors* nil)
         (operation (get-operation document operation-name))
         (coerced-vars (coerce-vars operation variable-values)))
    (string-case (operation-type operation)
      ("Query"        (execute-query operation coerced-vars initial-value))
      ("Mutation"     (execute-mutation operation coerced-vars initial-value))
      ("Subscription" (subscribe operation coerced-vars initial-value)))))

(defun execute-subscription-event (subscription variable-values initial-value)
  ;; TODO: https://spec.graphql.org/draft/#ExecuteSubscriptionEvent()
  (declare (ignorable subscription variable-values initial-value))
  t)

(defun create-source-event-stream (subscription variable-values initial-value)
  ;; TODO: https://spec.graphql.org/draft/#CreateSourceEventStream()
  (declare (ignorable subscription variable-values initial-value))
  t)

(defun resolve-field-event-stream (subscription-type root-value field-name argument-values)
  ;; TODO: https://spec.graphql.org/draft/#ResolveFieldEventStream()
  (declare (ignorable subscription-type root-value field-name argument-values))
  t)

(defun map-source-to-response-event (source-stream subscription variable-values)
  ;; TODO: https://spec.graphql.org/draft/#MapSourceToResponseEvent()
  (declare (ignorable source-stream subscription variable-values))
  t)

(defun merge-selection-sets (fields)
  ;; TODO: https://spec.graphql.org/draft/#MergeSelectionSets()
  (loop
    :with selection-set = nil
    :for field :in fields
    :for field-selection-set = (selection-set field)
    :when field-selection-set
      :do (loop
            :for selection :in (selections field-selection-set)
            :do (push selection selection-set))
    :finally (return (nreverse selection-set))))
