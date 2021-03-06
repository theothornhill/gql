(in-package #:gql)

(declaim (optimize (debug 3)))

(defun fragment-type-applies-p (object-type fragment-type)
  ;; TODO: https://spec.graphql.org/draft/#DoesFragmentTypeApply()
  (let ((type-definition (gethash object-type (type-map (schema *context*)))))
    (typecase type-definition
      (object-type-definition
       (string= (nameof type-definition)
                (nameof fragment-type)))
      (interface-type-definition
       ;; TODO: Not good enough.  We need to see if we are an implementation.
       (string= (nameof type-definition)
                (nameof fragment-type)))
      (union-type-definition
       (with-accessors ((union-members union-members)) type-definition
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
      :with fragments = (get-fragments)
      :with grouped-fields = (make-hash-table :test #'equal)
      :for selection :in selection-set
      :do (unless (skippable-field-p (directives selection))
            (with-accessors ((kind kind) (name name)) selection
              (ecase kind
                (field (sethash selection (nameof selection) grouped-fields))
                (fragment-spread
                 (with-accessors ((fragment-name fragment-name)) selection
                   (with-accessors ((name name)) fragment-name
                     (unless (member name visited-fragments :test #'equal)
                       (push name visited-fragments)
                       (let ((fragment (gethash name fragments)))
                         (when fragment
                           (with-accessors ((type-condition type-condition)) fragment
                             (when (fragment-type-applies-p object-type type-condition)
                               (with-accessors ((selection-set selection-set)) fragment
                                 (maphash (lambda (key value) (sethash value key grouped-fields))
                                          (collect-fields object-type (selections selection-set) variable-values visited-fragments)))))))))))
                (inline-fragment
                 (with-accessors ((type-condition type-condition)) selection
                   (unless (and (not (null type-condition))
                                (not (fragment-type-applies-p object-type type-condition)))
                     (with-accessors ((selection-set selection-set)) selection
                       (maphash (lambda (key value) (sethash value key grouped-fields))
                                (collect-fields object-type (selections selection-set) variable-values visited-fragments)))))))))
      :finally (return grouped-fields))))

(defun get-operation (document &optional operation-name)
  ;; TODO: https://spec.graphql.org/draft/#GetOperation()
  (if (null operation-name)
      (let ((operation
              (remove-if-not (lambda (x) (typep x 'operation-definition))
                             (definitions document))))
        (if (= 1 (length operation))
            (car operation)
            (gql-error "Need to raise a request error: https://spec.graphql.org/draft/#GetOperation()")))
      (let ((operation
              (gethash operation-name (get-types 'operation-definition document))))
        (if operation operation
            (gql-error "Need to raise a request error: https://spec.graphql.org/draft/#GetOperation()")))))

(defun input-type-p (type)
  ;; TODO: https://spec.graphql.org/draft/#IsInputType()
  (if (typep (kind type) 'wrapper-type)
      (input-type-p (gql-type type))
      (let ((possible-type (gethash (nameof type) (type-map (schema *context*)))))
        (if possible-type
            (typep (kind possible-type) 'input-types)
            (typep (nameof type) 'built-in-scalar)))))

(defun output-type-p (type)
  ;; TODO: https://spec.graphql.org/draft/#IsOutputType()
  (if (typep (kind type) 'wrapper-type)
      (output-type-p (gql-type type))
      (let ((possible-type (gethash (nameof type) (type-map (schema *context*)))))
        (if possible-type
            (typep (kind possible-type) 'output-types)
            (typep (nameof type) 'built-in-scalar)))))

(declaim (ftype (function (operation-definition hash-table t) hash-table) execute-query))
(defun execute-query (query variable-values initial-value)
  ;; TODO: https://spec.graphql.org/draft/#sec-Query
  (let ((query-type (query-type (schema *context*))))
    (check-type query-type object-type-definition)
    (with-accessors ((selection-set selection-set)) query
      (setf (gethash "data" *result*)
            (execute-selection-set (selections selection-set) query-type initial-value variable-values))
      (when *errors*
        ;; TODO: This might be too strict.  It may be okay to leave some data here.
        (setf (gethash "data" *result*) nil)
        (setf (gethash "errors" *result*) *errors*))
      *result*)))

(declaim (ftype (function (operation-definition hash-table t) hash-table) execute-mutation))
(defun execute-mutation (mutation variable-values initial-value)
  ;; TODO: https://spec.graphql.org/draft/#ExecuteMutation()
  (let ((mutation-type (mutation-type (schema *context*))))
    (check-type mutation-type object-type-definition)
    (with-accessors ((selection-set selection-set)) mutation
      (setf (gethash "data" *result*)
            (execute-selection-set (selections selection-set) mutation-type initial-value variable-values))
      (when *errors*
        ;; TODO: This might be too strict.  It may be okay to leave some data here.
        (setf (gethash "data" *result*) nil)
        (setf (gethash "errors" *result*) *errors*))
      *result*)))

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
       (let* ((field-definition (get-field-definition (car fields) object-type)))
         (unless (stringp field-definition)
           (setf (gethash response-key results)
                 (execute-field object-type
                                object-value
                                field-definition
                                fields
                                variable-values)))))
     (collect-fields object-type selection-set variable-values))
    results))

(defun coerce-argument-values (object-type field variable-values)
  ;; TODO: https://spec.graphql.org/draft/#sec-Coercing-Field-Arguments
  (loop
    :with coerced-values = (make-hash-table :test #'equal)
    :for argument-values = (arguments field)
    :for argument-definition :in (args (get-field-definition field object-type))
    :do (let* ((argument-name (nameof argument-definition))
               (argument-type (gql-type argument-definition))
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
             ;; TODO: Check if we can properly identify 'null here
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


(defun resolve-field-value ()
  ;; TODO: https://spec.graphql.org/draft/#ResolveFieldValue()
  (declare (optimize (debug 3)))
  (with-accessors ((resolver resolver)) (field-definition (execution-context *context*))
    (when resolver (funcall resolver))))

(defun complete-value (field-type fields result variable-values)
  ;; TODO: https://spec.graphql.org/draft/#CompleteValue()
  (declare (optimize (debug 3)))
  (when result
    (typecase field-type
      (non-null-type
       (let ((completed-result
               (complete-value (gql-type field-type) fields result variable-values)))
         (if completed-result completed-result
             (gql-error "Need to raise a field error here"))))
      (list-type
       (if (null (listp result)) ;; TODO: What type is result really?
           (gql-error "Need to raise a field error here")
           (mapcar
            (lambda (result-item)
              (complete-value (gql-type field-type) fields result-item variable-values))
            result)))
      ;; TODO: We don't handle nil/null/'null yet
      (named-type
       (let ((type-definition (gethash (nameof field-type) (type-map (schema *context*))))) ;; TODO: #32
         ;; TODO: Maybe check for presentness rather than nil?
         (if (typep (nameof field-type) 'built-in-scalar)
             (coerce-result field-type result)
             (etypecase type-definition
               ((or scalar-type-definition enum-type-definition)
                (coerce-result field-type result))
               ((or object-type-definition interface-type-definition union-type-definition)
                (execute-selection-set (merge-selection-sets fields)
                                       (if (typep type-definition 'object-type-definition)
                                           type-definition
                                           (resolve-abstract-type type-definition result))
                                       result
                                       variable-values)))))))))

(defun coerce-result (leaf-type value)
  ;; TODO: https://spec.graphql.org/draft/#CoerceResult()
  ;; TODO: #28
  (let ((leaf-type-name (if (typep (kind leaf-type) 'wrapper-type)
                            (nameof (gql-type leaf-type))
                            (nameof leaf-type))))
    (etypecase value
      ;; TODO: This should report a field error if out of coerce range.
      (integer
       (if (string= leaf-type-name "Int")
           (coerce value '(signed-byte 32))
           (push-error (format nil "Cannot coerce result into Int for value: ~a when value should be: ~a"
                                 value leaf-type-name)
                       leaf-type)))
      ;; TODO: This should report a field error if non-finite internal values (NaN
      ;; and Infinity.
      ((or single-float double-float)
       (if (string= leaf-type-name "Float")
           (coerce value 'double-float)
           (push-error (format nil "Cannot coerce result into Float for value: ~a when value should be: ~a"
                               value leaf-type-name)
                       leaf-type)))
      (string-value
       (value value))
      (enum-value
       (if (or (string= (name leaf-type-name) "String") (string= leaf-type-name "String"))
           (value value)
           (push-error (format nil "Cannot coerce result into Enum value for value: ~a when value should be: ~a"
                               value leaf-type-name)
                       leaf-type)))
      (name ;; TODO: Should this be possible??
       (if (string= leaf-type-name "String")
           (name value)
           (push-error (format nil "Cannot coerce result into String for value: ~a when value should be: ~a"
                               value leaf-type-name)
                       leaf-type)))
      ;; TODO: Add other clauses for other literal values
      (bool ;; TODO: Make sure we get the correct bool/nil/t/true/false
       (if (string= leaf-type-name "Boolean")
           (if (equal value 'true) "true" "false")
           (push-error (format nil "Cannot coerce result into Boolean for value: ~a when type should be: ~a"
                               value leaf-type-name )
                       leaf-type)))
      (string
       (if (or (string= leaf-type-name "String") (string= leaf-type-name "ID"))
           value
           (push-error (format nil "Cannot coerce result into String or ID for value: ~a when type should be: ~a"
                               value leaf-type-name)
                       leaf-type)))
      (t (push-error (format nil "Cannot coerce result for value: ~a when value should be: ~a "
                               value leaf-type-name)
                     leaf-type)))))

(defun resolve-abstract-type (abstract-type object-value)
  ;; TODO: https://spec.graphql.org/draft/#ResolveAbstractType()
  (check-type object-value gql-object)
  (with-accessors ((type-name type-name)) object-value
    (etypecase abstract-type
      (interface-type-definition (gethash type-name (type-map (schema *context*))))
      (union-type-definition
       (let ((union-member (gethash type-name (union-members abstract-type))))
         (gethash (nameof union-member) (type-map (schema *context*))))))))

(defun execute-field (object-type object-value field-definition fields variable-values)
  ;; TODO: https://spec.graphql.org/draft/#sec-Executing-Fields
  (let* ((field (car fields))
         (field-name (name-or-alias field)) ;; TODO: Is nameof correct here??
         (arg-values (coerce-argument-values object-type field variable-values)))
    (setf (execution-context *context*)
          (make-instance 'execution-context
                         :object-type object-type
                         :object-value object-value
                         :field-definition field-definition
                         :field-name field-name
                         :arg-values arg-values))
    (complete-value (gql-type field-definition) fields (resolve-field-value) variable-values)))

(declaim (ftype (function (operation-definition hash-table) hash-table) coerce-vars))
(defun coerce-vars (operation variable-values)
  ;; TODO: https://spec.graphql.org/draft/#CoerceVariableValues()
  (with-accessors ((variable-definitions variable-definitions)) operation
    (loop
      :with coerced-vars = (make-hash-table :test #'equal)
      :for variable :in variable-definitions
      :for var-name = (nameof (var variable))
      :for var-type = (var-type variable)
      :when (input-type-p var-type)
        :do (with-accessors ((default-value default-value)) variable
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

(defun execute-request (operation-name initial-value)
  ;; https://spec.graphql.org/draft/#sec-Executing-Requests
  (let* ((operation (get-operation (document *context*) operation-name))
         (coerced-vars (coerce-vars operation (variables *context*))))
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

(defun execute (&optional operation-name initial-value)
  (unless *context*
    (gql-error "No context is set.  This is really bad."))
  (unless (document *context*)
    (gql-error "We need a document to execute"))
  ;; TODO: We can't really validate yet
  ;; (validate document)
  (if *errors*
      (setf (gethash "errors" *result*) *errors*)
      (execute-request operation-name initial-value))
  *result*)
