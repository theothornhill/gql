(in-package #:gql)


(defun sethash (item key table)
  ;; TODO: Do we need to check for present-ness if nil is just appendable?
  (let ((items (if (listp item) item (list item))))
    (setf (gethash key table) (append (gethash key table) items))))

(defun fragment-type-applies-p (object-type fragment-type)
  ;; TODO: We need to check all kinds of type designators.  Non-null-types,
  ;; named-types and list-type, at least.
  (let ((type-definition (gethash object-type *all-types*)))
    (cond
      (;; TODO: This early exit is probably not good enough.  Why is it ok to do
       ;; this when we _don't_ get a hit?
       (null type-definition) t)
      ((eq (kind type-definition) 'object-type-definition)
       (string= (name (name type-definition))
                (name (name fragment-type))))
      ((eq (kind type-definition) 'interface-type-definition)
       (string= (name (name type-definition))
                (name (name fragment-type))))
      ((eq (kind type-definition) 'union-type-definition)
       (with-slots (union-members) type-definition
         (let ((members (mapcar (lambda (x) (name (name x))) union-members)))
           (member (name (name fragment-type)) members :test #'string=))))
      (t nil))))

(declaim (ftype (function (string selection-set list &optional list) hash-table) collect-fields))
(defun collect-fields (object-type
                       selection-set
                       variable-values
                       &optional
                         (visited-fragments nil))
  "Collect all fields from a `selection-set'.
  OBJECT-TYPE is the type of the current object. SELECTION-SET is a
`selection-set'.  VARIABLE-VALUES are all the variables supplied with the
`document'.  VISITED-FRAGMENTS is a list of the currently visited fragments.  It
is an accumulator of the current state."
  ;; https://spec.graphql.org/draft/#CollectFields()
  (declare (type list visited-fragments))
  (loop
    :with fragments = (get-types 'fragment-definition)
    :with grouped-fields = (make-hash-table :test #'equal)
    :for selection :in (selections selection-set)
    :do (unless (skippable-field-p (directives selection))
          (with-slots (kind name) selection
            (ecase kind
              (field (sethash selection (name-or-alias selection) grouped-fields))
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
                                        (collect-fields object-type selection-set variable-values visited-fragments)))))))))))
              (inline-fragment
               (with-slots (type-condition) selection
                 (unless (and (not (null type-condition))
                              (not (fragment-type-applies-p object-type type-condition)))
                   (with-slots (selection-set) selection
                     (maphash (lambda (key value) (sethash value key grouped-fields))
                              (collect-fields object-type selection-set variable-values visited-fragments)))))))))
    :finally (return grouped-fields)))

(defun get-operation (document &optional operation-name)
  ;; TODO: Still with the *schema*.
  (cond
    ((null operation-name)
     (let ((operation
             (remove-if-not (lambda (x) (equal (kind x) 'operation-definition))
                            (definitions document))))
       (if (= 1 (length operation))
           (car operation)
           (gql-error "Need to raise a request error: https://spec.graphql.org/draft/#GetOperation()"))))
    (t
     (let ((operation
             (gethash operation-name
                      (get-types 'operation-definition))))
       (if operation operation
           (gql-error "Need to raise a request error: https://spec.graphql.org/draft/#GetOperation()"))))))

(defun input-type-p (type)
  ;; TODO: https://spec.graphql.org/draft/#IsInputType()
  (with-slots (kind) type
    (if (typep kind 'wrapper-type)
        (input-type-p (ty type))
        (let ((possible-type (gethash (nameof type) *all-types*)))
          ;; Get the type corresponding to the name in question
          (if possible-type
              (typep (kind possible-type) 'input-types)
              (typep (nameof type) 'built-in-scalar))))))

(defun output-type-p (type)
  ;; TODO: https://spec.graphql.org/draft/#IsOutputType()
  (with-slots (kind) type
    (if (typep kind 'wrapper-type)
        (output-type-p (ty type))
        (let ((possible-type (gethash (nameof type) *all-types*)))
          ;; Get the type corresponding to the name in question
          (if possible-type
              (typep (kind possible-type) 'output-types)
              (typep (nameof type) 'built-in-scalar))))))

(declaim (ftype (function (operation-definition document hash-table t) hash-table) execute-query))
(defun execute-query (query variable-values initial-value)
  ;; TODO: https://spec.graphql.org/draft/#sec-Query
  (let ((query-type (gethash "Query" *all-types*)))
    (check-type query-type object-type-definition)
    (with-slots (selection-set) query
      (let ((results (make-hash-table :test #'equal)))
        (setf (gethash "data" results)
              (execute-selection-set selection-set query-type initial-value variable-values))))))

(defun execute-mutation (operation schema coerced-vars initial-value)
  (declare (ignorable operation schema coerced-vars initial-value))
  (gql-error "TODO: execute-mutation not implemented"))

(defun subscribe (operation schema coerced-vars initial-value)
  (declare (ignorable operation schema coerced-vars initial-value))
  (gql-error "TODO: subscribe not implemented"))

(defun unsubscribe (response-stream)
  ;; TODO: https://spec.graphql.org/draft/#Unsubscribe()
  (declare (ignorable response-stream))
  (gql-error "TODO: unsubscribe not implemented"))

(defun execute-selection-set (selection-set object-type object-value variable-values)
  ;; TODO: https://spec.graphql.org/draft/#sec-Executing-Selection-Sets
  (let ((results (make-hash-table :test #'equal)))
    (maphash
     (lambda (response-key fields)
       (let* (;; TODO: This should be unaffected if an alias is used.  What does
              ;; that mean?
              (field-name (nameof (car fields)))
              (field-definition
                (car (remove-if-not (lambda (obj) (string= (nameof obj) field-name))
                                    (gethash (nameof object-type) *all-types*)))))
         (with-slots (ty) field-definition
           (when ty
             (sethash (execute-field object-type object-value ty fields variable-values)
                      response-key
                      results)))))
     (collect-fields object-type selection-set variable-values))
    results))

(declaim (ftype (function (object-type-definition field hash-table) hash-table) coerce-args))
(defun coerce-args (object-type field variable-values)
  ;; TODO: https://spec.graphql.org/draft/#sec-Coercing-Field-Arguments
  (loop
    :with coerced-values = (make-hash-table :test #'equal)
    :for argument-values = (arguments field)
    :for field-name = (name-or-alias field)
    :for argument-definitions = (car (fields object-type))
    :for argument-definition :in argument-definitions
    :do (let ((argument-name (name-or-alias argument-definition))
              (argument-type (ty argument-definition))
              (;; TODO: Where do I get this value??
               default-value t)
              (;; TODO: if argumentValues provides a value for the name argumentName
               has-value-p nil)
              (;; TODO: the value provided in argumentValues for the name argumentName
               argument-value nil)
              (value))
          (if (eq (kind argument-value) 'var)
              (multiple-value-bind (val val-p)
                  (gethash (name-or-alias argument-value) variable-values)
                (setf value val has-value-p val-p))
              (setf value argument-value))
          (cond
            (;; including 'null' as a default value
             (and (null has-value-p) default-value)
             (sethash default-value argument-name coerced-values))
            ((and (eq (kind argument-type) 'non-null-type)
                  (or (null has-value-p)
                      (null value)))
             (gql-error "Raise a field error here!"))
            (has-value-p
             (cond
               ((null value)
                ;; TODO: Is nil ok here?
                (sethash nil argument-name coerced-values))
               ((eq (kind argument-value) 'var)
                (sethash value argument-name coerced-values))
               (t
                (let (;; TODO: Coerce the val first for the else part,
                      ;; find out how
                      (coerced-value t))
                  (sethash coerced-value argument-name coerced-values)))))))
    :finally (return coerced-values)))

(defun resolve-field-value (object-type object-value field-name arg-values)
  ;; TODO: https://spec.graphql.org/draft/#ResolveFieldValue()
  (declare (ignorable object-type object-value field-name arg-values))
  t)

(defun complete-value (field-type fields result variable-values)
  ;; TODO: https://spec.graphql.org/draft/#CompleteValue()
  (when result
    ;; TODO: We cannot check for KIND here because we need the resolved type
    ;; from *all-types*.  Don't we..?  I mean, we cannot possibly check for
    ;; "Alien" here without knowing what it is.  And what about leaves like
    ;; ENUM?  Can we handle that easily?
    (ecase (kind field-type)
      (non-null-type
       (let ((completed-result
               (complete-value (ty field-type)
                               fields
                               result
                               variable-values)))
         (if completed-result completed-result
             (gql-error "Need to raise a field error here"))))
      (list-type
       (if (null (listp result)) ;; TODO: What type is this really?
           (gql-error "Need to raise a field error here")
           (mapcar
            (lambda (result-item)
              (complete-value (ty field-type) fields result-item variable-values))
            result)))
      (;; TODO: Are these the correct types?
       (scalar-type-definition
        enum-type-definition)
       (coerce-result field-type result))
      (;; TODO: Are these the correct types?
       (object-type-definition
        interface-type-definition
        union-type-definition)
       (execute-selection-set
        (merge-selection-sets fields)
        (if (eq (kind field-type) 'object-type)
            field-type
            (resolve-abstract-type field-type result))
        result
        variable-values)))))

(defun coerce-result (leaf-type value)
  ;; TODO: https://spec.graphql.org/draft/#CoerceResult()
  (declare (ignorable leaf-type value))
  (gql-error "TODO: coerce-result not implemented"))

(defun resolve-abstract-type (abstract-type object-value)
  ;; TODO: https://spec.graphql.org/draft/#ResolveAbstractType()
  (declare (ignorable abstract-type object-value))
  (gql-error "TODO: resolve-abstract-type not implemented"))

(defun execute-field (object-type object-value field-type fields variable-values)
  ;; TODO: https://spec.graphql.org/draft/#sec-Executing-Fields
  (let* ((field (car fields))
         (field-name (name-or-alias field))
         (arg-values (coerce-args object-type field variable-values))
         (resolved-value
           (resolve-field-value object-type object-value field-name arg-values)))
    (complete-value field-type fields resolved-value variable-values)))

(declaim (ftype (function (document operation-definition hash-table) hash-table) coerce-vars))
(defun coerce-vars (schema operation variable-values)
  ;; TODO: https://spec.graphql.org/draft/#CoerceVariableValues()
  (declare (ignorable schema)) ;; The assumption about this schema is that it
                               ;; should be used by the `execute-*' defuns
  (with-slots (variable-definitions) operation
    (loop
      :with coerced-vars = (make-hash-table :test #'equal)
      :for variable :in variable-definitions
      :for var-name = (name (name (var variable)))
      :for var-type = (var-type variable)
      :when (input-type-p var-type)
        :do (with-slots (default-value) variable
              (multiple-value-bind (val val-p) (gethash var-name variable-values)
                (cond
                  ((and (null val-p) default-value)
                   (sethash default-value var-name coerced-vars))
                  ((and (eq (kind var-type) 'non-null-type)
                        (or (null val-p) (null val)))
                   (gql-error "Need to raise a request error for coerce-vars"))
                  (val-p
                   (if (null val)
                       (sethash nil var-name coerced-vars)
                       (let (;; TODO: Coerce the val first for the else part,
                             ;; find out how
                             (coerced-value t))
                         (sethash coerced-value var-name coerced-vars)))))))
      :finally (return coerced-vars))))

(defun execute-request (schema document operation-name variable-values initial-value)
  ;; https://spec.graphql.org/draft/#sec-Executing-Requests
  ;;
  ;; TODO: Still with the *schema*.  This function is supplied a schema as an
  ;; argument as defined in the spec, but for now we use the global thing.
  (let* ((operation (get-operation document operation-name))
         (coerced-vars (coerce-vars schema operation variable-values)))
    (string-case (operation-type operation)
      ("Query"        (execute-query operation schema coerced-vars initial-value))
      ("Mutation"     (execute-mutation operation schema coerced-vars initial-value))
      ("Subscription" (subscribe operation schema coerced-vars initial-value)))))

(defun execute-subscription-event (subscription schema variable-values initial-value)
  ;; TODO: https://spec.graphql.org/draft/#ExecuteSubscriptionEvent()
  (declare (ignorable subscription schema variable-values initial-value))
  t)

(defun create-source-event-stream (subscription schema variable-values initial-value)
  ;; TODO: https://spec.graphql.org/draft/#CreateSourceEventStream()
  (declare (ignorable subscription schema variable-values initial-value))
  t)

(defun resolve-field-event-stream (subscription-type root-value field-name argument-values)
  ;; TODO: https://spec.graphql.org/draft/#ResolveFieldEventStream()
  (declare (ignorable subscription-type root-value field-name argument-values))
  t)

(defun map-source-to-response-event (source-stream subscription schema variable-values)
  ;; TODO: https://spec.graphql.org/draft/#MapSourceToResponseEvent()
  (declare (ignorable source-stream subscription schema variable-values))
  t)

(defun merge-selection-sets (fields)
  ;; TODO: https://spec.graphql.org/draft/#MergeSelectionSets()
  (loop
    :with selection-set = nil
    :for field :in fields
    :for field-selection-set = (selection-set field)
    :when field-selection-set
      :do (setf selection-set (append selection-set field-selection-set))
    :finally (return selection-set)))
