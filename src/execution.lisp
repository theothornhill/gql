(in-package #:gql)


(defun sethash (item key table)
  ;; TODO: Do we need to check for present-ness if nil is just appendable?
  (let ((items (if (listp item) item (list item))))
    (setf (gethash key table) (append (gethash key table) items))))

(defun fragment-type-applies-p (object-type fragment-type)
  ;; TODO: We need to check all kinds of type designators.  Non-null-types,
  ;; named-types and list-type, at least.
  (let ((type-definition (gethash object-type (all-types))))
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
  (declare (ignorable type))
  ;; TODO: https://spec.graphql.org/draft/#IsInputType()
  ;; (if (or (eq (kind type) 'non-null-type)
  ;;         (eq (kind type) 'list-type)))
  t)

(defun output-type-p (type)
  (declare (ignorable type))
  ;; TODO: https://spec.graphql.org/draft/#IsOutputType()
  ;; (if (or (eq (kind type) 'non-null-type)
  ;;         (eq (kind type) 'list-type)))
  t)

(declaim (ftype (function (operation-definition document hash-table t) hash-table) execute-query))
(defun execute-query (query schema variable-values initial-value)
  ;; TODO: https://spec.graphql.org/draft/#sec-Query
  ;;
  ;; TODO: Still with the schema.  I think we can get away without the dynamic
  ;; var.
  (declare (ignorable schema))
  (let ((query-type (gethash "Query" (all-types))))
    (check-type query-type object-type-definition)
    (with-slots (selection-set) query
      (let ((results (make-hash-table :test #'equal)))
        (setf (gethash "data" results)
              (execute-selection-set selection-set query-type initial-value variable-values))))))

(defun execute-mutation (operation schema coerced-vars initial-value)
  (declare (ignorable operation schema coerced-vars initial-value))
  (gql-error "TODO: execute-mutation not implemented")
  nil)

(defun subscribe (operation schema coerced-vars initial-value)
  (declare (ignorable operation schema coerced-vars initial-value))
  (gql-error "TODO: subscribe not implemented")
  nil)

(defun execute-selection-set (selection-set object-type object-value variable-values)
  ;; TODO: https://spec.graphql.org/draft/#sec-Executing-Selection-Sets
  (declare (ignorable object-value))
  (let ((results (make-hash-table :test #'equal)))
    (maphash (lambda (key value) (cons key value))
             (collect-fields object-type selection-set variable-values))
    results))

(declaim (ftype (function (document operation-definition hash-table) hash-table) coerce-vars))
(defun coerce-vars (schema operation variable-values)
  ;; TODO: https://spec.graphql.org/draft/#CoerceVariableValues()
  (declare (ignorable schema)) ;; The assumptino about this schema is that it
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
                             (coerced-val t))
                         (sethash coerced-val var-name coerced-vars)))))))
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
