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

(defun get-operation (document operation-name)
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
