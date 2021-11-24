(in-package #:gql)


(defun sethash (item key table)
  ;; TODO: Do we need to check for present-ness if nil is just appendable?
  (let ((items (if (listp item) item (list item))))
    (setf (gethash key table) (append (gethash key table) items))))

(defun fragment-type-applies-p (object-type fragment-type)
  (cond
    ((string= object-type (name (name fragment-type))) t)
    ;; TODO: Handle case 2 and 3 from
    ;; https://spec.graphql.org/draft/#DoesFragmentTypeApply()
    ;; 
    ;; We need the interface type checking and union type checking, but let's do
    ;; that later.
    (t nil)))

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
              (field (sethash selection (name name) grouped-fields))
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
                                        (collect-fields object-type
                                                        selection-set
                                                        variable-values
                                                        visited-fragments)))))))))))
              (inline-fragment
               (with-slots (type-condition) selection
                 (unless (and (not (null type-condition))
                              (not (fragment-type-applies-p object-type type-condition)))
                   (with-slots (selection-set) selection
                     (maphash (lambda (key value) (sethash value key grouped-fields))
                              (collect-fields object-type
                                              selection-set
                                              variable-values
                                              visited-fragments)))))))))
    :finally (return grouped-fields)))

