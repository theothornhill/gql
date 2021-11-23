(in-package #:gql)


(defun sethash (item key table)
  ;; TODO: Do we need to check for present-ness if nil is just appendable?
  (let ((items (if (listp item) item (list item))))
    (setf (gethash key table) (append (gethash key table) items))))

(defun fragment-type-applies-p (object-type fragment-type)
  ;; TODO: Just default to t for now
  (cond
    ((string= object-type (name (name fragment-type))) t)
    ;; TODO: Handle case 2 and 3 from
    ;; https://spec.graphql.org/draft/#DoesFragmentTypeApply()
    ;; 
    ;; We need the interface type checking and union type checking, but let's do
    ;; that later.
    (t nil)))

(declaim (ftype (function (hash-table string list list &optional list) hash-table) collect-fields))
(defun collect-fields (fragments
                       object-type
                       selection-set
                       variable-values
                       &optional
                         (visited-fragments nil))
  "Collect all fields from a `selection-set'.
FRAGMENTS is a hash-table of a string key and a `fragment-definition' as value,
and should consist of all fragments in a `document'.  OBJECT-TYPE is the type of
the current object. SELECTION-SET are the `selections' of a `selection-set'.
VARIABLE-VALUES are all the variables supplied with the `document'.
VISITED-FRAGMENTS is a list of the currently visited fragments.  It is an
accumulator of the current state."
  ;; https://spec.graphql.org/draft/#CollectFields()
  ;;
  ;; TODO: Right now we operate on the actual selections of a selection-set, in
  ;; that we pass the list.  We might just rather want to pass the class
  ;; `selection-set', but now sure if that is worth.
  (declare (type list visited-fragments))
  (loop
    :with grouped-fields = (make-hash-table :test #'equal)
    :for selection :in selection-set
    :do (unless (skippable-field-p (directives selection))
          (with-slots (kind name) selection
            (ecase kind
              (field (sethash selection (name name) grouped-fields))
              (fragment-spread
               (with-slots (fragment-name) selection
                 (unless (member (name fragment-name) visited-fragments :test #'equal)
                   (push fragment-name visited-fragments)
                   (let ((fragment (gethash (name fragment-name) fragments)))
                     (when fragment
                       (with-slots (type-condition) fragment
                         ;; More wishful thinking
                         (when (fragment-type-applies-p object-type type-condition)
                           (with-slots (selection-set) fragment
                             (maphash (lambda (key value) (sethash value key grouped-fields))
                                      (collect-fields fragments
                                                      object-type
                                                      (selections selection-set)
                                                      variable-values
                                                      visited-fragments))))))))))
              (inline-fragment
               (with-slots (type-condition) selection
                 (unless (and (not (null type-condition))
                              (not (fragment-type-applies-p object-type type-condition)))
                   (with-slots (selection-set) selection
                     (maphash (lambda (key value) (sethash value key grouped-fields))
                              (collect-fields fragments
                                              object-type
                                              (selections selection-set)
                                              variable-values
                                              visited-fragments)))))))))
    :finally (return grouped-fields)))

