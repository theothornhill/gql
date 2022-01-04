(in-package #:gql)

(defmacro defobject (name description fields &optional interfaces)
  (check-type name string)
  (check-type description string)
  (check-type fields list)
  (check-type interfaces list)
  `(ensure-object ,name
     :description ,description
     :fields ,(canonicalize-fields fields)
     :interfaces ,(canonicalize-interfaces interfaces)))

(defun ensure-object (name &rest all-keys)
  (let ((object (apply #'make-instance
                       'object-type-definition
                       :kind 'object-type-definition
                       :name (make-name name) all-keys)))
    (setf (find-item name) object)
    object))

(defmacro definterface (name description fields &optional directives)
  (check-type name string)
  (check-type description string)
  (check-type fields list)
  (check-type directives list)
  `(ensure-interface ,name
     :description ,description
     :fields ,(canonicalize-fields fields)
     :directives ,directives))

(defun ensure-interface (name &rest all-keys)
  (let ((object (apply #'make-instance
                       'interface-type-definition
                       :kind 'interface-type-definition
                       :name (make-name name) all-keys)))
    (setf (find-item name) object)
    object))

(defmacro defenum (name description enum-values)
  (check-type name string)
  (check-type description string)
  (check-type enum-values list)
  `(ensure-enum ,name
     :description ,description
     :enum-values ,(canonicalize-enum-values enum-values)))

(defun ensure-enum (name &rest all-keys)
  (let ((object (apply #'make-instance
                       'enum-type-definition
                       :kind 'enum-type-definition
                       :name (make-name name) all-keys)))
    (setf (find-item name) object)
    object))

(let ((items (make-hash-table :test #'equal)))
  (defun find-item (symbol &optional (errorp t))
    (let ((item (gethash symbol items nil)))
      (if (and (null item) errorp)
          (error "No item named ~S." item)
          item)))

  (defun find-items (names)
    (mapcar #'find-item names))

  (defun clear-items (&optional (truly-clear nil))
    (if truly-clear (clrhash items)
        (maphash
         (lambda (k _) (declare (ignore _))
           (unless (uiop:string-prefix-p "__" k)
             ;; Don't remove the introspection types by default
             (remhash k items)))
         items)))

  (defun (setf find-item) (new-value name)
    (setf (gethash name items) new-value)))

(defun canonicalize-field (field)
  `(field :name ,(car field) ,@(cdr field)))

(defun canonicalize-fields (fields)
  `(list ,@(mapcar #'canonicalize-field fields)))

(defun canonicalize-interfaces (interfaces)
  interfaces)

(defun canonicalize-enum-value (enum-value)
  `(enum-val :enum-value (make-name ,enum-value)))

(defun canonicalize-enum-values (enum-values)
  `(list ,@(mapcar #'canonicalize-enum-value enum-values)))
