(in-package #:gql)

(defclass schema ()
  ((type-map :initarg :type-map :accessor type-map)
   (directives :initarg :directives :accessor directives)
   (query-type :initarg :query-type :accessor query-type)
   (mutation-type :initarg :mutation-type :accessor mutation-type)
   (subscription-type :initarg :subscription-type :accessor subscription-type)
   (implementations :initarg :implementations :accessor implementations)
   (possible-type-map :initarg :possible-type-map :accessor possible-type-map)
   (extensions :initarg :extensions :accessor extensions)))

(defmacro defschema ((&key query types) &body all-keys)
  `(ensure-schema ,query ,types ',@all-keys))

(defun ensure-schema (query types all-keys)
  ;; TODO: We do want this check at some point
  ;; (check-type query object-type-definition)
  (let ((schema (make-instance 'schema
                               :query-type query
                               :mutation-type (getf all-keys :mutation)
                               :subscription-type (getf all-keys :subscription)
                               ;; TODO: Provide include/skip by default?
                               :directives (getf all-keys :directives)
                               :extensions (getf all-keys :extensions)))
        (type-map (make-hash-table :test #'equal))
        (initial-types nil))
    (when (query-type schema)
      (push (query-type schema) initial-types))
    (when (mutation-type schema)
      (push (mutation-type schema) initial-types))
    (when (subscription-type schema)
      (push (subscription-type schema) initial-types))

    (dolist (item (find-items '(|__Schema|
                                |__Type|
                                |__TypeKind|
                                |__Field|
                                |__InputValue|
                                |__EnumValue|
                                |__Directive|
                                |__DirectiveLocation|)))
      (push item initial-types))

    (dolist (type types)
      (push type initial-types))

    (dolist (type initial-types)
      ;; TODO: Error handling
      (type-map-reducer schema type-map type)
      (mapify-type-definitions type))

    (setf (type-map schema) type-map)
    (setf (find-item '|Schema|) schema)
    schema))

(defun type-map-reducer (schema type-map object-type)
  ;; TODO: Return errors as well?
  (when (or (null object-type) (equal (nameof object-type) ""))
    (return-from type-map-reducer type-map))

  (typecase object-type
    (list-type
     (when (ty object-type)
       (type-map-reducer schema type-map (ty object-type))))
    (non-null-type
     (when (ty object-type)
       (type-map-reducer schema type-map (ty object-type)))))

  ;; (when (gethash (nameof object-type) type-map)
  ;;   ;; TODO: return an error here because the type already exists?
  ;;   )

  (setf (gethash (nameof object-type) type-map) object-type)

  ;; TODO: Lots more to do here

  type-map)

(defun mapify-type-definitions (object-type)
  (let ((table (make-hash-table :test #'equal)))
    (typecase object-type
      ;; TODO: Do we need to map more things here?
      (object-type-definition
       (let ((interface-map (make-hash-table :test #'equal)))
         (when (listp (fields object-type))
           (dolist (field (fields object-type))
             (setf (gethash (nameof field) table) field))
           (setf (fields object-type) table))
         (when (listp (interfaces object-type))
           (dolist (interface (interfaces object-type))
             (setf (gethash (nameof interface) interface-map) interface))
           (setf (interfaces object-type) interface-map))))

      (interface-type-definition
       (when (listp (fields object-type))
         (dolist (field (fields object-type))
           (setf (gethash (nameof field) table) field))
         (setf (fields object-type) table)))

      (enum-type-definition
       (when (listp (enum-values object-type))
         (dolist (enum-val (enum-values object-type))
           (setf (gethash (name (enum-value enum-val)) table) enum-val))
         (setf (enum-values object-type) table)))

      (union-type-definition
       (when (listp (union-members object-type))
         (dolist (union-member (union-members object-type))
           (setf (gethash (nameof union-member) table) union-member))
         (setf (union-members object-type) table))))
    object-type))

(defmacro defobject (name description fields &optional interfaces)
  (check-type name symbol)
  (check-type description string)
  (check-type fields list)
  (check-type interfaces list)
  `(ensure-item ',name 'object-type-definition
     :description ,description
     :fields ,(canonicalize-fields fields)
     :interfaces ,(canonicalize-interfaces interfaces)))

(defmacro definterface (name description fields &optional directives)
  (check-type name symbol)
  (check-type description string)
  (check-type fields list)
  (check-type directives list)
  `(ensure-item ',name 'interface-type-definition
     :description ,description
     :fields ,(canonicalize-fields fields)
     :directives ,directives))

(defmacro defenum (name description enum-values)
  (check-type name symbol)
  (check-type description string)
  (check-type enum-values list)
  `(ensure-item ',name 'enum-type-definition
     :description ,description
     :enum-values ,(canonicalize-enum-values enum-values)))

(defun ensure-item (name item &rest all-keys)
  (let ((item (apply #'make-instance item
                       :kind item
                       :name (make-name name) all-keys)))
    (setf (find-item name) item)
    item))

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
  `(field :name ',(car field) ,@(cdr field)))

(defun canonicalize-fields (fields)
  `(list ,@(mapcar #'canonicalize-field fields)))

(defun canonicalize-interfaces (interfaces)
  interfaces)

(defun canonicalize-enum-value (enum-value)
  `(enum-val :enum-value (make-name ',enum-value)))

(defun canonicalize-enum-values (enum-values)
  `(list ,@(mapcar #'canonicalize-enum-value enum-values)))
