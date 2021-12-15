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

(defun make-schema (&key query mutation subscription types directives extensions)
  ;; TODO: We do want this check at some point
  ;; (check-type query object-type-definition)
  (let ((schema (make-instance 'schema
                               :query-type query
                               :mutation-type mutation
                               :subscription-type subscription
                               ;; TODO: Provide include/skip by default?
                               :directives directives
                               :extensions extensions))
        (type-map (make-hash-table :test #'equal))
        (initial-types nil))
    (when (query-type schema)
      (push (query-type schema) initial-types))
    (when (mutation-type schema)
      (push (mutation-type schema) initial-types))
    (when (subscription-type schema)
      (push (subscription-type schema) initial-types))

    (when *__schema*
      (push *__schema* initial-types))
    (when *__type*
      (push *__type* initial-types))
    (when *__type-kind*
      (push *__type-kind* initial-types))
    (when *__field*
      (push *__field* initial-types))
    (when *__input-value*
      (push *__input-value* initial-types))
    (when *__enum-value*
      (push *__enum-value* initial-types))
    (when *__directive*
      (push *__directive* initial-types))
    (when *__directive-location*
      (push *__directive-location* initial-types))

    (dolist (type types)
      (push type initial-types))

    (dolist (type initial-types)
      ;; TODO: Error handling
      (type-map-reducer schema type-map type)
      (mapify-type-definitions type))

    (setf (type-map schema) type-map)
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
           (setf (gethash (enum-value enum-val) table) enum-val))
         (setf (enum-values object-type) table)))
      (union-type-definition
       (when (listp (union-members object-type))
         (dolist (union-member (union-members object-type))
           (setf (gethash (nameof union-member) table) union-member))
         (setf (union-members object-type) table))))
    object-type))
