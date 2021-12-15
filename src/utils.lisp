(in-package :gql)

(defun slurp (file)
  (uiop:read-file-string file))

(defun skippable-field-p (directives)
  (some (lambda (directive)
          (with-slots (name arguments) directive
            (and (or (string= (name name) "skip")
                     (string= (name name) "include"))
                 ;; TODO: More cases needed here. For example to check up
                 ;; against variable list for what the actual value is.
                 (value (car arguments))
                 t)))
        directives))

(declaim (ftype (function (hash-table) boolean) introspection-field-p))
(defun introspection-field-p (fields)
  (loop
    :for v :being :each :hash-key :of fields
      :thereis (uiop:string-prefix-p "__" v)))

(defun get-fragments ()
  (let ((table (make-hash-table :test #'equal)))
    (maphash (lambda (k v)
               (when (equal (kind v) 'fragment-definition)
                 (setf (gethash k table) v)))
             (type-map (schema *context*)))
    table))

(defun get-types (node document)
  (with-slots (definitions) document
    (let ((node-table (make-hash-table :test #'equal))
          (nodes
            (remove-if-not
             (lambda (x) (equal (kind x) node))
             definitions)))
      (dolist (node nodes node-table)
        (if (name node)
            (setf (gethash (nameof node) node-table) node)
            ;; TODO: In this case we are probably an anonymous operation-definition
            (setf (gethash (operation-type node) node-table) node))))))

(defclass* errors
  message
  locations
  path
  extensions)

(defclass* error-location
  line
  column)

(defun make-error (message nodes)
  (let ((node-list (if (listp nodes) nodes (list nodes))))
    (push (make-instance
           'errors
           :message message
           :locations (mapcar
                       (lambda (node)
                         (let ((start-token (start-token (location node))))
                           (make-instance
                            'error-location
                            :line (line start-token)
                            :column (column start-token))))
                       node-list)
           :path nil
           :extensions nil)
          *errors*)))

(defun name-or-alias (field)
  (with-slots (alias name) field
    (if alias
        (name alias)
        (name name))))

(defun nameof (type)
  (name (name type)))

(defun make-context (&key schema document execution-context)
  (make-instance 'context
                 :schema schema
                 :document document
                 :execution-context execution-context))

(defmacro with-schema (schema &body body)
  `(let ((*schema* ,schema))
     ,@body))

(defmacro with-context ((&key schema document variables execution-context) &body body)
  (let ((s (gensym))
        (d (gensym))
        (v (gensym))
        (e (gensym)))
    `(let* ((,s ,schema)
            (,d ,document)
            (,v (or ,variables ,(make-hash-table :test #'equal)))
            (,e ,execution-context)
            (*context* (make-instance 'context
                                      :schema ,s
                                      :document ,d
                                      :variables ,v
                                      :execution-context ,e)))
       ,@body)))

(defun get-field-definition (field object-type)
  (declare (optimize (debug 3)))
  (let ((field-name (name-or-alias field)))
    (cond ((string= "__typename" field-name) *__typename-field-definition*)
          ((string= "__schema" field-name) *__schema-field-definition*)
          ((string= "__type" field-name) *__type-field-definition*)
          (t
           (let ((object (gethash (nameof object-type) (type-map (schema *context*)))))
             (gethash field-name (fields object)))))))

(defclass gql-object ()
  ((type-name
    :initarg :type-name
    :accessor type-name
    :initform nil ;;(gql-error "Need to supply type name. Consult your schema.")
    )
   (resolver
    :initarg :resolver
    :accessor resolver
    :initform nil ;;(gql-error "Need to supply resolver for gql types")
    )))

(defmacro make-resolvers (&body body)
  `(let ((ht (make-hash-table :test #'equal)))
     ,@(mapcar
        (lambda (resolver)
          `(setf (gethash ,(car resolver) ht) ,(cdr resolver)))
        body)
     ht))

;;; Type system things

(defun make-name (type)
  (check-type type string)
  (make-instance 'name :name type :kind 'name))

(defun named (type)
  (check-type type string)
  (make-instance 'named-type
                 :kind 'named-type
                 :name (make-name type)))

(defun list-type (type)
  ;; TODO: Not done.  What type goes here?
  (make-instance 'list-type
                 :ty type
                 :kind 'list-type))

(defun non-null-type (type)
  (make-instance 'non-null-type
                 :ty type
                 :kind 'non-null-type))

(defun field (&key name type resolver description args)
  (make-instance 'field-definition
                 :kind 'field-definition
                 :description description
                 :args args
                 :ty type ;; TODO: Make sure we can use type instead of ty
                 :name (make-name name)
                 :resolver resolver))

(defun object (&key name fields interfaces description)
  (make-instance 'object-type-definition
                 :kind 'object-type-definition
                 :description description
                 :name (make-name name)
                 :fields fields
                 :interfaces interfaces))

(defun interface (&key name fields directives description)
  (make-instance 'interface-type-definition
                 :kind 'interface-type-definition
                 :description description
                 :name (make-name name)
                 :fields fields
                 :directives directives))

(defun enum (&key name enum-values description)
  (make-instance 'enum-type-definition
                 :kind 'enum-type-definition
                 :enum-values enum-values
                 :description description
                 :name (make-name name)))

(defun enum-val (&key enum-value)
  (make-instance 'enum-value-definition
                 :kind 'enum-value
                 :enum-value enum-value))

(defun set-resolver (type-name field-name fn)
  (declare (optimize (debug 3)))
  (let ((field-definition
          (gethash field-name (fields (gethash type-name (type-map (schema *context*)))))))
    (setf (resolver field-definition) fn)))
