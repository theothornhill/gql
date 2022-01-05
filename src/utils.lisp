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

(defun location-errors (nodes)
  (remove nil
          (mapcar
           (lambda (node)
             (when (location node)
               (with-slots (line column) (start-token (location node))
                 (make-instance
                  'error-location
                  :line line
                  :column column))))
           nodes)))

(defun push-error (message nodes)
  (push (make-instance
         'errors
         :message message
         :locations (location-errors (if (listp nodes) nodes (list nodes)))
         :path nil ;; TODO: We need the path
         :extensions nil) ;; TODO: Do we need extensions?
        *errors*))

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

(defmacro with-context ((&key document variables execution-context) &body body)
  (let ((s (gensym))
        (d (gensym))
        (v (gensym))
        (e (gensym)))
    `(let* ((,s (find-item "Schema"))
            (,d ,document)
            (,v (or ,variables ,(make-hash-table :test #'equal)))
            (,e ,execution-context)
            (*result* (make-hash-table :test #'equal))
            (*errors* nil)
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
