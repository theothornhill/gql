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

(defun get-subscriptions ()
  (remove-if-not
   (lambda (x)
     (and (eq (kind x) 'operation-definition)
          (string= (operation-type x) "Subscription")))
   (definitions *schema*)))

(declaim (ftype (function (hash-table) boolean) introspection-field-p))
(defun introspection-field-p (fields)
  (loop
    :for v :being :each :hash-key :of fields
      :thereis (uiop:string-prefix-p "__" v)))

(defun get-types (node document)
  "Get specific NODE from a DOCUMENT.
This is not tied to the `*schema*', so that it is usable for other kinds of
documents."
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

(defun all-types ()
  "Get all user defined types within a schema."
  (unless *schema*
    (gql-error "Schema not bound, cannot get all-types.  Consider your options."))
  (with-slots (definitions) *schema*
    (let ((node-table (make-hash-table :test #'equal))
          (nodes
            (remove-if-not
             (lambda (x)
               (let ((kind (kind x)))
                 (or
                  (eq kind 'scalar-type-definition)
                  (eq kind 'object-type-definition)
                  (eq kind 'interface-type-definition)
                  (eq kind 'union-type-definition)
                  (eq kind 'enum-type-definition)
                  (eq kind 'input-object-type-definition))))
             definitions)))
      (dolist (node nodes node-table)
        (with-slots (name) node
          (setf (gethash (name name) node-table) node))))))

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

(defmacro with-schema (schema &body body)
  `(let* ((*schema* ,schema)
          (*all-types* (all-types)))
     ,@body))

(defun get-field-definition (field object-type &optional results)
  (let ((field-name (name-or-alias field)))
    (if (string= "__typename" field-name)
        ;; TODO: Is it enough just to set name here?  Do we get interfaces and
        ;; such things?
        (and results (setf (gethash "__typename" results) (nameof object-type)))
        (find-if (lambda (obj) (string= (nameof obj) field-name))
                 (fields (gethash (nameof object-type) *all-types*))))))

(defclass gql-object ()
  ((type-name
    :initarg :type-name
    :accessor type-name
    :initform (gql-error "Need to supply type name. Consult your schema."))))

(defmacro make-resolvers (&body body)
  `(let ((ht (make-hash-table :test #'equal)))
     ,@(mapcar
        (lambda (resolver)
          `(setf (gethash ,(car resolver) ht) ,(cdr resolver)))
        body)
     ht))
