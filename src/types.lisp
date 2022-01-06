(in-package #:gql)

(defun built-in-scalar-p (scalar)
  (member scalar '("Int" "Float" "String" "Boolean" "ID") :test #'string=))

(deftype built-in-scalar ()
  '(and string (satisfies built-in-scalar-p)))

(deftype wrapper-type ()
  '(member non-null-type list-type))

(deftype input-types ()
  '(member
    scalar-type-definition
    input-object-type-definition
    enum-type-definition))

(deftype output-types ()
  '(member
    scalar-type-definition
    object-type-definition
    enum-type-definition
    interface-type-definition
    union-type-definition))

(deftype bool ()
  '(member true false))

(defun make-name (type)
  (make-instance 'name
                 :name (if (stringp type) type (symbol-name type))
                 :kind 'name))

(defun named (type)
  (check-type type string)
  (make-instance 'named-type
                 :kind 'named-type
                 :name (make-name type)))

(defun list-type (type)
  (make-instance 'list-type
                 :ty type
                 :kind 'list-type))

(defun non-null-type (type)
  (make-instance 'non-null-type
                 :ty type
                 :kind 'non-null-type))

(defun maybe-named (type)
  (if (stringp type) (named type) type))

(defun ! (type)
  (non-null-type (maybe-named type)))

(defun [] (type)
  (list-type (maybe-named type)))

(defun [!] (type)
  ([] (! type)))

(defun [!]! (type)
  (! ([!] type)))

(defvar *int*     (named "Int"))
(defvar *float*   (named "Float"))
(defvar *string*  (named "String"))
(defvar *boolean* (named "Boolean"))
(defvar *id*      (named "ID"))

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

(defun enum-val (&key enum-value description)
  (make-instance 'enum-value-definition
                 :kind 'enum-value
                 :description description
                 :enum-value enum-value))

(defun arg (&key name description type)
  (make-instance 'input-value-definition
                 :kind 'input-value-definition
                 :name (make-name name)
                 :description description
                 :ty type))

(defun set-resolver (type-name field-name fn)
  (let ((field-definition
          (with-slots (type-map) (schema *context*)
            (gethash field-name (fields (gethash type-name type-map ))))))
    (setf (resolver field-definition) fn)))
