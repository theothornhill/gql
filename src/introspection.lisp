(in-package :gql)

(defobject "__Schema"
  "A GraphQL Schema defines the capabilities of a GraphQL server."
  (("description" :description "A description of the current schema."
                  :type *string*
                  :resolver (lambda () (description (schema *context*))))
   ("types" :description "A list of all types supported by this server."
            :type ([!]! "__Type")
            :resolver (lambda ()
                        (let ((types nil))
                          (with-slots (type-map) (schema *context*)
                            (maphash (lambda (k v)
                                       (unless (uiop:string-prefix-p "__" k)
                                         (push v types)))
                                     type-map))
                          types)))
   ("queryType" :description "The type that query operations will be rooted at."
                :type (! "__Type")
                :resolver (lambda () (query-type (schema *context*))))
   ("mutationType" :description "If this server supports mutation, the type that mutation operations will be rooted at."
                   :type (named "__Type")
                   :resolver (lambda () (mutation-type (schema *context*))))
   ("subscriptionType" :description "If this server supports subscription, the type that subscription operations will be rooted at."
                       :type (named "__Type")
                       :resolver (lambda () (subscription-type (schema *context*))))
   ("directives" :type ([!]! "__Directive") :resolver (lambda () (directives (schema *context*))))))

(defobject "__Type"
  "A GraphQL Type"
  (("kind" :type (non-null-type "__TypeKind"))
   ("name" :type *string* :resolver (lambda () (name (object-value (execution-context *context*)))))
   ("description" :type *string*
                  :resolver (lambda () (description (object-value (execution-context *context*)))))
   ("fields" :type ([!] "__Field")
             :resolver (lambda ()
                         (with-slots (fields) (object-value (execution-context *context*))
                           (let ((result nil))
                             (maphash (lambda (k v) (declare (ignore k))
                                        (push v fields))
                                      fields)
                             result))))
   ("interfaces" :type ([!] "__Type"))
   ("possibleTypes" :type ([!] "__Type"))
   ("enumValues" :type ([!] "__EnumValue"))
   ("inputFields" :type ([!] "__InputValue"))
   ("ofType" :type (named "__Type") :resolver (lambda () (find-item "__Type")))
   ("specifiedByUrl" :type *string* :resolver (lambda () "Hello"))))

(defenum "__TypeKind"
  "An enum describing what kind of type a given `__Type` is"
  ("SCALAR"
   "OBJECT"
   "INTERFACE"
   "UNION"
   "ENUM"
   "INPUT_OBJECT"
   "LIST"
   "NON_NULL"))

(defobject "__Field"
  "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type."
  (("name"
    :type (! *string*)
    :resolver (lambda () (name (object-value (execution-context *context*)))))
   ("description" :type *string*)
   ("args" :type ([!]! "__InputValue"))
   ("type" :type (! "__Type")
           :resolver (lambda ()
                       (with-slots (object-value) (execution-context *context*)
                         (ty object-value))))
   ("isDeprecated" :type (! *boolean*))
   ("deprecationReason" :type *string*)))

(defobject "__InputValue"
  "A GraphQL input value"
    (("name" :type (! *string*))
     ("description" :type *string*)
     ("type" :type (! "Type"))
     ("defaultValue" :type *string*)))

(defobject "__EnumValue"
  "A GraphQL enum value"
  (("name" :type (! *string*))
   ("description" :type *string*)
   ("isDeprecated" :type (! *boolean*))
   ("deprecationReason" :type *string*)))

(defobject "__Directive"
  "A GraphQL directive value"
  (("name" :type (! *string*))
   ("description" :type *string*)
   ("location" :type ([!]! "__DirectiveLocation"))
   ("args" :type (! *boolean*))))

(defenum "__DirectiveLocation"
  "Enum enumerating Directive Location"
  ("QUERY"
   "MUTATION"
   "SUBSCRIPTION"
   "FIELD"
   "FRAGMENT_DEFINITION"
   "FRAGMENT_SPREAD"
   "INLINE_FRAGMENT"
   "SCHEMA"
   "SCALAR"
   "OBJECT"
   "FIELD_DEFINITION"
   "ARGUMENT_DEFINITION"
   "INTERFACE"
   "UNION"
   "ENUM"
   "ENUM_VALUE"
   "INPUT_OBJECT"
   "INPUT_FIELD_DEFINITION"))

(defvar *__schema-field-definition*
  (field :description "Request the schema information."
         :name "__schema"
         :args nil
         :type (! "__Schema")
         :resolver (lambda () (schema *context*))))

(defvar *__type-field-definition*
  (field :description "Request the type information of a single type."
         :name "__type"
         :args `(,(arg :name "name" :type (! *string*)))
         :type (named "__Type")
         :resolver (lambda ()
                     (with-slots (schema execution-context) *context*
                       (let* ((name (gethash "name" (arg-values execution-context))))
                         (gethash name (type-map schema)))))))

(defvar *__typename-field-definition*
  (field :description "The name of the current Object type at runtime."
         :name "__typename"
         :args nil
         :type (! "String")
         :resolver (lambda () (name (object-type (execution-context *context*))))))
