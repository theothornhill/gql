(in-package :gql)

(defobject |__Schema|
  "A GraphQL Schema defines the capabilities of a GraphQL server."
  ((|description| :description "A description of the current schema."
                  :gql-type *string*
                  :resolver (lambda () (description (schema *context*))))
   (|types| :description "A list of all types supported by this server."
            :gql-type ([!]! "__Type")
            :resolver (lambda ()
                        (let ((types nil))
                          (with-accessors ((type-map type-map)) (schema *context*)
                            (maphash (lambda (k v)
                                       (unless (uiop:string-prefix-p "__" k)
                                         (push v types)))
                                     type-map))
                          types)))
   (|queryType| :description "The type that query operations will be rooted at."
                :gql-type (! "__Type")
                :resolver (lambda () (query-type (schema *context*))))
   (|mutationType| :description "If this server supports mutation, the type that mutation operations will be rooted at."
                   :gql-type (named "__Type")
                   :resolver (lambda () (mutation-type (schema *context*))))
   (|subscriptionType| :description "If this server supports subscription, the type that subscription operations will be rooted at."
                       :gql-type (named "__Type")
                       :resolver (lambda () (subscription-type (schema *context*))))
   (|directives| :gql-type ([!]! "__Directive") :resolver (lambda () (directives (schema *context*))))))

(defobject |__Type|
  "A GraphQL Type"
  ((|kind| :gql-type (non-null-type "__TypeKind"))
   (|name| :gql-type *string* :resolver (lambda () (name (object-value (execution-context *context*)))))
   (|description| :gql-type *string*
                  :resolver (lambda () (description (object-value (execution-context *context*)))))
   (|fields| :gql-type ([!] "__Field")
             :resolver (lambda ()
                         (with-accessors ((fields fields)) (object-value (execution-context *context*))
                           (let ((result nil))
                             (maphash (lambda (k v) (declare (ignore k))
                                        (push v fields))
                                      fields)
                             result))))
   (|interfaces| :gql-type ([!] "__Type"))
   (|possibleTypes| :gql-type ([!] "__Type"))
   (|enumValues| :gql-type ([!] "__EnumValue"))
   (|inputFields| :gql-type ([!] "__InputValue"))
   (|ofType| :gql-type (named "__Type") :resolver (lambda () (find-item "__Type")))
   (|specifiedByUrl| :gql-type *string* :resolver (lambda () "Hello"))))

(defenum |__TypeKind|
  "An enum describing what kind of type a given `__Type` is"
  (scalar
   object
   interface
   union
   enum
   input_object
   list
   non_null))

(defobject |__Field|
  "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type."
  ((|name|
    :gql-type (! *string*)
    :resolver (lambda () (name (object-value (execution-context *context*)))))
   (|description| :gql-type *string*)
   (|args| :gql-type ([!]! "__InputValue"))
   (|type| :gql-type (! "__Type")
           :resolver (lambda ()
                       (with-accessors ((object-value object-value)) (execution-context *context*)
                         (gql-type object-value))))
   (|isDeprecated| :gql-type (! *boolean*))
   (|deprecationReason| :gql-type *string*)))

(defobject |__InputValue|
  "A GraphQL input value"
  ((|name| :gql-type (! *string*))
   (|description| :gql-type *string*)
   (|type| :gql-type (! "Type"))
   (|defaultValue| :gql-type *string*)))

(defobject |__EnumValue|
  "A GraphQL enum value"
  ((|name| :gql-type (! *string*))
   (|description| :gql-type *string*)
   (|isDeprecated| :gql-type (! *boolean*))
   (|deprecationReason| :gql-type *string*)))

(defobject |__Directive|
  "A GraphQL directive value"
  ((|name| :gql-type (! *string*))
   (|description| :gql-type *string*)
   (|location| :gql-type ([!]! "__DirectiveLocation"))
   (|args| :gql-type (! *boolean*))))

(defenum |__DirectiveLocation|
  "Enum enumerating Directive Location"
  (query
   mutation
   subscription
   field
   fragment_definition
   fragment_spread
   inline_fragment
   schema
   scalar
   object
   field_definition
   argument_definition
   interface
   union
   enum
   enum_value
   input_object
   input_field_definition))

(defvar *__schema-field-definition*
  (make-instance 'field-definition
                 :kind 'field-definition
                 :description "Request the schema information."
                 :name (make-name "__schema")
                 :args nil
                 :gql-type (! "__Schema")
                 :resolver (lambda () (schema *context*))))

(defvar *__type-field-definition*
  (make-instance 'field-definition
                 :kind 'field-definition
                 :description "Request the type information of a single type."
                 :name (make-name "__type")
                 :args `(,(arg :name "name" :gql-type (! *string*)))
                 :gql-type (named "__Type")
                 :resolver (lambda ()
                             (with-accessors ((schema schema)
                                              (execution-context execution-context))
                                 *context*
                               (let* ((name (gethash "name" (arg-values execution-context))))
                                 (gethash name (type-map schema)))))))

(defvar *__typename-field-definition*
  (make-instance 'field-definition
                 :kind 'field-definition
                 :description "The name of the current Object type at runtime."
                 :name (make-name "__typename")
                 :args nil
                 :gql-type (! "String")
                 :resolver (lambda () (name (object-type (execution-context *context*))))))
