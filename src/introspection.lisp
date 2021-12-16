(in-package :gql)

(defvar *__schema*
  (object :name "__Schema"
          :description "A GraphQL Schema defines the capabilities of a GraphQL server."
          :fields `(,(field :name "description"
                            :description "A description of the current schema."
                            :type *string*
                            :resolver (lambda () (description (schema *context*))))
                    ,(field :name "types"
                            :description "A list of all types supported by this server."
                            :type ([!]! "__Type")
                            :resolver (lambda ()
                                        (let ((types nil))
                                          (with-slots (type-map) (schema *context*)
                                            (maphash (lambda (k v)
                                                       (unless (uiop:string-prefix-p "__" k)
                                                         (push v types)))
                                                     type-map))
                                          types)))
                    ,(field :name "queryType"
                            :description "The type that query operations will be rooted at."
                            :type (! "__Type")
                            :resolver (lambda () (query-type (schema *context*))))
                    ,(field :name "mutationType"
                            :description "If this server supports mutation, the type that mutation operations will be rooted at."
                            :type (named "__Type")
                            :resolver (lambda () (mutation-type (schema *context*))))
                    ,(field :name "subscriptionType"
                            :description "If this server supports subscription, the type that subscription operations will be rooted at."
                            :type (named "__Type")
                            :resolver (lambda () (subscription-type (schema *context*))))
                    ,(field :name "directives"
                            :type ([!]! "__Directive")
                            :resolver (lambda () (directives (schema *context*)))))))

(defvar *__type*
  (object :name "__Type"
          :fields `(,(field :name "kind"
                            :type (non-null-type "__TypeKind"))
                    ,(field :name "name"
                            :type *string*
                            :resolver (lambda () (name (object-value (execution-context *context*)))))
                    ,(field :name "description"
                            :type *string*
                            :resolver (lambda () (description (object-value (execution-context *context*)))))
                    ,(field :name "fields"
                            :type ([!] "__Field")
                            :resolver (lambda ()
                                        (with-slots (fields) (object-value (execution-context *context*))
                                          (let ((result nil))
                                            (maphash (lambda (k v) (declare (ignore k))
                                                       (push v fields))
                                                     fields)
                                            result))))
                    ,(field :name "interfaces"
                            :type ([!] "__Type"))
                    ,(field :name "possibleTypes"
                            :type ([!] "__Type"))
                    ,(field :name "enumValues"
                            :type ([!] "__EnumValue"))
                    ,(field :name "inputFields"
                            :type ([!] "__InputValue"))
                    ,(field :name "ofType"
                            :type (named "__Type")
                            :resolver (lambda () *__type*))
                    ,(field :name "specifiedByUrl"
                            :type *string*
                            :resolver (lambda () "Hello")))))

(defvar *__type-kind*
  (enum :name "__TypeKind"
        :description "An enum describing what kind of type a given `__Type` is"
        :enum-values `(,(enum-val :enum-value (make-name "SCALAR"))
                       ,(enum-val :enum-value (make-name "OBJECT"))
                       ,(enum-val :enum-value (make-name "INTERFACE"))
                       ,(enum-val :enum-value (make-name "UNION"))
                       ,(enum-val :enum-value (make-name "ENUM"))
                       ,(enum-val :enum-value (make-name "INPUT_OBJECT"))
                       ,(enum-val :enum-value (make-name "LIST"))
                       ,(enum-val :enum-value (make-name "NON_NULL")))))

(defvar *__field*
  (object :name "__Field"
          :description "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type."
          :fields `(,(field :name "name"
                            :type (! *string*)
                            :resolver (lambda () (name (object-value (execution-context *context*)))))
                    ,(field :name "description"
                            :type *string*)
                    ,(field :name "args"
                            :type ([!]! "__InputValue"))
                    ,(field :name "type"
                            :type (! "__Type")
                            :resolver (lambda ()
                                        (with-slots (object-value) (execution-context *context*)
                                          (ty object-value))))
                    ,(field :name "isDeprecated"
                            :type (! *boolean*))
                    ,(field :name "deprecationReason"
                            :type *string*))))

(defvar *__input-value*
  (object :name "__InputValue"
          :fields `(,(field :name "name"
                            :type (! *string*))
                    ,(field :name "description"
                            :type *string*)
                    ,(field :name "type"
                            :type (! "Type"))
                    ,(field :name "defaultValue"
                            :type *string*))))

(defvar *__enum-value*
  (object :name "__EnumValue"
          :fields `(,(field :name "name"
                            :type (! *string*))
                    ,(field :name "description"
                            :type *string*)
                    ,(field :name "isDeprecated"
                            :type (! *boolean*))
                    ,(field :name "deprecationReason"
                            :type *string*))))

(defvar *__directive*
  (object :name "__Directive"
          :fields `(,(field :name "name"
                            :type (! *string*))
                    ,(field :name "description"
                            :type *string*)
                    ,(field :name "location"
                            :type ([!]! "__DirectiveLocation"))
                    ,(field :name "args"
                            :type (! *boolean*)))))

(defvar *__directive-location*
  (enum :name "__DirectiveLocation"
        :enum-values `(,(enum-val :enum-value (make-name "QUERY"))
                       ,(enum-val :enum-value (make-name "MUTATION"))
                       ,(enum-val :enum-value (make-name "SUBSCRIPTION"))
                       ,(enum-val :enum-value (make-name "FIELD"))
                       ,(enum-val :enum-value (make-name "FRAGMENT_DEFINITION"))
                       ,(enum-val :enum-value (make-name "FRAGMENT_SPREAD"))
                       ,(enum-val :enum-value (make-name "INLINE_FRAGMENT"))
                       ,(enum-val :enum-value (make-name "SCHEMA"))
                       ,(enum-val :enum-value (make-name "SCALAR"))
                       ,(enum-val :enum-value (make-name "OBJECT"))
                       ,(enum-val :enum-value (make-name "FIELD_DEFINITION"))
                       ,(enum-val :enum-value (make-name "ARGUMENT_DEFINITION"))
                       ,(enum-val :enum-value (make-name "INTERFACE"))
                       ,(enum-val :enum-value (make-name "UNION"))
                       ,(enum-val :enum-value (make-name "ENUM"))
                       ,(enum-val :enum-value (make-name "ENUM_VALUE"))
                       ,(enum-val :enum-value (make-name "INPUT_OBJECT"))
                       ,(enum-val :enum-value (make-name "INPUT_FIELD_DEFINITION")))))

(defvar *__schema-field-definition*
  (field :description "Request the schema information."
         :name "__schema"
         :args nil
         :type (! "__Schema")
         :resolver (lambda () (schema *context*))))

(defvar *__type-field-definition*
  (field :description "Request the type information of a single type."
         :name "__type"
         :args `(,(make-instance 'input-value-definition
                                 :name (make-name "name")
                                 :description nil
                                 :ty (! "String")))
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
