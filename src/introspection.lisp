(in-package :gql)

(defvar *__schema*
  (object :name "__Schema"
          :description "A GraphQL Schema defines the capabilities of a GraphQL server."
          :fields `(,(field :name "description"
                            :description "A description of the current schema."
                            :type (named "String")
                            :resolver (lambda () (description *schema*)))
                    ,(field :name "types"
                            :description "A list of all types supported by this server."
                            :type (non-null-type (list-type (non-null-type (named "__Type"))))
                            :resolver (lambda ()
                                        (let ((types nil))
                                          (maphash (lambda (k v)
                                                     (unless (uiop:string-prefix-p "__" k)
                                                       (push v types)))
                                                   (type-map *schema*))
                                          types)))
                    ,(field :name "queryType"
                            :description "The type that query operations will be rooted at."
                            :type (non-null-type (named "__Type"))
                            :resolver (lambda () (query-type *schema*)))
                    ,(field :name "mutationType"
                            :description "If this server supports mutation, the type that mutation operations will be rooted at."
                            :type (named "__Type")
                            :resolver (lambda () (mutation-type *schema*)))
                    ,(field :name "subscriptionType"
                            :description "If this server supports subscription, the type that subscription operations will be rooted at."
                            :type (named "__Type")
                            :resolver (lambda () (subscription-type *schema*)))
                    ,(field :name "directives"
                            :type (non-null-type (list-type (non-null-type "__Directive")))
                            :resolver (lambda () (directives *schema*))))))

(defvar *__type*
  (object :name "__Type"
          :fields `(,(field :name "kind"
                            :type (non-null-type "__TypeKind"))
                    ,(field :name "name"
                            :type (named "String")
                            :resolver (lambda () (name (object-value *execution-context*))))
                    ,(field :name "description"
                            :type (named "String")
                            :resolver (lambda () (description (object-value *execution-context*))))
                    ,(field :name "fields"
                            :type (list-type (non-null-type (named "__Field")))
                            :resolver (lambda () (fields (object-value *execution-context*))))
                    ,(field :name "interfaces"
                            :type (list-type (non-null-type (named "__Type"))))
                    ,(field :name "possibleTypes"
                            :type (list-type (non-null-type (named "__Type"))))
                    ,(field :name "enumValues"
                            :type (list-type (non-null-type (named "__EnumValue"))))
                    ,(field :name "inputFields"
                            :type (list-type (non-null-type (named "__InputValue"))))
                    ,(field :name "ofType"
                            :type (named "__Type")
                            :resolver (lambda () *__type*))
                    ,(field :name "specifiedByUrl"
                            :type (named "String")
                            :resolver (lambda () "Hello")))))

(defvar *__type-kind*
  (enum :name "__TypeKind"
        :description "An enum describing what kind of type a given `__Type` is"
        :enum-values `(,(enum-val :value "SCALAR")
                       ,(enum-val :value "OBJECT")
                       ,(enum-val :value "INTERFACE")
                       ,(enum-val :value "UNION")
                       ,(enum-val :value "ENUM")
                       ,(enum-val :value "INPUT_OBJECT")
                       ,(enum-val :value "LIST")
                       ,(enum-val :value "NON_NULL"))))

(defvar *__field*
  (object :name "__Field"
          :description "Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type."
          :fields `(,(field :name "name"
                            :type (non-null-type (named "String"))
                            :resolver (lambda () (name (object-value *execution-context*))))
                    ,(field :name "description"
                            :type (named "String"))
                    ,(field :name "args"
                            :type (non-null-type (list-type (non-null-type (named "__InputValue")))))
                    ,(field :name "type"
                            :type (non-null-type (named "__Type"))
                            :resolver (lambda () (ty (object-value *execution-context*))))
                    ,(field :name "isDeprecated"
                            :type (non-null-type (named "Boolean")))
                    ,(field :name "deprecationReason"
                            :type (named "String")))))

(defvar *__input-value*
  (object :name "__InputValue"
          :fields `(,(field :name "name"
                            :type (non-null-type (named "String")))
                    ,(field :name "description"
                            :type (named "String"))
                    ,(field :name "type"
                            :type (non-null-type (named "Type")))
                    ,(field :name "defaultValue"
                            :type (named "String")))))

(defvar *__enum-value*
  (object :name "__EnumValue"
          :fields `(,(field :name "name"
                            :type (non-null-type (named "String")))
                    ,(field :name "description"
                            :type (named "String"))
                    ,(field :name "isDeprecated"
                            :type (non-null-type (named "Boolean")))
                    ,(field :name "deprecationReason"
                            :type (named "String")))))

(defvar *__directive*
  (object :name "__Directive"
          :fields `(,(field :name "name"
                            :type (non-null-type (named "String")))
                    ,(field :name "description"
                            :type (named "String"))
                    ,(field :name "location"
                            :type (non-null-type (list-type (non-null-type (named "__DirectiveLocation")))))
                    ,(field :name "args"
                            :type (non-null-type (named "Boolean"))))))

(defvar *__directive-location*
  (enum :name "__DirectiveLocation"
        :enum-values `(,(enum-val :value "QUERY")
                       ,(enum-val :value "MUTATION")
                       ,(enum-val :value "SUBSCRIPTION")
                       ,(enum-val :value "FIELD")
                       ,(enum-val :value "FRAGMENT_DEFINITION")
                       ,(enum-val :value "FRAGMENT_SPREAD")
                       ,(enum-val :value "INLINE_FRAGMENT")
                       ,(enum-val :value "SCHEMA")
                       ,(enum-val :value "SCALAR")
                       ,(enum-val :value "OBJECT")
                       ,(enum-val :value "FIELD_DEFINITION")
                       ,(enum-val :value "ARGUMENT_DEFINITION")
                       ,(enum-val :value "INTERFACE")
                       ,(enum-val :value "UNION")
                       ,(enum-val :value "ENUM")
                       ,(enum-val :value "ENUM_VALUE")
                       ,(enum-val :value "INPUT_OBJECT")
                       ,(enum-val :value "INPUT_FIELD_DEFINITION"))))

(defvar *__schema-field-definition*
  (field :description "Request the schema information."
         :name "__schema"
         :args nil
         :type (non-null-type (named "__Schema"))
         :resolver (lambda () *schema*)))

(defvar *__type-field-definition*
  (field :description "Request the type information of a single type."
         :name "__type"
         :args `(,(make-instance 'input-value-definition
                                 :name (make-name "name")
                                 :description nil
                                 :ty (non-null-type (named "String"))))
         :type (named "__Type")
         :resolver (lambda ()
                     (let* ((args (arg-values *execution-context*))
                            (name (gethash "name" args)))
                       (gethash name (type-map *schema*))))))

(defvar *__typename-field-definition*
  (field :description "The name of the current Object type at runtime."
         :name "__typename"
         :args nil
         :type (non-null-type (named "String"))
         :resolver (lambda () (name (object-type *execution-context*)))))
