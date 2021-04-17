(in-package :gql)

(defmacro defintrospection (name &body slots)
  `(defclass ,name (ast-node)
     ,(loop :for slot :in slots
            :for initarg = (intern (symbol-name slot) :keyword)
            :collect `(,slot :initarg ,initarg :initform nil :accessor ,slot))))

(defintrospection __schema
  types
  query-type
  mutation-type
  subscription-type
  directives)

(defintrospection __type
  kind
  name
  description
  fields
  interfaces
  possible-types
  enum-values
  input-fields
  of-type)

(defintrospection __field
  name               ;; String!
  description        ;; String
  args               ;; [__InputValue!]!
  ty                 ;; __Type!
  isDeprecated       ;; Boolean!
  deprecationReason) ;; String

(defintrospection __input-value
  name          ;; String!
  description   ;; String
  ty            ;; __Type!
  defaultValue) ;; String

(defintrospection __enum-value
  name               ;; String!
  description        ;; String
  isDeprecated       ;; Boolean!
  deprecationReason) ;; String

(deftype __type-kind ()
  '(member
    scalar
    object
    interface
    union
    enum
    input_object
    list
    non_null))

(defintrospection __directive
  name        ;; String!
  description ;; String
  locations   ;; [__DirectiveLocation!]!
  args)       ;; [__InputValue!]!

(deftype __directive-location ()
  '(member
    query
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
