(in-package :gql)

(defmacro defintrospection (name &body slots)
  `(defclass ,name (ast-node)
     ,(loop :for slot :in slots
            :for initarg = (intern (symbol-name slot) :keyword)
            :collect `(,slot :initarg ,initarg :initform nil :accessor ,slot))))

;;; meta fields
;;; __schema: __schema!
;;; __type(name: string!): __type

(defintrospection __schema
  types             ;; [__type!]!
  query-type        ;; __type!
  mutation-type     ;; __type
  subscription-type ;; __type
  directives)       ;; [__directive!]!

(defintrospection __type
  kind           ;; __type-kind!
  name           ;; string
  description    ;; string
  fields         ;; (include-deprecated: boolean = false): [__field!]
  interfaces     ;; [__type!]
  possible-types ;; [__type!]
  enum-values    ;; (include-deprecated: boolean = false): [__enum-value!]
  input-fields   ;; [__input-value!]
  of-type)       ;; __type

(defintrospection __field
  name               ;; string!
  description        ;; string
  args               ;; [__input-value!]!
  ty                 ;; __type!
  isdeprecated       ;; boolean!
  deprecationreason) ;; string

(defintrospection __input-value
  name          ;; string!
  description   ;; string
  ty            ;; __type!
  defaultvalue) ;; string

(defintrospection __enum-value
  name               ;; string!
  description        ;; string
  isdeprecated       ;; boolean!
  deprecationreason) ;; string

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
  name        ;; string!
  description ;; string
  locations   ;; [__directive-location!]!
  args)       ;; [__input-value!]!

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
