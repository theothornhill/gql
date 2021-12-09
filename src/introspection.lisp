(in-package :gql)

(defmacro defintrospection (name &body slots)
  `(defclass ,name (gql-object)
     ,(loop :for slot :in slots
            :for initarg = (intern (symbol-name slot) :keyword)
            :collect `(,slot :initarg ,initarg :initform nil :accessor ,slot))))

;;; meta fields
;;; __schema: __schema!
;;; __type(name: string!): __type

(defintrospection __schema
  description       ;; string
  types             ;; [__type!]!
  query-type        ;; __type!
  mutation-type     ;; __type
  subscription-type ;; __type
  directives)       ;; [__directive!]!

(defvar *__schema-resolvers*
  (make-resolvers
    ("description"        . 'description)
    ("types"              . 'all-types) ;; TODO: This doesn't take an argument.  Problem?
    ("query-type"         . 'query-type)
    ("mutation-type"      . 'mutation-type)
    ("subscription-type"  . 'subscription-type)
    ("directives"         . 'directives))) 

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

(defvar *__type-resolvers*
  (make-resolvers
    ("kind"           . 'kind)
    ("name"           . 'name)
    ("description"    . 'description)
    ("fields"         . 'fields)
    ("interfaces"     . 'interfaces)
    ("possible-types" . 'possible-types)
    ("enum-values"    . 'enum-values)
    ("input-fields"   . 'input-fields)
    ("of-type"        . 'of-type))) 

(defintrospection __field
  name                ;; string!
  description         ;; string
  args                ;; [__input-value!]!
  ty                  ;; __type!
  is-deprecated       ;; boolean!
  deprecation-reason) ;; string

(defvar *__field-resolvers*
  (make-resolvers
    ("name"               . 'name)
    ("description"        . 'description)
    ("args"               . 'args)
    ("ty"                 . 'ty)
    ("is-deprecated"      . 'is-deprecated)
    ("deprecation-reason" . 'deprecation-reason))) 

(defintrospection __input-value
  name          ;; string!
  description   ;; string
  ty            ;; __type!
  defaultvalue) ;; string

(defvar *__field-resolvers*
  (make-resolvers
    ("name"          . 'name)
    ("description"   . 'description)
    ("ty"            . 'ty)
    ("default-value" . 'default-value))) 

(defintrospection __enum-value
  name                ;; string!
  description         ;; string
  is-deprecated       ;; boolean!
  deprecation-reason) ;; string

(defvar *__field-resolvers*
  (make-resolvers
    ("name"               . 'name)
    ("description"        . 'description)
    ("is-deprecated"      . 'is-deprecated)
    ("deprecation-reason" . 'deprecation-reason))) 

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

(defvar *__directive-resolvers*
  (make-resolvers
    ("name"        . 'name)
    ("description" . 'description)
    ("locations"   . 'locations)
    ("args"        . 'args))) 

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
