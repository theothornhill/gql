(in-package :gql)

(defmacro defclass* (name &body slots)
  `(defclass ,name ()
     ,(loop :for slot :in slots
            :for initarg = (intern (symbol-name slot) :keyword)
            :collect `(,slot :initarg ,initarg :initform nil :accessor ,slot))))

(defclass* lexer
  source
  last-token
  token
  line
  line-start)

(defclass* token
  kind
  start
  end
  line
  column
  value
  prev
  next)

(defmethod print-object ((token token) stream)
  (format stream "<TOKEN: kind = ~a line = ~a column = ~a"
          (kind token) (line token) (column token)))

(defclass* source
  body
  name
  location-offset)

(defclass* parser
  lexer)

(defclass* location
  start
  end
  start-token
  end-token
  source)

(defclass* ast-node
  kind
  location)

(defmacro defnode* (name &body slots)
  `(defclass ,name (ast-node)
     ,(loop :for slot :in slots
            :for initarg = (intern (symbol-name slot) :keyword)
            :collect `(,slot :initarg ,initarg :initform nil :accessor ,slot))))

(defnode* name
  name)

(defnode* document
  definitions)

(defnode* type-system-definition
  definitions)

(defnode* type-system-extension
  definitions)

(defnode* operation-definition
  operation-type
  name
  variable-definitions
  directives
  selection-set)

(defnode* selection-set
  selections)

(defnode* field
  alias
  name
  arguments
  directives
  selection-set)

(defnode* argument
  name
  value)

(defnode* fragment-spread
  fragment-name
  directives)

(defnode* fragment-definition
  name
  type-condition
  directives
  selection-set)

(defnode* inline-fragment
  type-condition
  directives
  selection-set)

(defnode* int-value
  value)

(defnode* float-value
  value)

(defnode* string-value
  value
  blockp)

(defnode* boolean-value
  value)

(defnode* null-value)

(defnode* enum-value
  value)

(defnode* list-value
  list-values)

(defnode* object-value
  fields)

(defnode* object-field
  name
  value)

(defnode* var
  name)

(defnode* variable-definition
  var
  var-type
  default-value
  directives)

(defnode* named-type
  name)

(defnode* list-type
  ty)

(defnode* non-null-type
  ty)

(defnode* directive
  name
  arguments)

(defnode* schema-definition
  description
  directives
  operation-types)

(defnode* operation-type-definition
  operation
  named-type)

(defnode* scalar-type-definition
  description
  name
  directives)

(defnode* object-type-definition
  description
  name
  interfaces
  directives
  fields)

(defnode* field-definition
  description
  name
  args
  ty
  directives)

(defnode* input-value-definition
  description
  name
  ty
  default-value
  directives)

(defnode* interface-type-definition
  description
  name
  directives
  fields)

(defnode* union-type-definition
  description
  name
  directives
  union-members)

(defnode* enum-type-definition
  description
  name
  directives
  enum-values)

(defnode* enum-value-definition
  description
  enum-value
  directives)

(defnode* input-object-type-definition
  description
  name
  directives
  fields)

(defnode* directive-definition
  description
  name
  args
  locations)

(defnode* schema-extension
  directives
  operation-types)

(defnode* scalar-type-extension
  name
  directives)

(defnode* object-type-extension
  name
  interfaces
  directives
  fields)

(defnode* interface-type-extension
  name
  directives
  fields)

(defnode* union-type-extension
  name
  directives
  union-members)

(defnode* enum-type-extension
  name
  directives
  enum-values)

(defnode* input-object-type-extension
  name
  directives
  fields)
