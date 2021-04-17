(in-package :gql)

(defclass lexer ()
  ((source
    :initarg :source
    :accessor source
    :documentation "Source file to scan.")
   (last-token
    :initarg :last-token
    :initform nil
    :accessor last-token
    :documentation "The previously focused non-ignored token.")
   (token
    :initarg :token
    :accessor token
    :documentation "The currently focused non-ignored token.")
   (line
    :initarg :line
    :accessor line
    :documentation "The (1-indexed) line containing the current token.")
   (line-start
    :initarg :line-start
    :accessor line-start
    :documentation "The character offset at which the current line begins.")))

(defclass token ()
  ((kind
    :initarg :kind
    :accessor kind
    :documentation "The kind of token.")
   (start
    :initarg :start
    :accessor start
    :documentation "The character offset at which this Node begins.")
   (end
    :initarg :end
    :accessor end
    :documentation "The character offset at which this Node ends.")
   (line
    :initarg :line
    :accessor line
    :documentation "The 1-indexed line number on which this Token appears.")
   (column
    :initarg :column
    :accessor column
    :documentation "The 1-indexed column number at which this Token begins.")
   (value
    :initarg :value
    :accessor value
    :documentation "For non-punctuation tokens, represents the interpreted value of the token.")
   (prev
    :initarg :prev
    :initform nil
    :accessor prev
    :documentation "The previous token.")
   (next
    :initarg :next
    :initform nil
    :accessor next
    :documentation "The next token.")))

(defmethod print-object ((token token) stream)
  (format stream "<TOKEN: value = ~a kind = ~a line = ~a column = ~a>"
          (value token) (kind token) (line token) (column token)))

(defclass source ()
  ((body
    :initarg :body
    :accessor body
    :documentation "The source file to lex and parse.")
   (name
    :initarg :name
    :accessor name
    :documentation "TODO")
   (location-offset
    :initarg :location-offset
    :accessor location-offset
    :documentation "TODO")))

(defclass parser ()
  ((lexer
    :initarg :lexer
    :accessor lexer
    :documentation "The lexer that previously lexed an input file.")))

(defclass location ()
  ((start
    :initarg :start
    :accessor start
    :documentation "The character offset at which this Node begins.")
   (end
    :initarg :end
    :accessor end
    :documentation "The character offset at which this Node ends.")
   (start-token
    :initarg :start-token
    :accessor start
    :documentation "The Token at which this Node begins.")
   (end-token
    :initarg :end-token
    :accessor end
    :documentation "The Token at which this Node ends.")
   (source
    :initarg :source
    :accessor source
    :documentation "The Source document the AST represents.")))

(defclass ast-node ()
  ((kind
    :initarg :kind
    :accessor kind
    :documentation "The kind of node this is.")
   (location
    :initarg :location
    :accessor location
    :documentation "Source location for this node")))

(define-ast-node name
  name)

(define-ast-node document
  definitions)

(define-ast-node executable-definition
  definitions)
(define-ast-node type-system-definition
  definitions)
(define-ast-node type-system-extension
  definitions)

(define-ast-node operation-definition
  operation
  name
  variable-definitions
  directives
  selection-set
  definitions)

(deftype operation-type ()
  '(member query mutation subscription))

(define-ast-node fragment-definition
  definitions)

(define-ast-node selection-set
  selections)

(define-ast-node field
  alias
  name
  arguments
  directives
  selection-set)

(define-ast-node argument
  name
  value)

(define-ast-node fragment-spread
  name
  directives)

(define-ast-node fragment-definition
  name
  type-condition
  directives
  selection-set)

(define-ast-node inline-fragment
  type-condition
  directives
  selection-set)

(define-ast-node int-value
  value)

(define-ast-node float-value
  value)

(define-ast-node string-value
  value
  blockp)

(define-ast-node boolean-value
  value)

(define-ast-node null-value)

(define-ast-node enum-value
  value)

(define-ast-node list-value
  list-values)

(define-ast-node object-value
  fields)

(define-ast-node object-field
  name
  value)

(define-ast-node var
  name)

(define-ast-node variable-definition
  var
  var-type
  default-value
  directives)

(define-ast-node named-type
  name)

(define-ast-node list-type
  ty)

(define-ast-node non-null-type
  ty)

(define-ast-node directive
  name
  arguments)

(define-ast-node schema-definition
  description
  directives
  operation-types)

(define-ast-node operation-type-definition
  operation
  named-type)

(define-ast-node scalar-type-definition
  description
  name
  directives)

(define-ast-node object-type-definition
  description
  name
  interfaces
  directives
  fields)

(define-ast-node field-definition
  description
  name
  args
  ty
  directives)

(define-ast-node input-value-definition
  description
  name
  ty
  default-value
  directives)

(define-ast-node interface-type-definition
  description
  name
  directives
  fields)

(define-ast-node union-type-definition
  description
  name
  directives
  union-members)

(define-ast-node enum-type-definition
  description
  name
  directives
  enum-values)

(define-ast-node enum-value-definition
  description
  enum-value
  directives)

(define-ast-node input-object-type-definition
  description
  name
  directives
  fields)

(define-ast-node directive-definition
  description
  name
  args
  locations)

(define-ast-node schema-extension
  directives
  operation-types)

(define-ast-node scalar-type-extension
  name
  directives)

(define-ast-node object-type-extension
  name
  interfaces
  directives
  fields)

(define-ast-node interface-type-extension
  name
  interfaces
  directives
  fields)

(define-ast-node union-type-extension
  name
  directives
  union-members)

(define-ast-node enum-type-extension
  name
  directives
  enum-values)

(define-ast-node input-object-type-extension
  name
  directives
  fields)
