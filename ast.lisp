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

(defmacro defnode (name &body slots)
  `(defclass ,name (ast-node)
     ,(loop :for slot :in slots
            :for initarg = (intern (symbol-name slot) :keyword)
            :collect `(,slot :initarg ,initarg :initform nil :accessor ,slot))))

(defnode name
  name)

(defnode document
  definitions)

(defnode executable-definition
  definitions)
(defnode type-system-definition
  definitions)
(defnode type-system-extension
  definitions)

(defnode operation-definition
  operation
  name
  variable-definitions
  directives
  selection-set
  definitions)

(deftype operation-type ()
  '(member query mutation subscription))

(defnode fragment-definition
  definitions)

(defnode selection-set
  selections)

(defnode field
  alias
  name
  arguments
  directives
  selection-set)

(defnode argument
  name
  value)

(defnode fragment-spread
  name
  directives)

(defnode fragment-definition
  name
  type-condition
  directives
  selection-set)

(defnode inline-fragment
  type-condition
  directives
  selection-set)

(defnode int-value
  value)

(defnode float-value
  value)

(defnode string-value
  value
  blockp)

(defnode boolean-value
  value)

(defnode null-value)

(defnode enum-value
  value)

(defnode list-value
  list-values)

(defnode object-value
  fields)

(defnode object-field
  name
  value)

(defnode var
  name)

(defnode variable-definition
  var
  var-type
  default-value
  directives)

(defnode named-type
  name)

(defnode list-type
  ty)

(defnode non-null-type
  ty)

(defnode directive
  name
  arguments)

(defnode schema-definition
  description
  directives
  operation-types)

(defnode operation-type-definition
  operation
  named-type)

(defnode scalar-type-definition
  description
  name
  directives)

(defnode object-type-definition
  description
  name
  interfaces
  directives
  fields)

(defnode field-definition
  description
  name
  args
  ty
  directives)

(defnode input-value-definition
  description
  name
  ty
  default-value
  directives)

(defnode interface-type-definition
  description
  name
  directives
  fields)

(defnode union-type-definition
  description
  name
  directives
  union-members)

(defnode enum-type-definition
  description
  name
  directives
  enum-values)

(defnode enum-value-definition
  description
  enum-value
  directives)

(defnode input-object-type-definition
  description
  name
  directives
  fields)

(defnode directive-definition
  description
  name
  args
  locations)

(defnode schema-extension
  directives
  operation-types)

(defnode scalar-type-extension
  name
  directives)

(defnode object-type-extension
  name
  interfaces
  directives
  fields)

(defnode interface-type-extension
  name
  interfaces
  directives
  fields)

(defnode union-type-extension
  name
  directives
  union-members)

(defnode enum-type-extension
  name
  directives
  enum-values)

(defnode input-object-type-extension
  name
  directives
  fields)
