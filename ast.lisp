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

(defclass name (ast-node)
  ((name
    :initarg :name
    :accessor name
    :documentation "A GraphQL name node.")))

(defclass document (ast-node)
  ((definitions
    :initarg :definitions
    :accessor definitions
    :documentation "A GraphQL Document describes a complete file or request string operated on by a GraphQL service or client.

A document contains multiple definitions, either executable or representative of a GraphQL type system.")))

;; Definition - https://spec.graphql.org/June2018/#sec-Language.Document
(defclass definition (document) ())
(defclass executable-definition (definition) ())
(defclass type-system-definition (definition) ())
(defclass type-system-extension (definition) ())

;;; Executable definition - https://spec.graphql.org/June2018/#sec-Language.Operations
(defclass operation-definition (executable-definition)
  ((operation
    :initarg :operation
    :accessor operation
    :documentation "There are three types of operations that GraphQL models:

- query – a read-only fetch.
- mutation – a write followed by a fetch.
- subscription – a long-lived request that fetches data in response to source events.
Each operation is represented by an optional operation name and a selection set.")
   (name
    :initarg :name
    :accessor name
    :documentation "An optional name.")
   (variable-definitions
    :initarg :variable-definitions
    :initform nil
    :accessor variable-definitions
    :documentation "An optional list of variable definitions.")
   (directives
    :initarg :directives
    :initform nil
    :accessor directives
    :documentation "An optional list of directives.")
   (selection-set
    :initarg :selection-set
    :initform nil
    :accessor selection-set
    :documentation "A selection set node.")))

(deftype operation-type ()
  '(member query mutation subscription))

(defclass fragment-definition (executable-definition) ())

;; Selection sets - https://spec.graphql.org/June2018/#sec-Selection-Sets
(defclass selection-set (ast-node)
  ((selections
    :initarg :selections
    :accessor selections
    :documentation "A list of selections.

An operation selects the set of information it needs, 
and will receive exactly that information and nothing more, 
avoiding over-fetching and under-fetching data.")))

(defclass selection (ast-node) ())

;; Fields - https://spec.graphql.org/June2018/#sec-Language.Fields
(defclass field (selection)
  ((alias
    :initarg :alias
    :accessor alias)
   (name
    :initarg :name
    :accessor name)
   (arguments
    :initarg :arguments
    :accessor arguments)
   (directives
    :initarg :directives
    :accessor directives)
   (selection-set
    :initarg :selection-set
    :accessor selection-set)))

;; Arguments - https://spec.graphql.org/June2018/#sec-Language.Arguments
(defclass argument (ast-node)
  ((name
    :initarg :name
    :accessor name)
   (value
    :initarg :value
    :accessor value)))

;; Fragments - https://spec.graphql.org/June2018/#sec-Language.Fragments
(defclass fragment-spread (ast-node)
  ((name
    :initarg :name
    :accessor name)
   (directives
    :initarg :directives
    :accessor directives)))

(defclass fragment-definition (ast-node)
  ((name
    :initarg :name
    :accessor name)
   (type-condition
    :initarg :type-condition
    :accessor type-condition)
   (directives
    :initarg :directives
    :accessor directives)
   (selection-set
    :initarg :selection-set
    :accessor selection-set)))

;;; Inline fragments - https://spec.graphql.org/June2018/#sec-Inline-Fragments
(defclass inline-fragment (selection)
  ((type-condition
    :initarg :type-condition
    :accessor type-condition)
   (directives
    :initarg :directives
    :accessor directives)
   (selection-set
    :initarg :selection-set
    :accessor selection-set)))

;; Input values - https://spec.graphql.org/June2018/#sec-Input-Values
(defclass value (ast-node)
  ())

(defclass int-value (value)
  ((value
    :initarg :value
    :accessor value)))

(defclass float-value (value)
  ((value
    :initarg :value
    :accessor value)))

(defclass string-value (value)
  ((value
    :initarg :value
    :accessor value)
   (blockp
    :initarg :blockp
    :accessor blockp)))

(defclass boolean-value (value)
  ((value
    :initarg :value
    :accessor value)))

(defclass null-value (value)
  ())

(defclass enum-value (value)
  ((value
    :initarg :value
    :accessor value)))

(defclass list-value (value)
  ((list-values
    :initarg :list-values
    :accessor list-values)))

(defclass object-value (value)
  ((fields
    :initarg :fields
    :accessor fields)))

(defclass object-field (ast-node)
  ((name
    :initarg :name
    :accessor name)
   (value
    :initarg :value
    :accessor value)))

;; Variables - https://spec.graphql.org/June2018/#sec-Language.Variables
(defclass var (ast-node)
  ((name
    :initarg :name
    :accessor name)))

(defclass variable-definition (ast-node)
  ((var
    :initarg :var
    :accessor var)
   (var-type
    :initarg :var-type
    :accessor var-type)
   (default-value
    :initarg :default-value
    :accessor default-value)
   (directives
    :initarg :directives
    :accessor directives)))

;; Type references - https://spec.graphql.org/June2018/#sec-Type-References
(defclass ty (ast-node) ())

(defclass named-type (ty)
  ((name
    :initarg :name
    :accessor name)))

(defclass list-type (ty)
  ((ty
    :initarg :ty
    :accessor ty)))

(defclass non-null-type (ty)
  ((ty
    :initarg :ty
    :accessor ty)))

(defclass directive (ast-node)
  ((name
    :initarg :name
    :accessor name)
   (arguments
    :initarg :arguments
    :accessor arguments))
  (:documentation "https://spec.graphql.org/June2018/#sec-Language.Directives"))

(defclass schema-definition (ast-node)
  ((description
    :initarg :description
    :accessor description)
   (directives
    :initarg :directives
    :accessor directives)
   (operation-types
    :initarg :operation-types
    :accessor operation-types)))

(defclass operation-type-definition (ast-node)
  ((operation
    :initarg :operation
    :accessor operation)
   (named-type
    :initarg :named-type
    :accessor named-type)))

(defclass scalar-type-definition (ast-node)
  ((description
    :initarg :description
    :accessor description)
   (name
    :initarg :name
    :accessor name)
   (directives
    :initarg :directives
    :accessor directives)))

(defclass object-type-definition (ast-node)
  ((description
    :initarg :description
    :accessor description)
   (name
    :initarg :name
    :accessor name)
   (interfaces
    :initarg :interfaces
    :accessor interfaces)
   (directives
    :initarg :directives
    :accessor directives)
   (fields
    :initarg :fields
    :accessor fields)))

(defclass field-definition (ast-node)
  ((description
    :initarg :description
    :accessor description)
   (name
    :initarg :name
    :accessor name)
   (args
    :initarg :args
    :accessor args)
   (ty
    :initarg :ty
    :accessor ty)
   (directives
    :initarg :directives
    :accessor directives)))

(defclass input-value-definition (ast-node)
  ((description
    :initarg :description
    :accessor description)
   (name
    :initarg :name
    :accessor name)
   (ty
    :initarg :ty
    :accessor ty)
   (default-value
    :initarg :default-value
    :accessor default-value)
   (directives
    :initarg :directives
    :accessor directives)))

(defclass interface-type-definition (ast-node)
  ((description
    :initarg :description
    :accessor description)
   (name
    :initarg :name
    :accessor name)
   (directives
    :initarg :directives
    :accessor directives)
   (fields
    :initarg :fields
    :accessor fields)))

(defclass union-type-definition (ast-node)
  ((description
    :initarg :description
    :accessor description)
   (name
    :initarg :name
    :accessor name)
   (directives
    :initarg :directives
    :accessor directives)
   (union-members
    :initarg :union-members
    :accessor union-members)))

(defclass enum-type-definition (ast-node)
  ((description
    :initarg :description
    :accessor description)
   (name
    :initarg :name
    :accessor name)
   (directives
    :initarg :directives
    :accessor directives)
   (enum-values
    :initarg :enum-values
    :accessor enum-values)))

(defclass enum-value-definition (ast-node)
  ((description
    :initarg :description
    :accessor description)
   (enum-value
    :initarg :enum-value
    :accessor enum-value)
   (directives
    :initarg :directives
    :accessor directives)))

(defclass input-object-type-definition (ast-node)
  ((description
    :initarg :description
    :accessor description)
   (name
    :initarg :name
    :accessor name)
   (directives
    :initarg :directives
    :accessor directives)
   (fields
    :initarg :fields
    :accessor fields)))

(defclass directive-definition (ast-node)
  ((description
    :initarg :description
    :accessor description)
   (name
    :initarg :name
    :accessor name)
   (args
    :initarg :args
    :accessor args)
   (locations
    :initarg :locations
    :accessor locations)))

(defclass schema-extension (ast-node)
  ((directives
    :initarg :directives
    :accessor directives)
   (operation-types
    :initarg :operation-types
    :accessor operation-types)))

(defclass scalar-type-extension (ast-node)
  ((name
    :initarg :name
    :accessor name)
   (directives
    :initarg :directives
    :accessor directives)))

(defclass object-type-extension (ast-node)
  ((name
    :initarg :name
    :accessor name)
   (interfaces
    :initarg :interfaces
    :accessor interfaces)
   (directives
    :initarg :directives
    :accessor directives)
   (fields
    :initarg :fields
    :accessor fields)))

(defclass interface-type-extension (ast-node)
  ((name
    :initarg :name
    :accessor name)
   (interfaces
    :initarg :interfaces
    :accessor interfaces)
   (directives
    :initarg :directives
    :accessor directives)
   (fields
    :initarg :fields
    :accessor fields)))

(defclass union-type-extension (ast-node)
  ((name
    :initarg :name
    :accessor name)
   (directives
    :initarg :directives
    :accessor directives)
   (union-members
    :initarg :union-members
    :accessor union-members)))

(defclass enum-type-extension (ast-node)
  ((name
    :initarg :name
    :accessor name)
   (directives
    :initarg :directives
    :accessor directives)
   (enum-values
    :initarg :enum-values
    :accessor enum-values)))

(defclass input-object-type-extension (ast-node)
  ((name
    :initarg :name
    :accessor name)
   (directives
    :initarg :directives
    :accessor directives)
   (fields
    :initarg :fields
    :accessor fields)))
