(in-package #:gql)

(defvar *parser* nil
  "The current parser.

Bind dynamically in the top level constructs.  Other functions rely on this
being set to an instance of GQL:PARSER.")

(defvar *token* nil
  "The current token.

Bind dynamically in the top level constructs.  Other functions rely on this
being set to an instance of GQL:TOKEN.")

(defvar *debug-print* nil
  "Set to t if you want to debug the call stack for the parse tree.")

(defvar *schema* nil
  "The current schema in scope for validation and execution.
Note: This is probably not a really good way to do things, as we need to
ensure we have initialized the schema.")

(defvar *all-types* nil
  "Hash-table containing all types from schema *SCHEMA*.
Should be bound together with *schema* when needed.")

(defvar *data* nil
  "Data to be returned to client after validation and execution.")

(defvar *errors* nil
  "Errors to be returned to client after validation and execution.")

(defvar *resolvers* nil
  "Hash table to store the resolvers corresponding to the schema")

(defun built-in-scalar-p (scalar)
  (member scalar '("Int" "Float" "String" "Boolean" "ID") :test #'string=))

(deftype built-in-scalar ()
  '(and string (satisfies built-in-scalar-p)))

(deftype wrapper-type ()
  '(member non-null-type list-type))

(deftype input-types ()
  '(member
    scalar-type-definition
    input-object-type-definition
    enum-type-definition))

(deftype output-types ()
  '(member
    scalar-type-definition
    object-type-definition
    enum-type-definition
    interface-type-definition
    union-type-definition))

(deftype bool ()
  '(member true false))
