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

(defvar *data* nil
  "Data to be returned to client after validation and execution.")

(defvar *errors* nil
  "Errors to be returned to client after validation and execution.")
