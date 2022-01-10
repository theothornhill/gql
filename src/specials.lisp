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

(defvar *result* nil
  "Hash table to contain the results of an execution.")

(defvar *errors* nil
  "Errors to be returned to client after validation and execution.")

(defvar *context* nil
  "Hash table to store the resolvers corresponding to the schema")

(defclass context ()
  ((%schema :initarg :schema :accessor schema)
   (%document :initarg :document :accessor document)
   (%variables :initarg :variables :accessor variables)
   (%execution-context :initarg :execution-context :accessor execution-context)))

(defclass execution-context ()
  ((%object-type :initarg :object-type :accessor object-type)
   (%object-value :initarg :object-value :accessor object-value)
   (%field-definition :initarg :field-definition :accessor field-definition)
   (%field-name :initarg :field-name :accessor field-name)
   (%arg-values :initarg :arg-values :accessor arg-values)))

(defvar *execution-context* nil)
