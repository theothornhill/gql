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
