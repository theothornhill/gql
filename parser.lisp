(in-package #:gql)

(defclass* parser
  lexer)

(defun make-parser (source)
  (make-instance 'parser :lexer (make-lexer source)))

(defclass* location
  start
  end
  start-token
  end-token
  source)

(defclass* ast-node
  kind
  location)

(defparser definition
  (cond 
    ;; We only peek here so that we can read the whole thing in their
    ;; respective handlers.  This way we can still assert with
    ;; (expect-* parser 'thing) when needed.
    ((peek 'name)
     (string-case (value *token*)
       ;; ExecutableDefinition
       ("query"        (parse 'operation-definition))
       ("mutation"     (parse 'operation-definition))
       ("subscription" (parse 'operation-definition))

       ("fragment"     (parse 'fragment-definition))

       ;; TypeSystemDefinition
       ("schema"       (parse 'type-system-definition))
       ("scalar"       (parse 'type-system-definition))
       ("type"         (parse 'type-system-definition))
       ("interface"    (parse 'type-system-definition))
       ("union"        (parse 'type-system-definition))
       ("enum"         (parse 'type-system-definition))
       ("input"        (parse 'type-system-definition))
       ("directive"    (parse 'type-system-definition))

       ;; TypeSystemExtension
       ("extend"       (parse 'type-system-extension))
       (t              (unexpected))))
    ((peek 'brace-l)    (parse 'operation-definition))
    ((peek-description) (parse 'type-system-definition))
    (t                  (unexpected))))

(defparser operation-type
  ;; Disallow other names than query, mutation and subscription.
  (with-expected-token 'name
    (string-case (value *token*)
      ("query"        "query")
      ("mutation"     "mutation")
      ("subscription" "subscription")
      (t              (unexpected *token*)))))

(defparser selection
  (if (peek 'spread)
      (parse 'fragment)
      (parse 'field)))

(defparser fragment
  (expect-token 'spread)
  (let ((type-condition-p (expect-optional-keyword "on")))
    (if (and (not type-condition-p) (peek 'name))
        (parse 'fragment-spread)
        (make-node 'inline-fragment
          :type-condition (when type-condition-p (parse 'named-type))
          :directives (parse 'directives nil)
          :selection-set (parse 'selection-set)))))

(defparser fragment-name
  (if (string= (value *token*) "on")
      (unexpected)
      (parse 'name)))

(defparser arguments
  (optional-many
   'paren-l
   (if constp 'const-argument 'argument)
   'paren-r))

(defparser const-argument
  (make-node 'argument
    :name (parse 'name)
    :value (expect-then-parse 'colon 'value t)))

(defparser variable-definitions
  (optional-many 'paren-l 'variable-definition 'paren-r))

(defparser value
  (case (kind *token*)
    (bracket-l
     (parse 'list-value constp))
    (brace-l
     (parse 'object-value constp))
    (int
     (parse 'int-value))
    (float
     (parse 'float-value))
    ((string block-string)
     (parse 'string-value))
    (name
     (string-case (advance-then-value)
       ("true"  (make-node 'boolean-value :value t))
       ("false" (make-node 'boolean-value :value nil))
       ("null"  (parse 'null-value))
       (t       (make-node 'enum-value :value (value *token*)))))
    (dollar
     (unless constp (parse 'var)))
    (t (unexpected))))

(defparser directives
  (loop
    :with directives
    :while (peek 'at)
    :do (push (parse 'directive constp) directives)
    :finally (return (nreverse directives))))

(defparser type-reference
  (let ((ty (if (expect-optional-token 'bracket-l)
                (parse 'list-type)
                (parse 'named-type))))
    (if (expect-optional-token 'bang)
        (make-node 'non-null-type :ty ty)
        ty)))

(defparser description
  (when (peek-description)
    (parse 'string-value)))

(defparser implements-interfaces
  (when (expect-optional-keyword "implements")
    (delimited-many 'amp 'named-type)))

(defparser fields-definition
  (optional-many 'brace-l 'field-definition 'brace-r))

(defparser argument-definitions
  (optional-many 'paren-l 'input-value-definition 'paren-r))

(defparser default-value
  (when (expect-optional-token 'equals)
    (parse 'value t)))

(defparser union-member-types
  (when (expect-token 'equals)
    (delimited-many 'pipe 'named-type)))

(defparser enum-values
  (optional-many 'brace-l 'enum-value-definition 'brace-r))

(defparser input-fields-definition
  (optional-many 'brace-l 'input-value-definition 'brace-r))

(defparser directive-locations
  (delimited-many 'pipe 'directive-location))

(defvar *executable-directive-location*
  '("query" "mutation" "subscription" "field" "fragment_definition"
    "fragment_spread" "inline_fragment"))

(defvar *type-system-directive-location*
  '("schema" "scalar" "object" "field_definition" "argument_definition"
    "interface" "union" "enum" "enum_value" "input_object" "input_field_definition"))

(defparser directive-location
  (let ((name (parse 'name)))
    (if (some (lambda (x) (string-equal (name name) x))
              (append
               *executable-directive-location*
               *type-system-directive-location*))
        name
        (unexpected))))
