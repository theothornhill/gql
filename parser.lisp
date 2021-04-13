(in-package #:gql)

(defun make-parser (source)
  (make-instance 'parser :lexer (make-lexer source)))

(defgeneric parse (node-type &optional constp)
  (:method :before (node-type &optional (constp nil))
    (declare (ignorable constp))
    (when *debug-print*
      (with-token
        (with-slots (value kind) *token*
          (format t "; value: ~Vakind: ~Vanode-type: ~Va~%" 10 value 10 kind 10 node-type)))))
  (:documentation "Parse node of NODE-TYPE with parser PARSER."))

(defmacro defparser (node &body body)
  "Convenience macro to define new parser methods.
Specializes on the NODE-TYPE, so if more granular control is needed, either
expand this macro or just use a normal DEFMETHOD."
  `(defmethod parse ((node-type (eql ',node)) &optional (constp nil))
     (declare (ignorable constp))
     (with-token
       (declare (ignorable *token*))
       ,@body)))

(defparser document
  (make-node 'document :definitions (many 'sof 'definition 'eof)))

(defparser definition
  (cond 
    ;; We only peek here so that we can read the whole thing in their
    ;; respective handlers.  This way we can still assert with
    ;; (expect-* parser 'thing) when needed.
    ((peek 'name)
     (let ((value (value *token*)))
       ;; ExecutableDefinition
       (cond
         ((string= value "query")        (parse 'operation-definition))
         ((string= value "mutation")     (parse 'operation-definition))
         ((string= value "subscription") (parse 'operation-definition))

         ((string= value "fragment")     (parse 'fragment-definition))

         ;; TypeSystemDefinition
         ((string= value "schema")       (parse 'type-system-definition))
         ((string= value "scalar")       (parse 'type-system-definition))
         ((string= value "type")         (parse 'type-system-definition))
         ((string= value "interface")    (parse 'type-system-definition))
         ((string= value "union")        (parse 'type-system-definition))
         ((string= value "enum")         (parse 'type-system-definition))
         ((string= value "input")        (parse 'type-system-definition))
         ((string= value "directive")    (parse 'type-system-definition))

         ;; TypeSystemExtension
         ((string= value "extend")       (parse 'type-system-extension))
         (t (unexpected)))))
    ((peek 'brace-l) (parse 'operation-definition))
    ((peek-description)  (parse 'type-system-definition))
    (t (unexpected))))

(defparser operation-definition
  (when (peek 'brace-l)
    ;; We allow for the query shorthand by first checking for the opening
    ;; brace.  If we arrive here we know that we don't have any DIRECTIVES,
    ;; VARIABLE-DEFINITIONS or NAME.  However, we do have the SELECTION-SET.
    ;; We early return to avoid parsing more than we need.
    (return-from parse
      (make-node 'operation-definition
                 :directives nil
                 :variable-definitions nil
                 :name nil
                 :operation "query"
                 :selection-set (parse 'selection-set))))
  ;; Parse the OPERATION-TYPE this so that we traverse over the node if we
  ;; don't error.
  (let ((operation (parse 'operation-type))
        name)
    (when (peek 'name)
      (setf name (parse 'name)))
    (make-node 'operation-definition
               :name name
               :operation operation
               :variable-definitions (parse 'variable-definitions)
               :directives (parse 'directives)
               :selection-set (parse 'selection-set))))

(defparser operation-type
  ;; Disallow other names than query, mutation and subscription.
  (let* ((operation-token (expect-token 'name))
         (value (value operation-token)))
    (cond
      ((string= value "query") "query")
      ((string= value "mutation") "mutation")
      ((string= value "subscription") "subscription")
      (t (unexpected operation-token)))))

(defparser fragment-definition
  (expect-keyword "fragment")
  (make-node 'fragment-definition
             :name (parse 'fragment-name)
             :type-condition (progn
                               (expect-keyword "on")
                               (parse 'named-type))
             :directives (parse 'directives)
             :selection-set (parse 'selection-set)))

(defparser name
  (with-expected-token 'name
    (make-node 'name :name (value token))))

(defparser selection-set
  ;; Selection-set : { selection+ }
  (make-node 'selection-set
             :selections (many 'brace-l 'selection 'brace-r)))

(defparser selection
  ;; Selection :
  ;;    - Field
  ;;    - FragmentSpread
  ;;    - InlineFragment
  (if (peek 'spread)
      (parse 'fragment)
      (parse 'field)))

(defparser field
  ;; Field : Alias? Name Arguments? Directives SelectionSet
  (let ((name-or-alias (parse 'name)) alias name)
    (if (expect-optional-token 'colon)
        (setf alias name-or-alias name (parse 'name))
        (setf name name-or-alias))
    (make-node 'field
               :alias alias
               :name name
               :arguments (parse 'arguments)
               :directives (parse 'directives)
               :selection-set (when (peek 'brace-l) (parse 'selection-set)))))

(defparser fragment
  (expect-token 'spread)
  (let ((type-condition-p (expect-optional-keyword "on")))
    (if (and (not type-condition-p) (peek 'name))
        (make-node 'fragment-spread
                   :name (parse 'fragment-name)
                   :directives (parse 'directives))
        (make-node 'inline-fragment
                   :type-condition (when type-condition-p (parse 'named-type))
                   :directives (parse 'directives nil)
                   :selection-set (parse 'selection-set)))))

(defparser fragment-name
  ;; Fragment-name : Name but not `on`
  (if (string= (value *token*) "on")
      (unexpected)
      (parse 'name)))

(defparser arguments
  (let ((item (if constp 'const-argument 'argument)))
    (optional-many 'paren-l item 'paren-r)))

(defparser argument
  (let ((name (parse 'name)))
    (expect-token 'colon)
    (make-node 'argument
               :name name
               :value (parse 'value))))

(defparser const-argument
  (make-node 'argument
             :name (parse 'name)
             :value (progn
                      (expect-token 'colon)
                      (parse 'value))))

(defparser variable-definitions
  (optional-many 'paren-l 'variable-definition 'paren-r))

(defparser variable-definition
  (make-node 'variable-definition
             :var (parse 'var)
             :var-type (progn
                         (expect-token 'colon)
                         (parse 'type-reference))
             :default-value nil
             :directives (parse 'directives t)))

(defparser var
  (expect-token 'dollar)
  (make-node 'var :name (parse 'name)))

(defparser string-value
  (advance-one-token)
  (make-node 'string-value
             :value (value *token*)
             :blockp (when (peek 'block-string) t)))

(defparser value
  (case (kind *token*)
    (bracket-l (parse 'list-value constp))
    (brace-l (parse 'object-value constp))
    (int (progn
           (advance-one-token)
           (make-node 'int-value :value (value *token*))))
    (float (progn
             (advance-one-token)
             (make-node 'float-value :value (value *token*))))
    ((or string block-string) (parse 'string-value))
    (name (progn
            (advance-one-token)
            (let ((value (value *token*)))
              (cond
                ((string= value "true")
                 (make-node 'boolean-value :value t))
                ((string= value "false")
                 (make-node 'boolean-value :value nil))
                ((string= value "null")
                 (make-node 'null-value))
                (t
                 (make-node 'enum-value :value value))))))
    (dollar (unless constp (parse 'var)))
    (t (unexpected))))

(defparser list-value
  (make-node 'list-value
             :list-values (any 'bracket-l 'value 'bracket-r constp)))

(defparser object-value
  (make-node 'object-value
             :fields (any 'brace-l 'object-field 'brace-r constp)))

(defparser object-value
  (let ((name (parse 'name)))
    (expect-token 'colon)
    (make-node 'object-field
               :name name
               :value (parse 'value constp))))

(defparser enum-value
  (let ((val (value *token*)))
    (if (or (string= "true" val) (string= "false" val) (string= "null" val))
        (unexpected)
        (parse 'name))))

(defparser directives
  ;; Directives[Const] : Directive[?Const]+
  (loop
    with directives
    while (peek 'at)
    do (push (parse 'directive constp) directives)
    finally (return (nreverse directives))))

(defparser directive
  ;; Directive[Const] : @ Name Arguments[?Const]?
  (expect-token 'at)
  (make-node 'directive
             :name (parse 'name)
             :arguments (parse 'arguments constp)))

(defparser named-type
  ;; Directive[Const] : @ Name Arguments[?Const]?
  (make-node 'named-type :name (parse 'name)))

(defparser type-reference
  ;; Directive[Const] : @ Name Arguments[?Const]?
  (let ((ty))
    (if (expect-optional-token 'bracket-l)
        (let ((inner-type (parse 'type-reference)))
          (expect-token 'bracket-r)
          (setf ty (make-node 'list-type :ty inner-type)))
        (setf ty (parse 'named-type)))
    (when (expect-optional-token 'bang)
      (return-from parse
        (make-node 'non-null-type :ty ty)))
    ty))

(defun peek-description ()
  (or (peek 'string) (peek 'block-string)))

(defparser type-system-definition
  (let ((keyword-token
          ;; The token could be a STRING or BLOCK-STRING, and if it is, we
          ;; want to check if the next token is 'schema'.  If it isn't a
          ;; string, just return the token we currently look at.  We still
          ;; haven't advanced beyond the possible docstring. That happens in
          ;; the next parse call.
          (if (peek-description) (lookahead (lexer *parser*)) *token*)))
    (cond
      ((string= (value keyword-token) "schema")    (parse 'schema-definition))
      ((string= (value keyword-token) "scalar")    (parse 'scalar-type-definition))
      ((string= (value keyword-token) "type")      (parse 'object-type-definition))
      ((string= (value keyword-token) "interface") (parse 'interface-type-definition))
      ((string= (value keyword-token) "union")     (parse 'union-type-definition))
      ((string= (value keyword-token) "enum" )     (parse 'enum-type-definition))
      ((string= (value keyword-token) "input" )    (parse 'input-object-type-definition))
      ((string= (value keyword-token) "directive") (parse 'directive-definition))
      (t (unexpected)))))

(defparser description
  (when (peek-description)
    (parse 'string-value)))

(defparser operation-type-definition
  (let ((operation (parse 'operation-type)))
    (expect-token 'colon)
    (let ((named-type (parse 'named-type)))
      (make-node 'operation-type-definition
                 :operation operation
                 :named-type named-type))))

(defparser schema-definition
  (let ((description (parse 'description)))
    (expect-keyword "schema")
    (let ((directives (parse 'directives t))
          (operation-types
            (many 'brace-l 'operation-type-definition 'brace-r)))
      (make-node 'schema-definition
                 :description description
                 :directives directives
                 :operation-types operation-types))))

(defparser scalar-type-definition
  (let ((description (parse 'description)))
    (expect-keyword "scalar")
    (let ((name (parse 'name))
          (directives (parse 'directives t)))
      (make-node 'scalar-type-definition
                 :description description
                 :name name
                 :directives directives))))

(defparser object-type-definition
  (let ((description (parse 'description)))
    (expect-keyword "type")
    (let ((name (parse 'name))
          (interfaces (parse 'implements-interfaces))
          (directives (parse 'directives t))
          (fields (parse 'fields-definition)))
      (make-node 'object-type-definition
                 :description description
                 :name name
                 :interfaces interfaces
                 :directives directives
                 :fields fields))))

(defparser implements-interfaces
  (when (expect-optional-keyword "implements")
    (delimited-many 'amp 'named-type)))

(defparser fields-definition
  (optional-many 'brace-l 'field-definition 'brace-r))

(defparser field-definition
  (let ((description (parse 'description))
        (name (parse 'name))
        (args (parse 'argument-definitions)))
    (expect-token 'colon)
    (let ((ty (parse 'type-reference))
          (directives (parse 'directives t)))
      (make-node 'field-definition
                 :description description
                 :name name
                 :args args
                 :ty ty
                 :directives directives))))

(defparser argument-definitions
  (optional-many 'paren-l 'input-value-definition 'paren-r))

(defparser input-value-definition
  (let ((description (parse 'description))
        (name (parse 'name)))
    (expect-token 'colon)
    (let ((ty (parse 'type-reference))
          (default-value (parse 'default-value t))
          (directives (parse 'directives t)))
      (make-node 'input-value-definition
                 :description description
                 :name name
                 :ty ty
                 :default-value default-value
                 :directives directives))))

(defparser default-value
  (when (expect-optional-token 'equals)
    (parse 'value t)))

(defparser interface-type-definition
  (let ((description (parse 'description)))
    (expect-keyword "interface")
    (let ((name (parse 'name))
          (directives (parse 'directives t))
          (fields (parse 'fields-definition)))
      (make-node 'interface-type-definition
                 :description description
                 :name name
                 :directives directives
                 :fields fields))))

(defparser union-type-definition
  (let ((description (parse 'description)))
    (expect-keyword "union")
    (let ((name (parse 'name))
          (directives (parse 'directives t))
          (union-members (parse 'union-member-types)))
      (make-node 'union-type-definition
                 :description description
                 :name name
                 :directives directives
                 :union-members union-members))))

(defparser union-member-types
  (when (expect-token 'equals)
    (delimited-many 'pipe 'named-type)))

(defparser enum-type-definition
  (let ((description (parse 'description)))
    (expect-keyword "enum")
    (let ((name (parse 'name))
          (directives (parse 'directives t))
          (enum-values (parse 'enum-values)))
      (make-node 'enum-type-definition
                 :description description
                 :name name
                 :directives directives
                 :enum-values enum-values))))

(defparser enum-values
  (optional-many 'brace-l 'enum-value-definition 'brace-r))

(defparser enum-value-definition
  (let ((description (parse 'description))
        (enum-value (parse 'enum-value))
        (directives (parse 'directives t)))
    (make-node 'enum-value-definition
               :description description
               :enum-value enum-value
               :directives directives)))

(defparser input-object-type-definition
  (let ((description (parse 'description)))
    (expect-keyword "input")
    (let ((name (parse 'name))
          (directives (parse 'directives t))
          (fields (parse 'input-fields-definition)))
      (make-node 'input-object-type-definition
                 :description description
                 :name name
                 :directives directives
                 :fields fields))))

(defparser input-fields-definition
  (optional-many 'brace-l 'input-value-definition 'brace-r))

(defparser directive-definition
  (let ((description (parse 'description)))
    (expect-keyword "directive")
    (expect-token 'at)
    (let ((name (parse 'name))
          (args (parse 'argument-definitions)))
      (expect-keyword "on")
      (make-node 'directive-definition
                 :description description
                 :name name
                 :args args
                 :locations (parse 'directive-locations)))))

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
