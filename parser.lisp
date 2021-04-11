(in-package #:gql)

(defun make-parser (source)
  (make-instance 'parser :lexer (make-lexer source)))

(defgeneric parse (parser node-type &optional constp)
  (:method :before ((parser parser) node-type &optional (constp nil))
    (declare (ignorable constp))
    (when *debug-print*
      (with-token
        (with-slots (value kind) token
          (format t "; value: ~Vakind: ~Vanode-type: ~Va~%" 10 value 10 kind 10 node-type)))))
  (:documentation "Parse node of NODE-TYPE with parser PARSER."))

(defmacro defparser (node &body body)
  "Convenience macro to define new parser methods.
Specializes on the NODE-TYPE, so if more granular control is needed, either
expand this macro or just use a normal DEFMETHOD."
  `(defmethod parse ((parser parser) (node-type (eql ,node)) &optional (constp nil))
     (declare (ignorable constp))
     ,@body))

(defparser :document
  (with-token
    (make-node 'document :definitions (many parser 'sof :definition 'eof))))

(defparser :definition
  (with-token
    (cond 
      ;; We only peek here so that we can read the whole thing in their
      ;; respective handlers.  This way we can still assert with
      ;; (expect-* parser 'thing) when needed.
      ((peek parser 'name)
       (let ((value (value token)))
         ;; ExecutableDefinition
         (cond
           ((string= value "query")        (parse parser :operation-definition))
           ((string= value "mutation")     (parse parser :operation-definition))
           ((string= value "subscription") (parse parser :operation-definition))

           ((string= value "fragment")     (parse parser :fragment-definition))

           ;; TypeSystemDefinition
           ((string= value "schema")       (parse parser :type-system-definition))
           ((string= value "scalar")       (parse parser :type-system-definition))
           ((string= value "type")         (parse parser :type-system-definition))
           ((string= value "interface")    (parse parser :type-system-definition))
           ((string= value "union")        (parse parser :type-system-definition))
           ((string= value "enum")         (parse parser :type-system-definition))
           ((string= value "input")        (parse parser :type-system-definition))
           ((string= value "directive")    (parse parser :type-system-definition))

           ;; TypeSystemExtension
           ((string= value "extend")       (parse parser :type-system-extension))
           (t (unexpected parser token)))))
      ((peek parser 'brace-l) (parse parser :operation-definition))
      ((peek-description parser)  (parse parser :type-system-definition))
      (t (unexpected parser token)))))

(defparser :operation-definition
  (with-token
    (when (peek parser 'brace-l)
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
                   :selection-set (parse parser :selection-set))))
    ;; Parse the OPERATION-TYPE this so that we traverse over the node if we
    ;; don't error.
    (let ((operation (parse parser :operation-type))
          name)
      (when (peek parser 'name)
        (setf name (parse parser :name)))
      (make-node 'operation-definition
                 :name name
                 :operation operation
                 :variable-definitions (parse parser :variable-definitions)
                 :directives (parse parser :directives)
                 :selection-set (parse parser :selection-set)))))


(defparser :operation-type
  ;; Disallow other names than query, mutation and subscription.
  (let* ((operation-token (expect-token parser 'name))
         (value (value operation-token)))
    (cond
      ((string= value "query") "query")
      ((string= value "mutation") "mutation")
      ((string= value "subscription") "subscription")
      (t (unexpected parser operation-token)))))

(defparser :fragment-definition
  (with-token
    (expect-keyword parser "fragment")
    (make-node 'fragment-definition
               :name (parse parser :fragment-name)
               :type-condition (progn
                                 (expect-keyword parser "on")
                                 (parse parser :named-type))
               :directives (parse parser :directives)
               :selection-set (parse parser :selection-set))))

(defparser :name
  (with-expected-token parser 'name
    (make-node 'name :name (value token))))

(defparser :selection-set
  ;; Selection-set : { selection+ }
  (with-token
    (make-node 'selection-set
               :selections (many parser 'brace-l :selection 'brace-r))))

(defparser :selection
  ;; Selection :
  ;;    - Field
  ;;    - FragmentSpread
  ;;    - InlineFragment
  (if (peek parser 'spread)
      (parse parser :fragment)
      (parse parser :field)))

(defparser :field
  ;; Field : Alias? Name Arguments? Directives SelectionSet
  (with-token
    (let ((name-or-alias (parse parser :name)) alias name)
      (if (expect-optional-token parser 'colon)
          (setf alias name-or-alias name (parse parser :name))
          (setf name name-or-alias))
      (make-node 'field
                 :alias alias
                 :name name
                 :arguments (parse parser :arguments)
                 :directives (parse parser :directives)
                 :selection-set (when (peek parser 'brace-l) (parse parser :selection-set))))))

(defparser :fragment
  (with-token
    (expect-token parser 'spread)
    (let ((type-condition-p (expect-optional-keyword parser "on")))
      (if (and (not type-condition-p) (peek parser 'name))
          (make-node 'fragment-spread
                     :name (parse parser :fragment-name)
                     :directives (parse parser :directives))
          (make-node 'inline-fragment
                     :type-condition (when type-condition-p (parse parser :named-type))
                     :directives (parse parser :directives nil)
                     :selection-set (parse parser :selection-set))))))

(defparser :fragment-name
  ;; Fragment-name : Name but not `on`
  (with-token
    (if (string= (value token) "on")
        (unexpected parser token)
        (parse parser :name))))

(defmethod parse ((parser parser) (node-type (eql :arguments)) &optional (constp nil))
  (let ((item (if constp :const-argument :argument)))
    (optional-many parser 'paren-l item 'paren-r)))

(defparser :argument
  (with-token
    (let ((name (parse parser :name)))
      (expect-token parser 'colon)
      (make-node 'argument
                 :name name
                 :value (parse parser :value)))))

(defparser :const-argument
  (with-token
    (make-node 'argument
               :name (parse parser :name)
               :value (progn
                        (expect-token parser 'colon)
                        (parse parser :value)))))

(defparser :variable-definitions
  (optional-many parser 'paren-l :variable-definition 'paren-r))

(defparser :variable-definition
  (with-token
    (make-node 'variable-definition
               :var (parse parser :var)
               :var-type (progn
                           (expect-token parser 'colon)
                           (parse parser :type-reference))
               :default-value nil
               :directives (parse parser :directives t))))

(defparser :var
  (with-token
    (expect-token parser 'dollar)
    (make-node 'var :name (parse parser :name))))

(defparser :string-value
  (with-token
    (advance (lexer parser))
    (make-node 'string-value
               :value (value token)
               :blockp (when (eq (kind token) 'block-string) t))))

(defparser :value
  (with-token
    (case (kind token)
      (bracket-l (parse parser :list-value constp))
      (brace-l (parse parser :object-value constp))
      (int (progn
             (advance (lexer parser))
             (make-node 'int-value :int-value (value token))))
      (float (progn
               (advance (lexer parser))
               (make-node 'float-value :int-value (value token))))
      ((or string block-string) (parse parser :string-value))
      (name (progn
              (advance (lexer parser))
              (let ((value (value token)))
                (cond
                  ((string= value "true")
                   (make-node 'boolean-value :value t))
                  ((string= value "false")
                   (make-node 'boolean-value :value nil))
                  ((string= value "null")
                   (make-node 'null-value))
                  (t
                   (make-node 'enum-value :value value))))))
      (dollar (unless constp (parse parser :var)))
      (t (unexpected parser token)))))

(defparser :list-value
  (with-token
    (make-node 'list-value
               :list-values (any parser 'bracket-l :value 'bracket-r constp))))

(defparser :object-value
  (with-token
    (make-node 'object-value
               :fields (any parser 'brace-l :object-field 'brace-r constp))))

(defparser :object-value
  (with-token
    (let ((name (parse parser :name)))
      (expect-token parser 'colon)
      (make-node 'object-field
                 :name name
                 :value (parse parser :value constp)))))

(defparser :directives
  ;; Directives[Const] : Directive[?Const]+
  (loop
    with directives
    while (peek parser 'at)
    do (push (parse parser :directive constp) directives)
    finally (return (nreverse directives))))

(defparser :directive
  ;; Directive[Const] : @ Name Arguments[?Const]?
  (with-token
    (expect-token parser 'at)
    (make-node 'directive
               :name (parse parser :name)
               :arguments (parse parser :arguments constp))))

(defparser :named-type
  ;; Directive[Const] : @ Name Arguments[?Const]?
  (with-token
    (make-node 'named-type :name (parse parser :name))))

(defparser :type-reference
  ;; Directive[Const] : @ Name Arguments[?Const]?
  (with-token
    (let ((ty))
      (if (expect-optional-token parser 'bracket-l)
          (let ((inner-type (parse parser :type-reference)))
            (expect-token parser 'bracket-r)
            (setf ty (make-node 'list-type :ty inner-type)))
          (setf ty (parse parser :named-type)))
      (when (expect-optional-token parser 'bang)
        (return-from parse
          (make-node 'non-null-type :ty ty)))
      ty)))

(defun peek-description (parser)
  (or (peek parser 'string) (peek parser 'block-string)))

(defparser :type-system-definition
  (with-token
    (let ((keyword-token
            ;; The token could be a STRING or BLOCK-STRING, and if it is, we
            ;; want to check if the next token is 'schema'.  If it isn't a
            ;; string, just return the token we currently look at.  We still
            ;; haven't advanced beyond the possible docstring. That happens in
            ;; the next parse call.
            (if (peek-description parser) (lookahead (lexer parser)) token)))
      (cond
        ((string= (value keyword-token) "schema") (parse parser :schema-definition))
        ((string= (value keyword-token) "scalar") (parse parser :scalar-type-definition))
        ((string= (value keyword-token) "type") (parse parser :object-type-definition))
        (t (unexpected parser token))))))

(defparser :description
  (when (peek-description parser)
    (parse parser :string-value)))

(defparser :operation-type-definition
  (with-token
    (let ((operation (parse parser :operation-type)))
      (expect-token parser 'colon)
      (let ((named-type (parse parser :named-type)))
        (make-node 'operation-type-definition
                   :operation operation
                   :named-type named-type)))))

(defparser :schema-definition
  (with-token
    ;; A SCHEMA can start with an optional description.  We want to pick that
    ;; off if we can, though it isn't strictly necessary.
    (let ((description (parse parser :description)))
      (expect-keyword parser "schema")
      (let ((directives (parse parser :directives t))
            (operation-types
              (many parser 'brace-l :operation-type-definition 'brace-r)))
        (make-node 'schema-definition
                   :description description
                   :directives directives
                   :operation-types operation-types)))))

(defparser :scalar-type-definition
  (with-token
    ;; A SCALAR-TYPE-DEFINITION can start with an optional description.  We want
    ;; to pick that off if we can, though it isn't strictly necessary.
    (let ((description (parse parser :description)))
      (expect-keyword parser "scalar")
      (let ((name (parse parser :name))
            (directives (parse parser :directives t)))
        (make-node 'scalar-type-definition
                   :description description
                   :name name
                   :directives directives)))))

(defparser :object-type-definition
  (with-token
    ;; A SCALAR-TYPE-DEFINITION can start with an optional description.  We want
    ;; to pick that off if we can, though it isn't strictly necessary.
    (let ((description (parse parser :description)))
      (expect-keyword parser "type")
      (let ((name (parse parser :name))
            (interfaces (parse parser :implements-interfaces))
            (directives (parse parser :directives t))
            (fields (parse parser :fields-definition)))
        (make-node 'object-type-definition
                   :description description
                   :name name
                   :interfaces interfaces
                   :directives directives
                   :fields fields)))))

(defparser :implements-interfaces
  (when (expect-optional-keyword parser "implements")
    (delimited-many parser 'amp :named-type)))

(defparser :fields-definition
  (optional-many parser 'brace-l :field-definition 'brace-r))

(defparser :field-definition
  (with-token
    ;; A FIELD-DEFINITION can start with an optional description.  We want to
    ;; pick that off if we can, though it isn't strictly necessary.
    (let ((description (parse parser :description))
          (name (parse parser :name))
          (args (parse parser :argument-definitions)))
      (expect-token parser 'colon)
      (let ((ty (parse parser :type-reference))
            (directives (parse parser :directives t)))
        (make-node 'field-definition
                   :description description
                   :name name
                   :args args
                   :ty ty
                   :directives directives)))))

(defparser :argument-definitions
  (optional-many parser 'brace-l :input-value-definition 'brace-r))

(defparser :input-value-definition
  (with-token
    ;; A SCALAR-TYPE-DEFINITION can start with an optional description.  We want
    ;; to pick that off if we can, though it isn't strictly necessary.
    (let ((description (parse parser :description))
          (name (parse parser :name)))
      (expect-token parser 'colon)
      (let ((ty (parse parser :type-reference))
            (default-value (parse parser :default-value))
            (directives (parse parser :directives t)))
        (make-node 'input-value-definition
                   :description description
                   :name name
                   :ty ty
                   :default-value default-value
                   :directives directives)))))
