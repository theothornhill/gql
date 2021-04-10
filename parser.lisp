(in-package #:gql)

(defun make-parser (source)
  (make-instance 'parser :lexer (make-lexer source)))

(defgeneric parse (parser node-type &key &allow-other-keys)
  (:documentation "Parse node of NODE-TYPE with parser PARSER."))

(defmethod parse :before ((parser parser) node-type &key &allow-other-keys)
  (when *debug-print*
    (with-token parser
      (with-slots (value kind) token
        (format t "; value: ~Vakind: ~Vanode-type: ~Va~%" 10 value 10 kind 10 node-type)))))

(defmethod parse ((parser parser) (node-type (eql :document)) &key &allow-other-keys)
  "Document
     - Definition list

As described in: https://spec.graphql.org/June2018/#sec-Language.Document"
  (with-token parser
    (make-instance 'document
                   :kind 'document
                   :location (loc parser token)
                   :definitions (many parser 'sof :definition 'eof))))

(defmethod parse ((parser parser) (node-type (eql :definition)) &key &allow-other-keys)
  "Definition :
     - ExecutableDefinition
     - TypeSystemDefinition
     - TypeSystemExtension
   
   ExecutableDefinition :
     - OperationDefinition
     - FragmentDefinition

As described in: https://spec.graphql.org/June2018/#sec-Language.Document"
  (with-token parser
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
      ((peek parser 'string)  (parse parser :type-system-definitiona))
      (t (unexpected parser token)))))

(defmethod parse ((parser parser) (node-type (eql :operation-definition)) &key &allow-other-keys)
  (with-token parser
    (when (peek parser 'brace-l)
      ;; We allow for the query shorthand by first checking for the opening
      ;; brace.  If we arrive here we know that we don't have any DIRECTIVES,
      ;; VARIABLE-DEFINITIONS or NAME.  However, we do have the SELECTION-SET.
      ;; We early return to avoid parsing more than we need.
      (return-from parse
        (make-instance 'operation-definition
                       :directives nil
                       :variable-definitions nil
                       :name nil
                       :operation "query"
                       :selection-set (parse parser :selection-set)
                       :location (loc parser token)
                       :kind 'operation-definition)))
    ;; Parse the OPERATION-TYPE this so that we traverse over the node if we
    ;; don't error.
    (let ((operation (parse parser :operation-type))
          name)
      (when (peek parser 'name)
        (setf name (parse parser :name)))
      (make-instance 'operation-definition
                     :name name
                     :operation operation
                     :variable-definitions (parse parser :variable-definitions)
                     :directives (parse parser :directives)
                     :selection-set (parse parser :selection-set)
                     :location (loc parser token)
                     :kind 'operation-definition))))


(defmethod parse ((parser parser) (node-type (eql :operation-type)) &key &allow-other-keys)
  ;; Disallow other names than query, mutation and subscription.
  (let* ((operation-token (expect-token parser 'name))
         (value (value operation-token)))
    (cond
      ((string= value "query") "query")
      ((string= value "mutation") "mutation")
      ((string= value "subscription") "subscription")
      (t (unexpected parser operation-token)))))

(defmethod parse ((parser parser) (node-type (eql :fragment-definition)) &key &allow-other-keys)
  (with-token parser
    (expect-keyword parser "fragment")
    (make-instance 'fragment-definition
                   :name (parse parser :fragment-name)
                   :type-condition (progn
                                     (expect-keyword parser "on")
                                     (parse parser :named-type))
                   :directives (parse parser :directives)
                   :selection-set (parse parser :selection-set)
                   :location (loc parser token)
                   :kind 'fragment-definition)))

(defmethod parse ((parser parser) (node-type (eql :name)) &key &allow-other-keys)
  (with-expected-token parser 'name
    (make-instance 'name
                   :name (value token)
                   :location (loc parser token)
                   :kind 'name)))

(defmethod parse ((parser parser) (node-type (eql :selection-set)) &key &allow-other-keys)
  "Selection-set : { selection+ }"
  (with-token parser
    (make-instance 'selection-set
                   :selections (many parser 'brace-l :selection 'brace-r)
                   :location (loc parser token)
                   :kind 'selection-set)))

(defmethod parse ((parser parser) (node-type (eql :selection)) &key &allow-other-keys)
  "Selection :
     - Field
     - FragmentSpread
     - InlineFragment"
  (if (peek parser 'spread)
      (parse parser :fragment)
      (parse parser :field)))

(defmethod parse ((parser parser) (node-type (eql :field)) &key &allow-other-keys)
  "Field : Alias? Name Arguments? Directives SelectionSet"
  (with-token parser
    (let ((name-or-alias (parse parser :name)) alias name)
      (if (expect-optional-token parser 'colon)
          (setf alias name-or-alias name (parse parser :name))
          (setf name name-or-alias))
      (make-instance 'field
                     :alias alias
                     :name name
                     :arguments (parse parser :arguments)
                     :directives (parse parser :directives)
                     :selection-set (when (peek parser 'brace-l) (parse parser :selection-set))
                     :location (loc parser token)
                     :kind 'field))))

(defmethod parse ((parser parser) (node-type (eql :fragment)) &key &allow-other-keys)
  (with-token parser
    (expect-token parser 'spread)
    (let ((type-condition-p (expect-optional-keyword parser "on")))
      (if (and (not type-condition-p) (peek parser 'name))
          (make-instance 'fragment-spread
                         :name (parse parser :fragment-name)
                         :directives (parse parser :directives)
                         :location (loc parser token)
                         :kind 'fragment-spread)
          (make-instance 'inline-fragment
                         :type-condition (when type-condition-p (parse parser :named-type))
                         :directives (parse parser :directives :constp nil)
                         :selection-set (parse parser :selection-set)
                         :location (loc parser token)
                         :kind 'inline-fragment)))))

(defmethod parse ((parser parser) (node-type (eql :fragment-name)) &key &allow-other-keys)
  "Fragment-name : Name but not `on`"
  (with-token parser
    (if (string= (value token) "on")
        (unexpected parser token)
        (parse parser :name))))

(defmethod parse ((parser parser) (node-type (eql :arguments)) &key (constp nil) &allow-other-keys)
  (let ((item (if constp :const-argument :argument)))
    (optional-many parser 'paren-l item 'paren-r)))

(defmethod parse ((parser parser) (node-type (eql :argument)) &key &allow-other-keys)
  (with-token parser
    (let ((name (parse parser :name)))
      (expect-token parser 'colon)
      (make-instance 'argument
                     :name name
                     :value (parse parser :value)
                     :location (loc parser token)
                     :kind 'argument))))

(defmethod parse ((parser parser) (node-type (eql :const-argument)) &key &allow-other-keys)
  (with-token parser
    (make-instance 'argument
                   :name (parse parser :name)
                   :value (progn
                            (expect-token parser 'colon)
                            (parse parser :value))
                   :location (loc parser token)
                   :kind 'argument)))

(defmethod parse ((parser parser) (node-type (eql :variable-definitions)) &key &allow-other-keys)
  (optional-many parser 'paren-l :variable-definition 'paren-r))

(defmethod parse ((parser parser) (node-type (eql :variable-definition)) &key &allow-other-keys)
  (with-token parser
    (make-instance 'variable-definition
                   :var (parse parser :var)
                   :var-type (progn
                               (expect-token parser 'colon)
                               (parse parser :type-reference))
                   :default-value nil
                   :directives (parse parser :directives :constp t)
                   :location (loc parser token)
                   :kind 'variable-definition)))

(defmethod parse ((parser parser) (node-type (eql :var)) &key &allow-other-keys)
  (with-token parser
    (expect-token parser 'dollar)
    (make-instance 'var
                   :name (parse parser :name)
                   :location (loc parser token)
                   :kind 'var)))

(defmethod parse ((parser parser) (node-type (eql :string-value)) &key &allow-other-keys)
  (with-token parser
    (advance (lexer parser))
    (make-instance 'string-value
                   :value (value token)
                   :blockp (when (eq (kind token) 'block-string))
                   :location (loc parser token)
                   :kind 'var)))

(defmethod parse ((parser parser) (node-type (eql :value)) &key (constp nil) &allow-other-keys)
  "values"
  (with-token parser
    (case (kind token)
      (bracket-l (parse parser :list-value :constp constp))
      (brace-l (parse parser :object-value :consp constp))
      (int (progn
             (advance (lexer parser))
             (make-instance 'int-value
                            :int-value (value token)
                            :location (loc parser token)
                            :kind 'int-value)))
      (float (progn
               (advance (lexer parser))
               (make-instance 'float-value
                              :int-value (value token)
                              :location (loc parser token)
                              :kind 'float-value)))
      ((or string block-string) (parse parser :string-value))
      (name (progn
              (advance (lexer parser))
              (let ((value (value token)))
                (cond
                  ((string= value "true")
                   (make-instance 'boolean-value
                                  :value t
                                  :location (loc parser token)
                                  :kind 'boolean-value))
                  ((string= value "false")
                   (make-instance 'boolean-value
                                  :value nil
                                  :location (loc parser token)
                                  :kind 'boolean-value))
                  ((string= value "null")
                   (make-instance 'null-value
                                  :location (loc parser token)
                                  :kind 'null-value))
                  (t
                   (make-instance 'enum-value
                                  :value value
                                  :location (loc parser token)
                                  :kind 'enum-value))))))
      (dollar (unless constp (parse parser :var)))
      (t (unexpected parser token)))))

(defmethod parse ((parser parser) (node-type (eql :list-value)) &key (constp nil) &allow-other-keys)
  (with-token parser
    (make-instance 'list-value
                   :list-values (any parser 'bracket-l :value 'bracket-r constp)
                   :location (loc parser token)
                   :kind 'list-value)))

(defmethod parse ((parser parser) (node-type (eql :object-value)) &key (constp nil) &allow-other-keys)
  (with-token parser
    (make-instance 'object-value
                   :fields (any parser 'brace-l :object-field 'brace-r constp)
                   :location (loc parser token)
                   :kind 'object-value)))

(defmethod parse ((parser parser) (node-type (eql :object-value)) &key (constp nil) &allow-other-keys)
  (with-token parser
    (let ((name (parse parser :name)))
      (expect-token parser 'colon)
      (make-instance 'object-field
                     :name name
                     :value (parse parser :value :constp constp)
                     :location (loc parser token)
                     :kind 'object-value))))

(defmethod parse ((parser parser) (node-type (eql :directives)) &key (constp nil) &allow-other-keys)
  "Directives[Const] : Directive[?Const]+"
  (loop
    with directives
    while (peek parser 'at)
    do (push (parse parser :directive :constp constp) directives)
    finally (return (nreverse directives))))

(defmethod parse ((parser parser) (node-type (eql :directive)) &key (constp nil) &allow-other-keys)
  "Directive[Const] : @ Name Arguments[?Const]?"
  (with-token parser
    (expect-token parser 'at)
    (make-instance 'directive
                   :name (parse parser :name)
                   :arguments (parse parser :arguments :constp constp)
                   :location (loc parser token)
                   :kind 'directive)))

(defmethod parse ((parser parser) (node-type (eql :named-type)) &key &allow-other-keys)
  "Directive[Const] : @ Name Arguments[?Const]?"
  (with-token parser
    (make-instance 'named-type
                   :name (parse parser :name)
                   :location (loc parser token)
                   :kind 'named-type)))

(defmethod parse ((parser parser) (node-type (eql :type-reference)) &key &allow-other-keys)
  "Directive[Const] : @ Name Arguments[?Const]?"
  (with-token parser
    (let ((ty))
      (if (expect-optional-token parser 'bracket-l)
          (let ((inner-type (parse parser :type-reference)))
            (expect-token parser 'bracket-r)
            (setf ty (make-instance 'list-type
                                    :ty inner-type
                                    :location (loc parser token)
                                    :kind 'list-type)))
          (setf ty (parse parser :named-type)))
      (when (expect-optional-token parser 'bang)
        (return-from parse
          (make-instance 'non-null-type
                         :ty ty
                         :location (loc parser token)
                         :kind 'non-null-type)))
      ty)))

