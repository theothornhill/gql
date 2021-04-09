(in-package #:gql)

(defun make-parser (source)
  (make-instance 'parser :lexer (make-lexer source)))

(defgeneric parse (parser node-type)
  (:documentation "Parse node of NODE-TYPE with parser PARSER."))

(defmethod parse :before ((parser parser) node-type)
  (when *debug-print*
    (with-token parser
      (with-slots (value kind) token
        (format t "value: ~Vakind: ~Vanode-type: ~Va~%" 10 value 10 kind 10 node-type)))))

(defmethod parse ((parser parser) (node-type (eql :document)))
  "Document
     - Definition list

As described in: https://spec.graphql.org/June2018/#sec-Language.Document"
  (with-token parser
    (make-instance 'document
                   :kind 'document
                   :location (loc parser token)
                   :definitions (many parser 'sof :definition 'eof))))

(defmethod parse ((parser parser) (node-type (eql :definition)))
  "Definition :
     - ExecutableDefinition
     - TypeSystemDefinition
     - TypeSystemExtension
   
   ExecutableDefinition :
     - OperationDefinition
     - FragmentDefinition

As described in: https://spec.graphql.org/June2018/#sec-Language.Document"
  (with-slots (lexer) parser
    (with-slots (token) lexer
      (cond 
        ((peek parser 'name)
         (a:switch ((value token) :test #'string=)
           ;; ExecutableDefinition
           ("query"        (parse parser :operation-definition))
           ("mutation"     (parse parser :operation-definition))
           ("subscription" (parse parser :operation-definition))

           ("fragment"     (parse parser :fragment-definition))

           ;; TypeSystemDefinition
           ("schema"       (parse parser :type-system-definition))
           ("scalar"       (parse parser :type-system-definition))
           ("type"         (parse parser :type-system-definition))
           ("interface"    (parse parser :type-system-definition))
           ("union"        (parse parser :type-system-definition))
           ("enum"         (parse parser :type-system-definition))
           ("input"        (parse parser :type-system-definition))
           ("directive"    (parse parser :type-system-definition))

           ;; TypeSystemExtension
           ("extend"       (parse parser :type-system-extension))
           (t (unexpected parser token))))
        ((peek parser 'brace-l) (parse parser :operation-definition))
        ((peek parser 'string)  (parse parser :type-system-definitiona))
        (t (unexpected parser token))))))

(defmethod parse ((parser parser) (node-type (eql :operation-definition)))
  (with-token parser
    (when (peek parser 'brace-l)
      ;; We allow for the query shorthand by first checking for the opening
      ;; brace.  If we arrive here we know that we don't have any DIRECTIVES,
      ;; VARIABLE-DEFINITIONS or NAME.  However, we do have the SELECTION-SET.
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
                     :directives nil
                     :variable-definitions nil
                     :name name
                     :operation operation
                     :selection-set (parse parser :selection-set)
                     :location (loc parser token)
                     :kind 'operation-definition))))


(defmethod parse ((parser parser) (node-type (eql :operation-type)))
  (let* ((operation-token (expect-token parser 'name))
         (value (value operation-token)))
    (cond
      ((string= value "query") "query")
      ((string= value "mutation") "mutation")
      ((string= value "subscription") "subscription")
      (t (unexpected parser operation-token)))))

(defmethod parse ((parser parser) (node-type (eql :name)))
  (with-expected-token parser 'name
    (make-instance 'name
                   :name (value token)
                   :location (loc parser token)
                   :kind 'name)))

(defmethod parse ((parser parser) (node-type (eql :selection-set)))
  "Selection-set : { selection+ }"
  (with-token parser
    (make-instance 'selection-set
                   :selections (many parser 'brace-l :selection 'brace-r)
                   :location (loc parser token)
                   :kind 'selection-set)))

(defmethod parse ((parser parser) (node-type (eql :selection)))
  "Selection :
     - Field
     - FragmentSpread
     - InlineFragment"
  (if (peek parser 'spread)
      (parse parser :spread)
      (parse parser :field)))

(defmethod parse ((parser parser) (node-type (eql :field)))
  (with-token parser
    (let ((name-or-alias (parse parser :name)) alias name)
      (if (expect-optional-token parser 'colon)
          (setf alias name-or-alias name (parse parser :name))
          (setf name name-or-alias))
      (make-instance 'field
                     :alias alias
                     :name name
                     :arguments nil
                     :directives nil
                     :selection-set (when (peek parser 'brace-l) (parse parser :selection-set))
                     :location (loc parser token)
                     :kind 'field))))

(defmethod parse ((parser parser) (node-type (eql :spread))))

(defmethod parse ((parser parser) (node-type (eql :fragment-spread))))

(defmethod parse ((parser parser) (node-type (eql :inline-fragment))))
