(in-package #:gql)

(defun make-parser (source)
  (make-instance 'parser :lexer (make-lexer source)))

(defgeneric parse (parser node-type)
  (:documentation "Parse node of NODE-TYPE with parser PARSER."))

(defmethod parse ((parser parser) (node-type (eql :document)))
  "Document
     - Definition list

As described in: https://spec.graphql.org/June2018/#sec-Language.Document"
  (let ((start (token (lexer parser))))
    (make-instance 'document
                   :kind 'document
                   :location (loc parser start)
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
           ("query"        (parse parser :operation-definition))
           ("mutation"     (parse parser :operation-definition))
           ("subscription" (parse parser :operation-definition))

           ("fragment"     (parse parser :fragment-definition))

           ("schema"       (parse parser :type-system-definition))
           ("scalar"       (parse parser :type-system-definition))
           ("type"         (parse parser :type-system-definition))
           ("interface"    (parse parser :type-system-definition))
           ("union"        (parse parser :type-system-definition))
           ("enum"         (parse parser :type-system-definition))
           ("input"        (parse parser :type-system-definition))
           ("directive"    (parse parser :type-system-definition))

           ("extend"       (parse parser :type-system-extension))
           (t (unexpected parser token))))
        ((peek parser 'brace-l) (parse parser :operation-definition))
        ((peek parser 'string)  (parse parser :type-system-definitiona))
        (t (unexpected parser token))))))
