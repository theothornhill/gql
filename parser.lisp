(in-package #:gql)

(defun make-parser (source)
  (make-instance 'parser :lexer (make-lexer source)))

(defgeneric parse (parser node-type)
  (:documentation "Parse node of NODE-TYPE with parser PARSER."))

(defmethod parse ((parser parser) (node-type (eql :document)))
  (with-slots (lexer) parser
    (let ((start (token lexer)))
      (make-instance 'document
                     :kind 'document
                     :location (loc parser start)
                     :definitions (many parser 'sof :definition 'eof)))))

(defmethod parse ((parser parser) (node-type (eql :definition)))
  ;; TODO
  (advance (lexer parser)))
