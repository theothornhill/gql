(in-package #:gql)

(defclass parser ()
  ((lexer
    :initarg :lexer
    :accessor lexer
    :documentation "The lexer that previously lexed an input file.")))

(defgeneric parse (parser node-type)
  (:documentation "Parse node of NODE-tYPE with parser PARSER."))
