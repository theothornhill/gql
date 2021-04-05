(in-package #:gql)

;;; Utilities

(defgeneric loc (parser token)
  (:documentation "Returns a location object, used to identify the place in the source that
created a given parsed object."))

(defgeneric peek (parser kind)
  (:documentation "Determines if the next token is of a given KIND."))

(defgeneric expect-token (parser kind)
  (:documentation "If the next token is of the given kind, return that token after advancing the lexer.
Otherwise, do not change the parser state and throw an error."))

(defgeneric expect-optional-token (parser kind)
  (:documentation "If the next token is of the given kind, return that token after advancing the lexer.
Otherwise, do not change the parser state and return nil."))

(defgeneric expect-keyword (parser value)
  (:documentation "If the next token is of the given kind, return that token after advancing the lexer.
Otherwise, do not change the parser state and throw an error."))

(defgeneric expect-optional-keyword (parser value)
  (:documentation "If the next token is of the given kind, return that token after advancing the lexer.
Otherwise, do not change the parser state and return nil."))

(defgeneric unexpected (parser token)
  (:documentation "Helper function for creating an error when an unexpected lexed token is encountered."))

(defgeneric any (parser open-kind parse-fn close-kind)
  (:documentation "Returns a possibly empty list of parse nodes, determined by the PARSE-FN.

This list begins with a lex token of OPEN-KIND and ends with a lex token of
CLOSE-KIND.  Advances the parser to the next lex token after the closing token."))

(defgeneric optional-many (parser open-kind parse-fn close-kind)
  (:documentation "Returns a list of parse nodes, determined by the PARSE-FN.

It can be empty only if open token is missing.  Otherwise it will always return
a non-empty list that begins with a lex token of OPEN-KIND and ends with a lex
token of CLOSE-KIND.  Advances the parser to the next lex token after the
closing token."))

(defgeneric many (parser open-kind parse-fn close-kind)
  (:documentation "Returns a non-empty list of parse nodes, determined by the parseFn.

This list begins with a lex token of OPEN-KIND and ends with a lex token of
CLOSE-KIND.  Advances the parser to the next lex token after last item in the
list."))

(defgeneric delimited-many (parser delimiter-kind parse-fn)
  (:documentation "Returns a possibly empty list of parse nodes, determined by the PARSE-FN

This list may begin with a lex token of DELIMITER-KIND followed by items
separated by lex tokens of TOKEN-KIND.  Advances the parser to the next lex
token after last item in the list."))

(defgeneric parse (parser node-type)
  (:documentation "Parse node of NODE-tYPE with parser PARSER."))

(defmethod loc ((parser parser) (start-token token))
  (with-slots (lexer) parser
    (with-slots (last-token) lexer
      (make-instance 'location
                     :start (start start-token)
                     :end (end last-token)
                     :start-token start-token
                     :end-token last-token
                     :source (source lexer)))))

(defmethod peek ((parser parser) kind)
  (with-slots (lexer) parser
    (with-slots (token) lexer
      (with-lexer-token
          (eq (kind token) kind)))))

(defmethod expect-token ((parser parser) kind)
  (with-slots (lexer) parser
    (with-slots (token) lexer
      (if (eq (kind token) kind)
          (progn (advance lexer) token)
          (gql-error "Expected ~a, found ~a" kind (kind token))))))

(defmethod expect-optional-token ((parser parser) kind)
  (with-slots (lexer) parser
    (with-slots (token) lexer
      (when (eq (kind token) kind)
        (advance lexer) token))))

(defmethod expect-keyword ((parser parser) (value string))
  (with-slots (lexer) parser
    (with-slots (token) lexer
      (if (and (eq (kind token) 'name) (equalp (value token) value))
          (advance lexer)
          (gql-error "Expected ~a, found ~a" value (value token))))))

(defmethod expect-optional-keyword ((parser parser) (value string))
  (with-slots (lexer) parser
    (with-slots (token) lexer
      (when (and (eq (kind token) 'name) (equalp (value token) value))
        (advance lexer) token))))

(defmethod unexpected ((parser parser) token)
  (let ((token (if token token (token (lexer parser)))))
    (gql-error "Unexpected token: ~a" token)))

(defmethod any ((parser parser) open-kind parse-fn close-kind)
  (expect-token parser open-kind)
  (loop
    with nodes = nil
    until (expect-optional-token parser close-kind)
    do (push (funcall parse-fn) nodes)
    finally (return (nreverse nodes))))

(defmethod optional-many ((parser parser) open-kind parse-fn close-kind)
  (when (expect-optional-token open-kind)
    (loop
      with nodes = nil
      do (push (funcall parse-fn) nodes)
         (when (expect-optional-token parser close-kind)
           (return-from optional-many (nreverse nodes))))))

(defmethod many ((parser parser) open-kind parse-fn close-kind)
  (expect-token parser open-kind)
  (loop
    with nodes = nil
    do (push (funcall parse-fn) nodes)
       (when (expect-optional-token parser close-kind)
         (return-from many (nreverse nodes)))))

(defmethod delimited-many ((parser parser) delimiter-kind parse-fn)
  (expect-token parser delimiter-kind)
  (loop
    with nodes = nil
    do (push (funcall parse-fn) nodes)
       (unless (expect-optional-token parser delimiter-kind)
         (return-from delimited-many (nreverse nodes)))))
