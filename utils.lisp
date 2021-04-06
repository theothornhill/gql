(in-package :gql)

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

(defgeneric any (parser open-kind parse-kind close-kind)
  (:documentation "Returns a possibly empty list of parse nodes, determined by the PARSE-KIND.

This list begins with a lex token of OPEN-KIND and ends with a lex token of
CLOSE-KIND.  Advances the parser to the next lex token after the closing token."))

(defgeneric optional-many (parser open-kind parse-kind close-kind)
  (:documentation "Returns a list of parse nodes, determined by the PARSE-KIND.

It can be empty only if open token is missing.  Otherwise it will always return
a non-empty list that begins with a lex token of OPEN-KIND and ends with a lex
token of CLOSE-KIND.  Advances the parser to the next lex token after the
closing token."))

(defgeneric many (parser open-kind parse-kind close-kind)
  (:documentation "Returns a non-empty list of parse nodes, determined by the PARSE-KIND

This list begins with a lex token of OPEN-KIND and ends with a lex token of
CLOSE-KIND.  Advances the parser to the next lex token after last item in the
list."))

(defgeneric delimited-many (parser delimiter-kind parse-kind)
  (:documentation "Returns a possibly empty list of parse nodes, determined by the PARSE-KIND

This list may begin with a lex token of DELIMITER-KIND followed by items
separated by lex tokens of TOKEN-KIND.  Advances the parser to the next lex
token after last item in the list."))


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
      (eq (kind token) kind))))

(defmethod expect-token ((parser parser) kind)
  (let ((token (token (lexer parser))))
    (if (eq (kind token) kind)
        (progn (advance (lexer parser)) token)
        (gql-error "Expected ~a, found ~a" kind (kind token)))))

(defmethod expect-optional-token ((parser parser) kind)
  (let ((token (token (lexer parser))))
    (when (eq (kind token) kind)
      (advance (lexer parser)) token)))

(defmethod expect-keyword ((parser parser) (value string))
  (let ((token (token (lexer parser))))
    (if (and (eq (kind token) 'name) (equalp (value token) value))
        (advance (lexer parser))
        (gql-error "Expected ~a, found ~a" value (value token)))))

(defmethod expect-optional-keyword ((parser parser) (value string))
  (let ((token (token (lexer parser))))
    (when (and (eq (kind token) 'name) (equalp (value token) value))
      (advance (lexer parser)) token)))

(defmethod unexpected ((parser parser) token)
  (let ((token (if token token (token (lexer parser)))))
    (gql-error "Unexpected token: ~a" token)))

(defmethod any ((parser parser) open-kind parse-kind close-kind)
  (expect-token parser open-kind)
  (loop
    with nodes = nil
    until (expect-optional-token parser close-kind)
    do (push (parse parser parse-kind) nodes)
    finally (return (nreverse nodes))))

(defmethod optional-many ((parser parser) open-kind parse-kind close-kind)
  (when (expect-optional-token open-kind)
    (loop
      with nodes
        initially (push (parse parser parse-kind) nodes)
      until (expect-optional-token parser close-kind)
      finally (return (nreverse nodes)))))

(defmethod many ((parser parser) open-kind parse-kind close-kind)
  (expect-token parser open-kind)
  (loop
    with nodes
      initially (push (parse parser parse-kind) nodes)
    until (expect-optional-token parser close-kind)
    do (push (parse parser parse-kind) nodes)
    finally (return (nreverse nodes))))

(defmethod delimited-many ((parser parser) delimiter-kind parse-kind)
  (expect-token parser delimiter-kind)
  (loop
    with nodes
      initially (push (parse parser parse-kind) nodes)
    while (expect-optional-token parser delimiter-kind)
    do (push (parse parser parse-kind) nodes)
    finally (return (nreverse nodes))))
