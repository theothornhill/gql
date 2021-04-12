(in-package :gql)

;;; Debugging

(defvar *debug-print* nil
  "Set to t if you want to debug the call stack for the parse tree.")

;;; Utilities

(defmacro with-token (&body body)
  "Bring the current token into scope anaphorically.

Refer to the current token with TOKEN.  Also works under the assumption that it
is called primarily through the PARSE DEFMETHOD, so that we actually have the PARSER"
  `(let ((token (token (lexer parser))))
     ,@body))

(defmacro with-expected-token (kind &body body)
  "Bring the expected token into scope anaphorically.

Refer to the token as TOKEN.  This macro also advances lexer one step as a side
effect.  Also works under the assumption that it is called primarily through the
PARSE DEFMETHOD, so that we actually have the PARSER."
  `(let ((token (expect-token parser ,kind)))
     ,@body))

(defmacro make-node (node-type &rest keys)
  "Make an instance of NODE-TYPE, with provided initargs in KEYS.

Assumes a PARSER and TOKEN already is in scope using WITH-TOKEN or
WITH-EXPECTED-TOKEN.  Convenience macro to avoid providing KIND and LOCATION for
all nodes."
  (let ((n-type (gensym)))
    `(let ((,n-type ,node-type))
       (make-instance ,n-type ,@keys :kind ,n-type :location (loc parser token)))))

(defun loc (parser start-token)
  (with-slots (last-token) (lexer parser)
    (make-instance 'location
                   :start (start start-token)
                   :end (end last-token)
                   :start-token start-token
                   :end-token last-token
                   :source (source (lexer parser)))))

(defun peek (parser kind)
  (eq (kind (token (lexer parser))) kind))

(defun expect-token (parser kind)
  (with-token
    (if (peek parser kind)
        (progn (advance (lexer parser)) token)
        (gql-error "Expected ~a, found ~a" kind (kind token)))))

(defun expect-optional-token (parser kind)
  (with-token
    (when (peek parser kind)
      (advance (lexer parser)) token)))

(defun expect-keyword (parser value)
  (with-token
    (if (and (peek parser 'name) (equalp (value token) value))
        (advance (lexer parser))
        (gql-error "Expected ~a, found ~a" value (value token)))))

(defun expect-optional-keyword (parser value)
  (with-token
    (when (and (peek parser 'name) (equalp (value token) value))
      (advance (lexer parser)) token)))

(defun unexpected (parser token)
  (let ((token (if token token (token (lexer parser)))))
    (gql-error "Unexpected token: ~a" token)))

(defun any (parser open-kind parse-kind close-kind &optional (constp nil))
  (expect-token parser open-kind)
  (loop
    with nodes = nil
    until (expect-optional-token parser close-kind)
    do (push (parse parser parse-kind constp) nodes)
    finally (return (nreverse nodes))))

(defun optional-many (parser open-kind parse-kind close-kind)
  (when (expect-optional-token parser open-kind)
    (loop
      with nodes
        initially (push (parse parser parse-kind) nodes)
      until (expect-optional-token parser close-kind)
      do (push (parse parser parse-kind) nodes)
      finally (return (nreverse nodes)))))

(defun many (parser open-kind parse-kind close-kind)
  (expect-token parser open-kind)
  (loop
    with nodes
      initially (push (parse parser parse-kind) nodes)
    until (expect-optional-token parser close-kind)
    do (push (parse parser parse-kind) nodes)
    finally (return (nreverse nodes))))

(defun delimited-many (parser delimiter-kind parse-kind)
  (expect-optional-token parser delimiter-kind)
  (loop
    with nodes
      initially (push (parse parser parse-kind) nodes)
    while (expect-optional-token parser delimiter-kind)
    do (push (parse parser parse-kind) nodes)
    finally (return (nreverse nodes))))
