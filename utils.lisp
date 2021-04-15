(in-package :gql)

(defmacro string-case (value &rest things)
  (let ((v (gensym)))
    `(let ((,v ,value))
       (cond
         ,@(mapcar (lambda (thing)
                     `((string= ,v ,(first thing)) ,(second thing)))
                   (butlast things))
         ,(first (reverse things))))))

(defmacro with-token (&body body)
  "Bring the current token into scope anaphorically.

Refer to the current token with TOKEN.  Also works under the assumption that it
is called primarily through the PARSE DEFMETHOD, so that we actually have the PARSER."
  `(let ((*token* (token (lexer *parser*))))
     ,@body))

(defmacro with-expected-token (kind &body body)
  "Bring the expected token into scope anaphorically.

Refer to the token as TOKEN.  This macro also advances lexer one step as a side
effect.  Also works under the assumption that it is called primarily through the
PARSE DEFMETHOD, so that we actually have the PARSER."
  `(let ((token (expect-token ,kind)))
     ,@body))

(defun advance-one-token ()
  (when *parser*
    (advance (lexer *parser*))))

(defun expect-then-parse (things node-type &optional (constp nil))
  (let ((normalized-things (if (listp things) things (list things))))
    (dolist (thing normalized-things)
      (if (symbolp thing)
          (expect-token thing)
          (expect-keyword thing))))
  (parse node-type constp))

(defun advance-then-value ()
  (advance-one-token)
  (value *token*))

(defmacro make-node (node-type &body keys)
  "Make an instance of NODE-TYPE, with provided initargs in KEYS.

Assumes a PARSER and TOKEN already is in scope using WITH-TOKEN or
WITH-EXPECTED-TOKEN.  Convenience macro to avoid providing KIND and LOCATION for
all nodes."
  (let ((n-type (gensym)))
    `(let ((,n-type ,node-type))
       (make-instance ,n-type ,@keys :kind ,n-type :location (loc *parser* *token*)))))

(defun loc (parser start-token)
  (with-slots (last-token) (lexer parser)
    (make-instance 'location
                   :start (start start-token)
                   :end (end last-token)
                   :start-token start-token
                   :end-token last-token
                   :source (source (lexer parser)))))

(defun peek (kind)
  (eq (kind (token (lexer *parser*))) kind))

(defun expect-token (kind)
  (with-token
    (if (peek kind)
        (progn (advance-one-token) *token*)
        (gql-error "Expected ~a, found ~a" kind (kind *token*)))))

(defun expect-optional-token (kind)
  (with-token
    (when (peek kind)
      (advance (lexer *parser*)) *token*)))

(defun expect-keyword (value)
  (with-token
    (if (and (peek 'name) (equalp (value *token*) value))
        (advance-one-token)
        (gql-error "Expected ~a, found ~a" value (value *token*)))))

(defun expect-optional-keyword (value)
  (with-token
    (when (and (peek 'name) (equalp (value *token*) value))
      (advance-one-token)
      *token*)))

(defun unexpected (&optional token)
  (let ((token (if token token *token*)))
    (gql-error "Unexpected token: ~a" token)))

(defun any (open-kind parse-kind close-kind &optional (constp nil))
  (expect-token open-kind)
  (loop
    with nodes = nil
    until (expect-optional-token close-kind)
    do (push (parse parse-kind constp) nodes)
    finally (return (nreverse nodes))))

(defun optional-many (open-kind parse-kind close-kind)
  (when (expect-optional-token open-kind)
    (loop
      with nodes
        initially (push (parse parse-kind) nodes)
      until (expect-optional-token close-kind)
      do (push (parse parse-kind) nodes)
      finally (return (nreverse nodes)))))

(defun many (open-kind parse-kind close-kind)
  (expect-token open-kind)
  (loop
    with nodes
      initially (push (parse parse-kind) nodes)
    until (expect-optional-token close-kind)
    do (push (parse parse-kind) nodes)
    finally (return (nreverse nodes))))

(defun delimited-many (delimiter-kind parse-kind)
  (expect-optional-token delimiter-kind)
  (loop
    with nodes
      initially (push (parse parse-kind) nodes)
    while (expect-optional-token delimiter-kind)
    do (push (parse parse-kind) nodes)
    finally (return (nreverse nodes))))
