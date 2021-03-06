(in-package :gql)

(defgeneric parse (node-type &key &allow-other-keys)
  (:method :before (node-type &key &allow-other-keys)
    (when *debug-print*
      (with-token
        (with-accessors ((value value) (kind kind)) *token*
          (format t "; value: ~Vakind: ~Vanode-type: ~Va~%" 10 value 10 kind 10 node-type)))))
  (:documentation "Parse node of NODE-TYPE with parser PARSER."))

(defgeneric generate (node &key &allow-other-keys)
  (:documentation "Print a node NODE as a valid GrqphQL statement.
The top level definitions should default to INDENT-LEVEL 0 and STREAM T.  The
other nodes should start with whatever indentation they are passed as well as
nil as STREAM, to keep them as a string when generating.  The toplevel
nodes (that default to STREAM T) could also of course be passed other streams,
i.e. for file streams etc."))

(defmacro defclass* (name &body slots)
  `(defclass ,name ()
     ,(loop :for slot :in slots
            :for percent-slot = (intern (format nil "%~a" (symbol-name slot)))
            :for initarg = (intern (symbol-name slot) :keyword)
            :collect `(,percent-slot :initarg ,initarg :initform nil :accessor ,slot))))

(defmacro defnode (name &body slots)
  `(defclass ,name (ast-node)
     ,(loop :for slot :in slots
            :for percent-slot = (intern (format nil "%~a" (symbol-name slot)))
            :for initarg = (intern (symbol-name slot) :keyword)
            :collect `(,percent-slot :initarg ,initarg :initform nil :accessor ,slot))))

(defmacro defparser (node keys &body body)
  `(defmethod parse ((node (eql ',node)) &key (constp nil) ,@keys &allow-other-keys)
     (declare (ignorable constp))
     (with-token
       (declare (ignorable *token*))
       ,@body)))

(defgeneric validate (node &key &allow-other-keys)
  (:documentation "TODO"))

(defmacro defgql (node-type &key
                              (node nil node?)
                              (parser nil parser?))
  (declare (ignorable node-type))
  (unless node?
    (gql-error "defgql requires a body for its node definition"))
  (unless parser?
    (gql-error "defgql requires a body for its parser definition"))
  `(progn
     ,node
     ,parser))

(defun add-indent (level)
  "Add indentation for pretty printing.

(add-indent 0) => \"\"
(add-indent 1) => \"  \"
(add-indent 2) => \"    \"
"
  (let ((indentation ""))
    (dotimes (_ level)
      (setf indentation (cat indentation "  ")))
    indentation))

(defun gather-nodes (node-list &optional indent-level stream)
  "Collect a list of formatted subnodes.
It indents where necessary (with help from calls to `generate'), then returns a
list of strings."
  (mapcar (lambda (node)
            (format
             nil "~a"
             (generate node :indent-level indent-level :stream stream)))
          node-list))

(defun cat (&rest args)
  (apply #'concatenate 'string args))

(defun char-code-at (body pos)
  (char-code (char body pos)))

(defmacro string-case (value &body things)
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
is called primarily through `parse', so that we actually have the PARSER."
  `(let ((*token* (token (lexer *parser*))))
     ,@body))

(defmacro with-expected-token (kind &body body)
  "Bring the expected token into scope anaphorically.

Refer to the token as TOKEN.  This macro also advances lexer one step as a side
effect.  Also works under the assumption that it is called primarily through
`parse', so that we actually have the PARSER."
  `(let ((*token* (expect-token ,kind)))
     ,@body))

(defun peek-description ()
  (or (peek 'string) (peek 'block-string)))

(defun advance-one-token ()
  (when *parser*
    (advance (lexer *parser*))))

(defun expect-then-parse (things node-type &key (constp nil))
  (let ((normalized-things (if (listp things) things (list things))))
    (dolist (thing normalized-things)
      (if (symbolp thing)
          (expect-token thing)
          (expect-keyword thing))))
  (parse node-type :constp constp))

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
  (with-accessors ((last-token last-token)) (lexer parser)
    (make-instance 'location
                   :start (start start-token)
                   :end (end last-token)
                   :start-token start-token
                   :end-token last-token
                   :source (source (lexer parser)))))

(defun peek (kind &optional token)
  (eq (kind (or token (token (lexer *parser*)))) kind))

(defun expect-token (kind)
  (with-token
    (if (peek kind)
        (progn (advance-one-token) *token*)
        (with-accessors ((line line) (column column)) *token*
          (gql-error "Expected ~a, found ~a at line: ~a, column: ~a"
                     kind (kind *token*) line column)))))

(defun expect-optional-token (kind)
  (with-token
    (when (peek kind)
      (advance (lexer *parser*)) *token*)))

(defun expect-keyword (value)
  (with-token
    (if (and (peek 'name) (equalp (value *token*) value))
        (advance-one-token)
        (with-accessors ((line line) (column column)) *token*
          (gql-error "Expected ~a, found ~a at line: ~a, column: ~a"
                     value (value *token*) line column)))))

(defun expect-optional-keyword (value)
  (with-token
    (when (and (peek 'name) (equalp (value *token*) value))
      (advance-one-token)
      *token*)))

(defun unexpected (&optional token)
  (let ((token (if token token *token*)))
    (with-accessors ((line line) (column column)) token
      (gql-error "Unexpected token: ~a at line: ~a, column: ~a" token line column))))

(defun any (open-kind parse-kind close-kind &key (constp nil))
  (expect-token open-kind)
  (loop
    :with nodes = nil
    :until (expect-optional-token close-kind)
    :do (push (parse parse-kind :constp constp) nodes)
    :finally (return (nreverse nodes))))

(defun optional-many (open-kind parse-kind close-kind)
  (when (expect-optional-token open-kind)
    (loop
      :with nodes
        :initially (push (parse parse-kind) nodes)
      :until (expect-optional-token close-kind)
      :do (push (parse parse-kind) nodes)
      :finally (return (nreverse nodes)))))

(defun many (open-kind parse-kind close-kind)
  (expect-token open-kind)
  (loop
    :with nodes
      :initially (push (parse parse-kind) nodes)
    :until (expect-optional-token close-kind)
    :do (push (parse parse-kind) nodes)
    :finally (return (nreverse nodes))))

(defun delimited-many (delimiter-kind parse-kind)
  (expect-optional-token delimiter-kind)
  (loop
    :with nodes
      :initially (push (parse parse-kind) nodes)
    :while (expect-optional-token delimiter-kind)
    :do (push (parse parse-kind) nodes)
    :finally (return (nreverse nodes))))
