(in-package #:cl-gql)

(defclass lexer ()
  ((source
    :initarg :source
    :accessor source
    :documentation "Source file to scan.")
   (last-token
    :initarg :last-token
    :accessor last-token
    :documentation "The previously focused non-ignored token.")
   (token
    :initarg :token
    :accessor token
    :documentation "The currently focused non-ignored token.")
   (line
    :initarg :line
    :accessor line
    :documentation "The (1-indexed) line containing the current token.")
   (line-start
    :initarg :line-start
    :accessor line-start
    :documentation "The character offset at which the current line begins.")))

(defun make-token (kind start end line column prev &optional value)
  (make-instance 'token :kind kind
                        :start start :end end
                        :line line :column column
                        :prev prev :value value))

(defun make-lexer (source)
  (let ((source (make-instance 'source
                               :body source
                               :name "name"
                               :location-offset 0))
        (start-of-file
          (make-token 'sof 0 0 0 0 nil nil)))
    (make-instance 'lexer
                   :source source
                   :token start-of-file
                   :last-token start-of-file
                   :line 1
                   :line-start 0)))

(defclass token ()
  ((kind
    :initarg :kind
    :accessor kind
    :documentation "The kind of token.")
   (start
    :initarg :start
    :accessor start
    :documentation "The character offset at which this Node begins.")
   (end
    :initarg :end
    :accessor end
    :documentation "The character offset at which this Node ends.")
   (line
    :initarg :line
    :accessor line
    :documentation "The 1-indexed line number on which this Token appears.")
   (column
    :initarg :column
    :accessor column
    :documentation "The 1-indexed column number at which this Token begins.")
   (value
    :initarg :value
    :accessor value
    :documentation "For non-punctuation tokens, represents the interpreted value of the token.")
   (prev
    :initarg :prev
    :accessor prev
    :documentation "The previous token.")
   (next
    :initarg :next
    :accessor next
    :documentation "The next token.")))

(defclass location ()
  ((start
    :initarg :start
    :accessor start
    :documentation "The character offset at which this Node begins.")
   (end
    :initarg :end
    :accessor end
    :documentation "The character offset at which this Node ends.")
   (start-token
    :initarg :start-token
    :accessor start
    :documentation "The Token at which this Node begins.")
   (end-token
    :initarg :end-token
    :accessor end
    :documentation "The Token at which this Node ends.")
   (source
    :initarg :source
    :accessor source
    :documentation "The Source document the AST represents.")))

(defclass source ()
  ((body
    :initarg :body
    :accessor body
    :documentation "TODO")
   (name
    :initarg :name
    :accessor name
    :documentation "TODO")
   (location-offset
    :initarg :location-offset
    :accessor location-offset
    :documentation "TODO")))

(deftype ast-node ()
  '(member
    :NameNode
    :DocumentNode
    :OperationDefinitionNode
    :VariableDefinitionNode
    :VariableNode
    :SelectionSetNode
    :FieldNode
    :ArgumentNode
    :FragmentSpreadNode
    :InlineFragmentNode
    :FragmentDefinitionNode
    :IntValueNode
    :FloatValueNode
    :StringValueNode
    :BooleanValueNode
    :NullValueNode
    :EnumValueNode
    :ListValueNode
    :ObjectValueNode
    :ObjectFieldNode
    :DirectiveNode
    :NamedTypeNode
    :ListTypeNode
    :NonNullTypeNode
    :SchemaDefinitionNode
    :OperationTypeDefinitionNode
    :ScalarTypeDefinitionNode
    :ObjectTypeDefinitionNode
    :FieldDefinitionNode
    :InputValueDefinitionNode
    :InterfaceTypeDefinitionNode
    :UnionTypeDefinitionNode
    :EnumTypeDefinitionNode
    :EnumValueDefinitionNode
    :InputObjectTypeDefinitionNode
    :DirectiveDefinitionNode
    :SchemaExtensionNode
    :ScalarTypeExtensionNode
    :ObjectTypeExtensionNode
    :InterfaceTypeExtensionNode
    :UnionTypeExtensionNode
    :EnumTypeExtensionNode
    :InputObjectTypeExtensionNode))

(defgeneric advance (lexer)
  (:documentation "Advance the token stream to the next non-ignored token."))
(defgeneric lookahead (lexer)
  (:documentation "Look ahead and return the next non-ignored token."))
(defgeneric read-token (lexer prev-token))

(defmethod advance ((lexer lexer))
  ;; Does not yet set next on prev
  (setf (last-token lexer) (token lexer))
  (setf (token lexer) (lookahead lexer)))

(defmethod lookahead ((lexer lexer))
  (with-slots (token) lexer
    (unless (eq (kind token) 'eof)
      (setf token (read-token lexer (last-token lexer))))))

(defun read-spread (body pos line col prev)
  (when (and (eq (char body (+ pos 1)) #\.)
             (eq (char body (+ pos 2)) #\.))
    (make-token 'spread pos (+ pos 3) line col prev)))

(defun read-string (body pos line col prev)
  (declare (ignore line col prev))
  (if (and (eq (char body (+ pos 1)) #\")
           (eq (char body (+ pos 2)) #\"))
      (error "Block string not handled yet")
      (error "String not handled yet")))

(defun read-number (source pos code line col prev)
  (declare (ignore source pos code line col prev))
  (error "Numbers not handled yet"))

(defun read-name (source start line col prev)
  (with-slots (body) source
    (let ((body-length (length body))
          (pos (1+ start))
          (code 0))
      (loop
        while (and (/= pos body-length)
                   (setf code (char-code (char body pos)))
                   (or ;; _
                    (= code 95)
                    ;; 0-9
                    (<= 48 code 57)
                    ;; A-Z
                    (<= 65 code 90)
                    ;; a-z
                    (<= 97 code 122)))
        do (incf pos))
      (make-token 'name start pos line col prev (subseq body start pos)))))

(defmethod read-token ((lexer lexer) (prev token))
  (with-slots (source) lexer
    (with-slots (body) source
      (let ((body-length (length body))
            (pos (end prev)))
        (loop until (>= pos body-length)
              do
                 (let ((code (char-code (char body pos)))
                       (line (line lexer))
                       (col (- (1+ pos) (line-start lexer))))
                   (print (code-char code))
                   (case code
                     (;; BOM, \t, space, ','
                      (#xfeff 9 32 44) (incf pos))
                     (10 (progn ;; Newline
                           (incf pos)
                           (incf (line lexer))
                           (setf (line-start lexer) pos)))
                     (13 (progn ;; \r - increment by two if \r\n
                           (incf pos (if (eq (char body (1+ pos)) #\Newline) 2 1))
                           (incf (line lexer))
                           (setf (line-start lexer) pos)))
                     (33 (return-from read-token ;; !
                           (make-token 'bang pos (1+ pos) line col prev)))
                     (35 (error "Comments are unhandled!"))
                     (36 (return-from read-token ;; $
                           (make-token 'dollar pos (1+ pos) line col prev)))
                     (38 (return-from read-token ;; &
                           (make-token 'amp pos (1+ pos) line col prev)))
                     (40 (return-from read-token ;; (
                           (make-token 'paren-l pos (1+ pos) line col prev)))
                     (41 (return-from read-token ;; )
                           (make-token 'paren-r pos (1+ pos) line col prev)))
                     (46 (return-from read-token ;; .
                           ;; TODO: Does this handle fall-through properly?
                           (read-spread body pos line col prev)))
                     (58 (return-from read-token ;; :
                           (make-token 'colon pos (1+ pos) line col prev)))
                     (61 (return-from read-token ;; =
                           (make-token 'equals pos (1+ pos) line col prev)))
                     (64 (return-from read-token ;; @
                           (make-token 'at pos (1+ pos) line col prev)))
                     (91 (return-from read-token ;; [
                           (make-token 'bracket-l pos (1+ pos) line col prev)))
                     (93 (return-from read-token ;; ]
                           (make-token 'bracket-r pos (1+ pos) line col prev)))
                     (123 (return-from read-token ;; {
                            (make-token 'brace-l pos (1+ pos) line col prev)))
                     (124 (return-from read-token ;; |
                            (make-token 'pipe pos (1+ pos) line col prev)))
                     (125 (return-from read-token ;; }
                            (make-token 'brace-r pos (1+ pos) line col prev)))
                     (34 (return-from read-token ;; "
                           (read-string body pos line col prev)))
                     (;; 0 1 2 3 4 5 6 7 8 9
                      (45 48 49 50 51 52 53 54 55 56 57)
                      (return-from read-token
                        (read-number source pos code line col prev)))
                     (;; A-Z_a-z
                      (65 ;; A
                       66
                       67
                       68
                       69
                       70
                       71
                       72
                       73
                       74
                       75
                       76
                       77
                       78
                       79
                       80
                       81
                       82
                       83
                       84
                       85
                       86
                       87
                       88
                       89
                       90 ;; Z
                       95 ;; _
                       97 ;; a
                       98
                       99
                       100
                       101
                       102
                       103
                       104
                       105
                       106
                       107
                       108
                       109
                       110
                       111
                       112
                       113
                       114
                       115
                       116
                       117
                       118
                       119
                       120
                       121
                       122)
                      (return-from read-token
                        (read-name source pos line col prev)))
                     (t (error "Unhandled syntax"))))
                 ;; Account for all parser cases here
              )
        ;; We have reached end of file - return it as a token
        (when (>= pos body-length)
          (let ((line (line lexer))
                (col (- (1+ pos) (line-start lexer))))
            (make-token 'eof body-length body-length line col prev)))))))
