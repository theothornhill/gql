(in-package #:gql)

;;; Utils

(defun cat (&rest args)
  (apply #'concatenate 'string args))

(defun char-code-at (body pos)
  (char-code (char body pos)))

;;; Lexer

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
  (make-instance
   'token :kind kind :start start :end end :line line :column column :prev prev :value value))

(defun make-lexer (source)
  (let ((source (make-instance 'source :body source :name "name" :location-offset 0))
        (start-of-file (make-token 'sof 0 0 0 0 nil nil)))
    (make-instance
     'lexer :source source :token start-of-file :last-token start-of-file :line 1 :line-start 0)))

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
    :initform nil
    :accessor prev
    :documentation "The previous token.")
   (next
    :initarg :next
    :initform nil
    :accessor next
    :documentation "The next token.")))

(defclass source ()
  ((body
    :initarg :body
    :accessor body
    :documentation "The source file to lex and parse.")
   (name
    :initarg :name
    :accessor name
    :documentation "TODO")
   (location-offset
    :initarg :location-offset
    :accessor location-offset
    :documentation "TODO")))

(deftype punctuation ()
  '(member bang dollar paren-l paren-r spread colon
    equals at bracket-l bracket-r brace-l pipe brace-r))

(defgeneric advance (lexer)
  (:documentation "Advance the token stream to the next non-ignored token."))
(defgeneric lookahead (lexer)
  (:documentation "Look ahead and return the next non-ignored token."))
(defgeneric read-token (lexer prev-token))

(defmethod advance ((lexer lexer))
  (with-slots (token last-token) lexer
    (setf last-token token)
    (setf token (lookahead lexer))))

(defmethod lookahead ((lexer lexer))
  (loop
    with tok = (token lexer)
    until (eq (kind tok) 'eof)
    do (with-slots (next) tok
         (setf tok (if next next (setf next (read-token lexer tok))))
         (unless (eq (kind next) 'comment)
           (return-from lookahead tok)))))

(defun read-spread (body pos line col prev)
  (when (and (eq (char body (+ pos 1)) #\.)
             (eq (char body (+ pos 2)) #\.))
    (make-token 'spread pos (+ pos 3) line col prev)))

(defun read-comment (source start line col prev)
  (loop
    with body = (body source)
    with pos = start
    with code = (char-code-at body (incf pos))
    do (setf code (char-code-at body pos))
    while (and code (or (> code #x001F) (= code #x0009)))
    do (incf pos)
    finally (return (make-token 'comment start pos line col prev
                                (subseq body (1+ start) pos)))))

(defun char-to-hex (x)
  (cond
    ((<= 48 x 57)  (- x 48)) ;; 0-9
    ((<= 65 x 70)  (- x 55)) ;; A-F
    ((<= 97 x 102) (- x 87)) ;; a-f
    (t -1)))

(defun unicode-char (q w e r)
  (logior
   (ash (char-to-hex q) 12)
   (ash (char-to-hex w) 8)
   (ash (char-to-hex e) 4)
   (char-to-hex r)))

(defun read-string (source start line col prev)
  (loop
    with body = (body source)
    with pos = (1+ start)
    with chunk-start = pos
    with code = 0
    with value = ""
    while (and (< pos (length body))
               (setf code (char-code-at body pos))
               (/= code #x000A)
               (/= code #x000D))
    do
       (when (= code 34)
         ;; When on closing quote
         (setf value (cat value (subseq body chunk-start pos)))
         (return-from read-string
           (make-token 'string start (1+ pos) line col prev value)))

       (when (and (< code #x0020) (/= #x0009))
         (gql-error (format nil "Invalid character within string: ~a" (code-char code))))

       (incf pos)

       (when (= code 92)
         ;; \
         (setf value (cat value (subseq body chunk-start (1- pos))))
         (setf code (char-code-at body pos))
         (case code
           (34  (setf value (cat value (format nil "~c" #\"))))
           (47  (setf value (cat value "/")))
           (92  (setf value (cat value (format nil "~c" #\\))))
           (98  (setf value (cat value (format nil "~c" #\Backspace))))
           (102 (setf value (cat value (format nil "~c" #\Page))))
           (110 (setf value (cat value (format nil "~c" #\Newline))))
           (114 (setf value (cat value (format nil "~c" #\Return))))
           (116 (setf value (cat value (format nil "~c" #\Tab))))
           (117
            ;; Unicode character
            (let ((u-char
                    (unicode-char
                     (char-code-at body (+ pos 1))
                     (char-code-at body (+ pos 2))
                     (char-code-at body (+ pos 3))
                     (char-code-at body (+ pos 4)))))

              (when (< u-char 0)
                (gql-error (format nil "Invalid character escape sequence: ~a" u-char)))

              (setf value (cat value (format nil "~c" (code-char u-char))))
              (incf pos 4)))
           (t (gql-error (format nil "Invalid character escape sequence: ~a" code))))
         (incf pos)
         (setf chunk-start pos))))

(defun read-block-string (source start line col prev lexer)
  (loop
    with body = (body source)
    with pos = (+ start 3)
    with chunk-start = pos
    with code = 0
    with raw-value = ""
    while (and (< pos (length body))
               (setf code (char-code-at body pos)))
    do
       (when (and (= 34 code)
                  (= 34 (char-code-at body (+ pos 1)))
                  (= 34 (char-code-at body (+ pos 2))))
         (setf raw-value (cat raw-value (subseq body chunk-start pos)))
         (return-from read-block-string
           (make-token 'block-string start (+ pos 3) line col prev
                       ;; dedentblockstringvalue - https://github.com/graphql/graphql-js/blob/98feb57b9e0af59b3a0dfa5179565cb3acf4fa9e/src/language/blockString.js#L9
                       raw-value)))

       (when (and (< code #x0020) (/= code #x0009) (/= code #x000A) (/= code #x000D))
         (gql-error (format nil "Invalid character within String: ~a" (code-char code))))

       (cond
         ((= code 10)
          ;; newline
          (incf pos)
          (incf (line lexer))
          (setf (line-start lexer) pos))
         ((= code 13)
          ;; carriage return
          (incf pos (if (eq (char body (1+ pos)) #\Newline) 2 1))
          (incf (line lexer))
          (setf (line-start lexer) pos))
         ((and (= code 92)
               (= 34 (char-code-at body (+ pos 1)))
               (= 34 (char-code-at body (+ pos 2)))
               (= 34 (char-code-at body (+ pos 3))))
          ;; escape triple quote
          (setf raw-value (cat raw-value (subseq body chunk-start pos) "\"\"\""))
          (incf pos 4)
          (setf chunk-start pos))
         (t (incf pos))))
  (gql-error "Unterminated string"))

(defun read-digits (source start first-code)
  ;; TODO: some error handling?
  (loop
    with body = (body source)
    with pos = start
    with code = first-code
    while (<= 48 code 57)
    do (setf code (char-code-at body (incf pos)))
    finally (return pos)))

(defun read-number (source start first-code line col prev)
  (with-slots (body) source
    (let ((code first-code)
          (pos start)
          is-float)
      (when (= code 45) ;; -
        (setf code (char-code-at body (incf pos))))

      (if (= code 48) ;; 0
          (progn
            (setf code (char-code-at body (incf pos)))
            (when (<= 48 code 57)
              (gql-error (format nil "Invalid number, unexpected digit after 0: ~a" (code-char code)))))
          (progn
            ;; This position goes out of index if number meets end of file
            ;; https://todo.sr.ht/~theo/gql/1
            (setf pos (read-digits source pos code))
            (setf code (char-code-at body pos))))

      (when (= code 46) ;; .
        (setf is-float t)
        (setf code (char-code-at body (incf pos)))
        (setf pos (read-digits source pos code))
        (setf code (char-code-at body pos)))

      (when (or (= code 69) (= code 101)) ;; E e
        (setf is-float t)
        (setf code (char-code-at body (incf pos)))
        (when (or (= code 43) (= code 45)) ;; + -
          (setf code (char-code-at body (incf pos))))
        (setf pos (read-digits source pos code))
        (setf code (char-code-at body pos)))

      (when (or (= code 46)
                ;; _ A-Z a-z
                (or (= code 95)
                    (<= 65 code 90)
                    (<= 97 code 122)))
        (gql-error "Invalid number. Expected digit, got foo"))

      (make-token (if is-float 'float 'int) start pos line col prev
                  (subseq body start pos)))))

(defun read-name (source start line col prev)
  (loop
    with body = (body source)
    with pos = (1+ start)
    with code = 0
    while (and (/= pos (length body))
               (setf code (char-code-at body pos))
               (or ;; _
                (= code 95)
                ;; 0-9
                (<= 48 code 57)
                ;; A-Z
                (<= 65 code 90)
                ;; a-z
                (<= 97 code 122)))
    do (incf pos)
    finally (return-from read-name
              (make-token 'name start pos line col prev (subseq body start pos)))))

(defmethod read-token ((lexer lexer) (prev token))
  (loop
    with source = (source lexer)
    with body = (body source)
    with body-length = (length body)
    with pos = (end prev)
    until (>= pos body-length)
    do
       (let ((code (char-code-at body pos))
             (line (line lexer))
             (col (- (1+ pos) (line-start lexer))))
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
           (35 (return-from read-token
                 (read-comment source pos line col prev)))
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
           (34 (if (and (eq (char body (+ pos 1)) #\")
                        (eq (char body (+ pos 2)) #\"))
                   (return-from read-token ;; "
                     (read-block-string source pos line col prev lexer))
                   (return-from read-token ;; "
                     (read-string source pos line col prev))))
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
             90     ;; Z
             95     ;; _
             97     ;; a
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
           (t (gql-error (format nil "Unhandled syntax: ~a" (code-char code))))))
       ;; Account for all parser cases here
    finally 
       ;; We have reached end of file - return it as a token
       (when (>= pos body-length)
         (let ((line (line lexer))
               (col (- (1+ pos) (line-start lexer))))
           (return-from read-token
             (make-token 'eof body-length body-length line col prev))))))
