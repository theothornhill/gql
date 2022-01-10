(in-package #:gql)

(defclass* lexer
  source
  last-token
  token
  line
  line-start)

(defun make-lexer (source)
  "Convenience function to make a GraphQL lexer.
SOURCE is a string representation of a GraphQL document."
  (let ((start-of-file (make-token 'sof 0 0 0 0 nil nil)))
    (make-instance 'lexer
     :source (make-instance 'source :body source :name (gensym) :location-offset 0)
     :token start-of-file
     :last-token start-of-file
     :line 1
     :line-start 0)))

(defclass* token
  kind
  start
  end
  line
  column
  value
  prev
  next)

(defun make-token (kind start end line column prev &optional value)
  "Convenience function to make a GraphQL token.
KIND is a subclass of TOKEN, and encompasses syntactic constructs in GraphQL.

START and END provides the position in the source file, respectively.

LINE signifies what line this token starts on.  It is 1-based.

COLUMN signifies what column this token starts on. It is 0-based.

PREV is the token preceeding this.

VALUE is the string representation of the token.  It is mostly used for
literals."
  (make-instance 'token
   :kind kind
   :start start
   :end end
   :line line
   :column column
   :prev prev
   :value value))

(defclass* source
  body
  name
  location-offset)

;;; Lexer api

(defun advance (lexer)
  (with-accessors ((token token) (last-token last-token)) lexer
    (setf last-token token)
    (setf token (lookahead lexer))))

(defun lookahead (lexer)
  (loop
    :with token = (token lexer)
    :until (eq (kind token) 'eof)
    :do (with-accessors ((next next)) token
          (setf token (if next next (setf next (read-token lexer token))))
          (unless (eq (kind next) 'comment)
            (return token)))))

;;; Tokenizers

(defun read-spread (body pos line col prev)
  (when (and (eq (char body (+ pos 1)) #\.)
             (eq (char body (+ pos 2)) #\.))
    (make-token 'spread pos (+ pos 3) line col prev)))

(defun read-comment (source start line col prev)
  (loop
    :with body = (body source)
    :with pos = start
    :with code = (char-code-at body (incf pos))
    :do (setf code (char-code-at body pos))
    :while (and code (or (> code #x001F) (= code #x0009)))
    :do (incf pos)
    :finally (return (make-token 'comment start pos line col prev
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
    :with body = (body source)
    :with pos = (1+ start)
    :with chunk-start = pos
    :with code = 0
    :with value = ""
    :while (and (< pos (length body))
                (setf code (char-code-at body pos))
                (/= code #x000A)
                (/= code #x000D))
    :do
       (when (= code 34)
         ;; When on closing quote
         (setf value (cat value (subseq body chunk-start pos)))
         (return
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
    :with body = (body source)
    :with pos = (+ start 3)
    :with chunk-start = pos
    :with code = 0
    :with raw-value = ""
    :while (and (< pos (length body))
                (setf code (char-code-at body pos)))
    :do
       (when (and (= 34 code)
                  (= 34 (char-code-at body (+ pos 1)))
                  (= 34 (char-code-at body (+ pos 2))))
         (setf raw-value (cat raw-value (subseq body chunk-start pos)))
         (return-from read-block-string ;; We want to exit this function
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
  (loop
    :with body = (body source)
    :with pos = start
    :with code = first-code
    :while (<= 48 code 57)
    :do (if (> (1- (length body)) pos)
            (setf code (char-code-at body (incf pos)))
            (gql-error "Unexpected EOF"))
    :finally (return pos)))

(defun read-number (source start first-code line col prev)
  (with-accessors ((body body)) source
    (let ((code first-code)
          (pos start)
          float?)
      (when (= code 45) ;; -
        (setf code (char-code-at body (incf pos))))

      (cond
        ((= code 48) ;; 0
         (setf code (char-code-at body (incf pos)))
         (when (<= 48 code 57)
           (gql-error (format nil "Invalid number, unexpected digit after 0: ~a"
                              (code-char code)))))

        (t
         (setf pos (read-digits source pos code))
         (setf code (char-code-at body pos))))

      (when (= code 46) ;; .
        (setf float? t)
        (setf code (char-code-at body (incf pos)))
        (setf pos (read-digits source pos code))
        (setf code (char-code-at body pos)))

      (when (or (= code 69) (= code 101)) ;; E e
        (setf float? t)
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

      (make-token (if float? 'float 'int) start pos line col prev
                  (subseq body start pos)))))

(defun read-name (source start line col prev)
  (loop
    :with body = (body source)
    :with pos = (1+ start)
    :with code = 0
    :while (and (/= pos (length body))
                (setf code (char-code-at body pos))
                (or ;; _
                 (= code 95)
                 ;; 0-9
                 (<= 48 code 57)
                 ;; A-Z
                 (<= 65 code 90)
                 ;; a-z
                 (<= 97 code 122)))
    :do (incf pos)
    :finally (return
               (make-token 'name start pos line col prev (subseq body start pos)))))

(defun read-token (lexer prev)
  (loop
    :with source = (source lexer)
    :with body = (body source)
    :with body-length = (length body)
    :with pos = (end prev)
    :if (< pos body-length)
      :do
         (let ((code (char-code-at body pos))
               (line (line lexer))
               (col (- pos (line-start lexer))))
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
             (33 (return ;; !
                   (make-token 'bang pos (1+ pos) line col prev)))
             (35 (return
                   (read-comment source pos line col prev)))
             (36 (return ;; $
                   (make-token 'dollar pos (1+ pos) line col prev)))
             (38 (return ;; &
                   (make-token 'amp pos (1+ pos) line col prev)))
             (40 (return ;; (
                   (make-token 'paren-l pos (1+ pos) line col prev)))
             (41 (return ;; )
                   (make-token 'paren-r pos (1+ pos) line col prev)))
             (46 (return ;; .
                   ;; TODO: Does this handle fall-through properly?
                   (read-spread body pos line col prev)))
             (58 (return ;; :
                   (make-token 'colon pos (1+ pos) line col prev)))
             (61 (return ;; =
                   (make-token 'equals pos (1+ pos) line col prev)))
             (64 (return ;; @
                   (make-token 'at pos (1+ pos) line col prev)))
             (91 (return ;; [
                   (make-token 'bracket-l pos (1+ pos) line col prev)))
             (93 (return ;; ]
                   (make-token 'bracket-r pos (1+ pos) line col prev)))
             (123 (return ;; {
                    (make-token 'brace-l pos (1+ pos) line col prev)))
             (124 (return ;; |
                    (make-token 'pipe pos (1+ pos) line col prev)))
             (125 (return ;; }
                    (make-token 'brace-r pos (1+ pos) line col prev)))
             (34 (if (and (> (length body) 2)
                          (eq (char body (+ pos 1)) #\")
                          (eq (char body (+ pos 2)) #\"))
                     (return ;; "
                       (read-block-string source pos line col prev lexer))
                     (return ;; "
                       (read-string source pos line col prev))))
             (;; 0 1 2 3 4 5 6 7 8 9
              (45 48 49 50 51 52 53 54 55 56 57)
              (return
                (read-number source pos code line col prev)))
             ((;; A-Z_a-z
               65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86
               87 88 89 90 95 97 98 99 100 101 102 103 104 105 106 107 108 109
               110 111 112 113 114 115 116 117 118 119 120 121 122)
              (return
                (read-name source pos line col prev)))
             (t (gql-error (format nil "Unhandled syntax: ~a" (code-char code))))))
    :else
      ;; Account for all parser cases here
      :do
         ;; We have reached end of file - return it as a token
         (return
           (make-token 'eof body-length body-length (line lexer)
                       (- (1+ pos) (line-start lexer)) prev))))
