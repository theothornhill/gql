(in-package #:gql)

;;; Utils

(defun slurp (file)
  (uiop:read-file-string file))

;;; api

(defgeneric gql (input)
  (:documentation "Entry point for lexing and parsing"))

(defmethod gql ((str string))
  (lex-whole-input str))

(defmethod gql ((f pathname))
  (lex-whole-input (slurp f)))

(declaim (ftype (function (string) token) lex-whole-input))
(defun lex-whole-input (input)
  (loop
    with lexer = (make-lexer input)
    with first-token = (token lexer)
    with token = (token lexer)
    until (eq (kind token) 'eof)
    do (setq token (advance lexer))
    finally (return first-token)))
