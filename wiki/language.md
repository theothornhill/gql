# language.lisp
The api isn't really settled yet, but the main entry point for the nodes in
GraphQL is defined in the file `language.lisp`.  In here we see a convenience
macro at work `defgql`, which in turn is defined in `parser-utils.lisp`.  The
point of this macro is to co-locate all the parts needed for lexing, parsing and
generating a node.

## `defnode`
Our nodes are defined as a simple `defclass`, inside of the `:node` part of
`defgql`

```lisp
(defnode directive-definition description name args locations)
```

## `defparser`
Our parser is the macro `defparser`.  It creates a new `defmethod parse`, and
also brings the current parser and the current token into scope. It is used
inside of the `:parser` part of `defgql`.  An example of how to define a parser
`defmethod` could be:

```lisp
(defparser directive-definition ()
  (make-node 'directive-definition
    :description (parse 'description)
    :name (expect-then-parse '("directive" at) 'name)
    :args (parse 'argument-definitions)
    :locations (expect-then-parse "on" 'directive-locations)))
```

In this case we produce the defmethod for `parse directive-definition`, then
parses the respective nodes recursively.  When there are some tokens to traverse
over, the utility function `expect-then-parse` takes either a list of strings or
symbols, or just one single string or symbol, then advances the lexer.  If
anything is unexpected, signal an error.

Some nodes need additional variables to be able to refer to input to the `parse`
method.  These can be added to the `keys` parameter of the macro, and later
referenced in the body.  Note that this is currently a very naive implementation
of this that only takes symbols, and you should be really afraid about variable
capturing if using this.

```lisp
(defparser inline-fragment (type-condition-p)
  (make-node 'inline-fragment
    :type-condition (when type-condition-p (parse 'named-type))
    :directives (parse 'directives)
    :selection-set (parse 'selection-set)))
```

## `defgenerator`
You guessed it, the `defgenerator` works in the same way, by using it inside of
the `:generator` section of `defgql`.

```lisp
(defgenerator directive-definition ()
    (cat "~@[~a~%~]"
         "directive @~a"
         "~@[~a~]"
         "~@[ on~%~{  | ~a~^~%~}~]")
    (when (description node)
      (generate (description node) :indent-level indent-level :stream stream))
    (generate (name node) :indent-level indent-level :stream stream)
    (gather-nodes (args node) indent-level)
    (gather-nodes (locations node) indent-level))
```

What's different with `defgenerator` from the others is that you can optionally
add a symbol `full` to the definition, thus enabling inserting a full body,
instead of the convenience wrapper of `(format stream ,@body)` usually used.  In
addition it works the same way as `defparser`, which can also introduce
variables to the body.

An example of this is
```lisp
(defgenerator string-value (full) ;; This keyword is significant...
  (if (blockp node)
      (format stream "~@[\"\"\"~a\"\"\"~]" (value node))
      (format stream "~@[\"~a\"~]" (value node))))
```

## The resulting macro

```lisp
(defgql directive-definition
  :node (defnode directive-definition description name args locations)
  :parser (defparser directive-definition ()
            (make-node 'directive-definition
              :description (parse 'description)
              :name (expect-then-parse '("directive" at) 'name)
              :args (parse 'argument-definitions)
              :locations (expect-then-parse "on" 'directive-locations)))
  :generator (defgenerator directive-definition ()
               (cat "~@[~a~%~]"
                    "directive @~a"
                    "~@[~a~]"
                    "~@[ on~%~{  | ~a~^~%~}~]")
               (when (description node)
                 (generate (description node) :indent-level indent-level :stream stream))
               (generate (name node) :indent-level indent-level :stream stream)
               (gather-nodes (args node) indent-level)
               (gather-nodes (locations node) indent-level)))
```

# Code generation
Gql can generate valid GraphQL code from initialized ast objects.  It pretty
prints by default, so it can be used to produce actual, human readable content.
Some time down the line I'll add a lisp dsl to aid with code generation, like
done in `sxql`.

## Example

```lisp
(multiple-value-bind (_ doc-obj) (gql "query { x { y } }")
  (generate doc-obj :indent-level 0 :stream t))

;; Will output this to the repl:
;;
;; query {
;;   x {
;;     y
;;   }
;; }
```

This means that we now can actually detect syntax errors and generate valid GraphQL statements.
