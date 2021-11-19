[![builds.sr.ht status](https://builds.sr.ht/~theo/gql/commits/.build.yml.svg)](https://builds.sr.ht/~theo/gql/commits/.build.yml?)

# gql

This is a WIP implementation of the [GraphQL spec](https://spec.graphql.org/draft/).

Right now this is mostly useful as a poor mans syntax checker for queries,
should you try to send requests using `hunchentoot` or whatnot. If the string or
file parses ok, `gql` returns two values, the original string and the parsed
object.


## api

```lisp
(gql "{
  user(id: 4) {
    id
    name
    profilePic(width: 100, height: 50)
  }
}" t)

(gql #p"~/path/to/file")
```

Will lex and parse the contents, signaling conditions when there are any.  Use
`t` as the second argument to `gql` to print debug information.

### What parts of the spec is worked on thus far

 - [x] [Language](https://spec.graphql.org/draft/#sec-Language)
 - [x] [Type System](https://spec.graphql.org/draft/#sec-Type-System)
 - [ ] [Introspection](https://spec.graphql.org/draft/#sec-Introspection)
 - [ ] [Validation](https://spec.graphql.org/draft/#sec-Validation)
 - [ ] [Execution](https://spec.graphql.org/draft/#sec-Execution)
 - [ ] [Response](https://spec.graphql.org/draft/#sec-Response)

### Parser api
The api is not done yet, but the main entry point for the parser is the macro
`defparser`.  It creates a new `defmethod parse`, and also brings the current
parser and the current token into scope.  An example of how to define a parser
`defmethod` could be:

```lisp
(defparser directive-definition
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


## Code generation
Gql can generate valid GraphQL code from initialized ast objects.  It pretty
prints by default, so it can be used to produce actual, human readable content.
Some time down the line I'll add a lisp dsl to aid with code generation, like
done in `sxql`.

### Example
```lisp
(multiple-value-bind (_ doc-obj) (gql "query { x { y } }")
  (generate doc-obj 0 t))

;; Will output this to the repl:
;;
;; query {
;;   x {
;;     y
;;   }
;; }
```
This means that we now can actually detect syntax errors and generate valid GraphQL statements.


## Reflection
Because we return the actual ast as the second value, it is inspectable and
modifiable from the repl.  I'm not really sure what that can be used to do, but
it is fun nonetheless.  An example session is:

```
GQL> (gql #p"~/lol.gql")
"query withFragments {
  user(id: 4) {
    friends(first: 10) {
      ...friendFields
    }
    mutualFriends(first: \"arst\") {
      ...friendFields
    }
  }
}
"
#<DOCUMENT {700778C7D3}>
GQL> (with-slots (definitions) #v0:1
       (with-slots (name selection-set) (car definitions)
         (setf (name name) "qwfpgjluy")
         (with-slots (selections) selection-set
           (with-slots (selection-set) (car selections)
             (destructuring-bind (first second) (selections selection-set)
               (setf (selections selection-set) (list second))
               first)))))
#<FIELD {700778B583}>
GQL> (generate #v1:0)
"friends(first: 10) {
  ...friendFields
}"
GQL> (generate #v0:1)
query qwfpgjluy {
  user(id: 4) {
    mutualFriends(first: "arst") {
      ...friendFields
    }
  }
}
NIL
```

## Making a request:
Use your json library of choice, map over a query either from a file or as a
string to the class `query`, then send it with the appropriate headers.  Easy as
pie.

```lisp

(defun make-query (query &optional variables)
  (com.inuoe.jzon:stringify
   (make-instance 'query :query query)))

(defmethod request (query)
  (format t "~%~a~%"
          (flexi-streams:octets-to-string
           (drakma:http-request
            "https://git.sr.ht/query"
            :method :post
            :content (make-query (gql query))
            :external-format-out 'utf-8
            :additional-headers `(("Content-Type" . "application/json")
                                  ("Authorization" . ,(concatenate 'string "Bearer "
                                                                   (uiop:getenv "srhttoken"))))))))

```

Now send the thing:

```
GQL> (request #p"~/lol.gql")

{"data":{"me":{"canonicalName":"~theo","repositories":{"results":[...]}}}}

```


## TODO:
  - [ ] Complete the Working Draft spec
  - [x] Complete the code generation
  - [ ] Create lisp-like dsl

### Contact
If interested in helping out, you can open an issue at the
[tracker](https://todo.sr.ht/~theo/gql), or send an email to the [public
inbox](https://lists.sr.ht/~theo/public-inbox), or directly to
<~theo/public-inbox@lists.sr.ht>. In addition, you can find my private email
sitting around in the commits.
