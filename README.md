[![builds.sr.ht status](https://builds.sr.ht/~theo/gql/commits/.build.yml.svg)](https://builds.sr.ht/~theo/gql/commits/.build.yml?)

# gql

This is a WIP implementation of the [GraphQL spec](https://spec.graphql.org/June2018/).

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

 - [x] [Language](https://spec.graphql.org/June2018/#sec-Language)
 - [x] [Type System](https://spec.graphql.org/June2018/#sec-Type-System) (extensions still missing)
 - [ ] [Introspection](https://spec.graphql.org/June2018/#sec-Introspection)
 - [ ] [Validation](https://spec.graphql.org/June2018/#sec-Validation)
 - [ ] [Execution](https://spec.graphql.org/June2018/#sec-Execution)
 - [ ] [Response](https://spec.graphql.org/June2018/#sec-Response)

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


### Contact
If interested, you can open an issue at the
[tracker](https://todo.sr.ht/~theo/gql), or send an email to the [public
inbox](https://lists.sr.ht/~theo/public-inbox), or directly to <~theo/public-inbox@lists.sr.ht>
