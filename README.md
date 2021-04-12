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

### What parts of the spec is worked on yet

 - [x] [Language](https://spec.graphql.org/June2018/#sec-Language)
 - [x] [Type System](https://spec.graphql.org/June2018/#sec-Type-System)
 - [ ] [Introspection](https://spec.graphql.org/June2018/#sec-Introspection)
 - [ ] [Validation](https://spec.graphql.org/June2018/#sec-Validation)
 - [ ] [Execution](https://spec.graphql.org/June2018/#sec-Execution)
 - [ ] [Response](https://spec.graphql.org/June2018/#sec-Response)


### Contact
If interested, you can open an issue at the
[tracker](https://todo.sr.ht/~theo/gql), or send an email to the [public
inbox](https://lists.sr.ht/~theo/public-inbox), or directly to <~theo/public-inbox@lists.sr.ht>
