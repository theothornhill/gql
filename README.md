[![builds.sr.ht status](https://builds.sr.ht/~theo/gql/commits/.build.yml.svg)](https://builds.sr.ht/~theo/gql/commits/.build.yml?)

# gql

This is a WIP implementation of the [GraphQL spec](https://spec.graphql.org/June2018/)


## api

```lisp
(gql "{}")

(gql #p"~/path/to/file")
```

Will lex and parse the contents, signaling conditions when there are any.
