[![builds.sr.ht status](https://builds.sr.ht/~theo/gql/commits/.build.yml.svg)](https://builds.sr.ht/~theo/gql/commits/.build.yml?)

# gql

This is a WIP implementation of the GraphQL spec.


## api

```lisp
(gql "{}")

(gql #p"~/path/to/file")
```

Will lex the contents and build a doubly linked list representing the ast. It
returns the first token, which can be traversed with `next` and `prev`.
