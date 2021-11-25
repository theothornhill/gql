[![builds.sr.ht status](https://builds.sr.ht/~theo/gql/commits/.build.yml.svg)](https://builds.sr.ht/~theo/gql/commits/.build.yml?)

# gql

## Documentation

Documentation can be found at the [wiki](https://man.sr.ht/~theo/gql/)

This is an implementation of the [GraphQL spec](https://spec.graphql.org/draft/)
for Common Lisp.

## What parts of the spec is worked on thus far

This does not mean that I'm done here.

 - [x] [Language](https://spec.graphql.org/draft/#sec-Language)
 - [x] [Type System](https://spec.graphql.org/draft/#sec-Type-System)
 - [ ] [Introspection](https://spec.graphql.org/draft/#sec-Introspection)
 - [x] [Validation](https://spec.graphql.org/draft/#sec-Validation)
 - [x] [Execution](https://spec.graphql.org/draft/#sec-Execution)
 - [ ] [Response](https://spec.graphql.org/draft/#sec-Response)


## Development
Development happens over at [Sourcehut](https://sr.ht/) , you can open an issue
at the [tracker](https://todo.sr.ht/~theo/gql), or send an email to the [mailing
lists](https://sr.ht/~theo/gql/lists).  Issues at
[github](https://github.com/theothornhill/gql) also possible for those not able
to or willing to use [Sourcehut](https://sr.ht/)


## Test files license
Some test files are taken from the [GraphQL
spec](https://spec.graphql.org/draft/), and they are listed under
`t/test-files`. These carry the same license as the GraphQl spec, which
currently is MIT.  The same goes for the strings in many of the test cases,
which also is taken from the spec.
