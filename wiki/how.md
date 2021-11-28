# How

Yes, just how does this tie together?

Right now there are very few options, but the general idea is actually working!

First, for good measure just quickload the project, tests and run them to check
that not _everything_ is broken.

```lisp
(progn 
  (ql:quickload :gql)
  (ql:quickload :gql-tests)
  (asdf:test-system :gql-tests))
```

Now, from inside of the `gql` package you can run 

```lisp
(with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
  (execute-request (build-schema "query { dog { name owner { name } } }")
                   "Query" 
                   (make-hash-table) 
                   nil))
```

And make an actual query against an actual schema.  There are no resolvers or
any fanciness, and many variants of the queries will crash in mysterious ways.

Still, it's a start!
