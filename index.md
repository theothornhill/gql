[![builds.sr.ht status](https://builds.sr.ht/~theo/gql/commits/.build.yml.svg)](https://builds.sr.ht/~theo/gql/commits/.build.yml?)

# gql

Our goal is to be a fully compliant implementation of the [GraphQL
spec](https://spec.graphql.org/draft/). Non-goals are serializing `json`, making
requests or doing authentication.  This should be handled by better suited
libraries.  However, we do want to be useful in the space of web development, so
integrations to `hunchentoot`, `woo`, `drakma` or `$LIBRARY` will be considered,
and possibly made in the future.  For the time being the only goal is the spec.

# Assumptions

We made some [assumptions](wiki/assumptions.md) during development

# Language

Descriptions for how we implement the [language](wiki/language.md)

# api

```lisp
(gql "{
  user(id: 4) {
    id
    name
    profilepic(width: 100, height: 50)
  }
}" t) ;; this t enables debug mode, printing lots of garbage to stdout

(gql #p"~/path/to/file") ;; you can also just supply a file path
```

will lex and parse the contents, signaling conditions when there are any.  use
`t` as the second argument to `gql` to print debug information.

# Reflection
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

# Making a request
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
