# Example 2

Let's try something a little more complicated.  We do want to be able to define
and use nested structures, such as the ones defined in the
[spec](https://spec.graphql.org/draft/#example-19f2a).  So let's use this.

The parts we will be exploring now are accessing a `Dog`, and getting the
properties from the `owner`, and at least get one `Pet`.  The interesting part
here is that `Pet` is an interface, so we need to be able to figure out what
actually is inside that list.  We still don't want to set up a database, so
we'll just define a simple object.

First, we need some libraries, including `gql`.  Let's quickload them:

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:gql :cl-json) :silent t))
```

Now let's create some classes reflecting what we see in the schema linked above:

```lisp
(defclass pet (gql-object)
  ((name :initarg :name :accessor name)))

(defclass dog (pet)
  ((owner :initarg :owner :accessor owner)))

(defclass sentient (gql-object)
  ((name :initarg :name :accessor name)))

(defclass human (sentient)
  ((pets :initarg :pets :accessor pets)))
```

An assumption is made here that classes also can act as interfaces, and that we
by convention try not to muddle up everything.  So pet inherits from an external
class from `gql`, `gql-object`.  This is just to provide one property,
`type-name` to the classes.  This is used internally in `gql` to simplify
inference of interfaces.  See [assumptions](wiki/assumptions.md)

Let's see our "database":

```lisp
(defvar *doggo*
  (make-instance
   'dog
   :name "Bingo-Bongo"
   :type-name "Dog"
   :owner (make-instance
           'human
           :name "Wingle Wangle"
           :type-name "Human"
           :pets `(,(make-instance
                     'dog
                     :name "Bingo-Bongo"
                     :type-name "Dog")
                   ,(make-instance
                     'dog
                     :name "Bango-Wango"
                     :type-name "Dog")))))
```

Next up are the resolvers.  These are pretty simple to make, but has a bit ugly syntax for now:

```lisp
(defvar *query-resolvers*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "dog" ht)
          (lambda (arg) (declare (ignorable arg)) *doggo*))
    ht))

(defvar *dog-resolvers*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "name" ht) (lambda (dog) (name dog)))
    (setf (gethash "owner" ht) (lambda (dog) (owner dog)))
    ht))

(defvar *human-resolvers*
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "name" ht) (lambda (human) (name human)))
    (setf (gethash "pets" ht) (lambda (human) (pets human)))
    ht))
```

`*query-resolvers*` has just a simple function inside, which returns object from
the "database".  This is typically where the first calls to the database is
made. `*dog-resolvers*` contains lambdas wrapping getters from the object as
they are defined through clos.  We wrap it in a lambda so that `gql` can call
the function for us.  The same is true for `*human-resolvers*`.

At last, we can do the request.  We need to parse the schema, then make the
request.  We don't need any variable-values now, because we still keep the
queries simple.  Our function looks like this:

```lisp
(defun example2 (query)
  (with-schema (build-schema (asdf:system-relative-pathname 'gql-tests #p"t/test-files/validation-schema.graphql"))
    (let* ((res (gql::execute-request
                 (build-schema query) nil (make-hash-table :test #'equal) nil)))
      (format t "~a" (cl-json:encode-json-to-string res)))))
```

Now we just bind the resolvers to the exported `*resolvers*` symbol and run our query:

```lisp
(let ((*resolvers* (make-hash-table :test #'equal)))
  (setf (gethash "Query" *resolvers*) *query-resolvers*)
  (setf (gethash "Dog" *resolvers*) *dog-resolvers*)
  (setf (gethash "Human" *resolvers*) *human-resolvers*)
  (example2 "query { dog { name owner { name pets { name } } } }"))
```

This yields:

```json
{
  "data": {
    "dog": {
      "name": "Bingo-Bongo",
      "owner": {
        "name": "Wingle Wangle",
        "pets": [
          {
            "name": "Bingo-Bongo"
          },
          {
            "name": "Bango-Wango"
          }
        ]
      }
    }
  },
  "errors": null
}
```

We can add aliases to our queries, so that we can be super agile(tm) in the front-end:

```lisp
(let ((*resolvers* (make-hash-table :test #'equal)))
  (setf (gethash "Query" *resolvers*) *query-resolvers*)
  (setf (gethash "Dog" *resolvers*) *dog-resolvers*)
  (setf (gethash "Human" *resolvers*) *human-resolvers*)
  (example2 "query { dog { name owner: wingle { name pets: dogs { name } } } }"))
```

Now we get:

```json
{
  "data": {
    "dog": {
      "name": "Bingo-Bongo",
      "wingle": {
        "name": "Wingle Wangle",
        "dogs": [
          {
            "name": "Bingo-Bongo"
          },
          {
            "name": "Bango-Wango"
          }
        ]
      }
    }
  },
  "errors": null
}
```

That's it!  The full file can be found here:
[example2.lisp](https://git.sr.ht/~theo/gql/tree/master/item/example/example2.lisp)
