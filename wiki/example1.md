# Example1.md

Ok, so let's see how we can get something working using this framework.  We'll
start by adding a couple of quickloads, just for good measure.

```lisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:gql :cl-json :hunchentoot) :silent t))
```

Then, we need to define our package and go inside of it.

```lisp
(defpackage :gql-exampleapp
  (:use :cl :gql))

(in-package :gql-exampleapp)
```

Because this is merely a simple demonstration to show the proof of concept, we
don't need a full database.  In a typical web app this is not how we should
persist data, but it is good enough for our purposes.

```lisp
(defparameter *fake-db* (make-hash-table :test #'equal))
(setf (gethash "name" *fake-db*) "Theodor")
(setf (gethash "age" *fake-db*) 31)
```

We create our database and add in a couple of mappings.  Now for the more
interesting part.  There are a couple of things needed in order to make queries
and get responses.  First, we need a defined schema.  We have supplied one,
[here](https://git.sr.ht/~theo/gql/tree/master/item/example/schema.graphql).  It
looks like this:

```
type Query {
  name: String
  age: Int
}
```

We define it, along with our variable-values like so:
```lisp
(defvar *example-schema*
  (build-schema (asdf:system-relative-pathname 'gql "example/schema.graphql")))
(defvar *variable-values* (make-hash-table :test #'equal))
```

Great, this is a good start!  The last item on our agenda is resolving
information from our database.  `gql` provides a generic function, `resolve`,
which sole purpose is to deal with this.  We need only a simple one here:

```lisp
(defmethod resolve (object-type object-value field-name arg-values)
  (gethash field-name *fake-db*))
```

The last few things is running a server and defining an easy handler:

```lisp
(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 3000))

(defun query (item)
  (build-schema (format nil "query { ~a }" item)))

(hunchentoot:define-easy-handler (home :uri "/home") (item)
  (setf (hunchentoot:content-type*) "text/plain")
  (when item
    (with-schema *example-schema*
      (let ((result (execute-request (query item) nil *variable-values* nil)))
        (format nil "~a~%" (cl-json:encode-json-to-string result))))))
```

Very nice.  The last thing is now to eval this line in your repl, from inside of
`:gql-exampleapp` package:

```lisp
(hunchentoot:start *server*)
```

Now, proceed to [localhost:3000/home](localhost:3000/home), then start typing
queries in the url like so:

```
localhost:3000/home?item=age
localhost:3000/home?item=name
localhost:3000/home?item=name age
```

And off we go!  The full file can be found here:
[example.lisp](https://git.sr.ht/~theo/gql/tree/master/item/example/example.lisp)
