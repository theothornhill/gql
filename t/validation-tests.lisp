(in-package #:gql-tests)

(deftest validation
  (testing "Only allows ExecutableDefintition in a Document"
    ;; https://spec.graphql.org/draft/#sec-Executable-Definitions
    (ok
     (validator-errors-p
      "query getDogName {
  dog {
    name
    color
  }
}

extend type Dog {
  color: String
}
"))
    (ok
     (validator-errors-p
      "query getDogName {
  dog {
    name
    color
  }
}

mutation dogOperation {
  mutateDog {
    id
  }
}

extend type Dog {
  color: String
}
"))
    (ng
     (validator-test
      "query getDogName {
  dog {
    name
    color
  }
}
"))
    (ng
     (validator-test
      "query getDogName {
  dog {
    name
    color
  }
}
mutation dogOperation {
  mutateDog {
    id
  }
}

fragment friendFields on User {
  id
  name
  profilePic(size: 50)
}"))
    (ng
     (validator-test ;; multiple queries with unique names are ok
      "query getDogName {
  dog {
    name
  }
}

query getOwnerName {
  dog {
    owner {
      name
    }
  }
}
"))
    (ok
     (validator-errors-p
      "query getName {
  dog {
    name
  }
}

query getName {
  dog {
    owner {
      name
    }
  }
}
"))
    (ok
     (validator-errors-p
      "query dogOperation {
  dog {
    name
  }
}

mutation dogOperation {
  mutateDog {
    id
  }
}
"))
    (ok
     (validator-errors-p
      "{
  dog {
    name
  }
}

query getName {
  dog {
    owner {
      name
    }
  }
}")))
  (testing "Subscription validation"
    (ng
     (validator-errors-p
      "subscription sub {
  newMessage {
    body
    sender
  }
}
"))
    (ng
     (validator-errors-p
      "subscription sub {
  ...newMessageFields
}

fragment newMessageFields on Subscription {
  newMessage {
    body
    sender
  }
}
" :no-schema t))
    (ok
     (validator-errors-p
      "subscription sub {
  newMessage {
    body
    sender
  }
  disallowedSecondRootField
}" :no-schema t))
    (ok
     (validator-errors-p
      "subscription sub {
  ...multipleSubscriptions
}

fragment multipleSubscriptions on Subscription {
  newMessage {
    body
    sender
  }
  disallowedSecondRootField
}" :no-schema t))
    (ok
     (validator-errors-p
      "subscription sub {
  __typename
}" :no-schema t))))

