(in-package #:gql-tests)

(deftest validation
  (testing "Only allows ExecutableDefintition in a Document"
    ;; https://spec.graphql.org/draft/#sec-Executable-Definitions
    (ok
     (validator-errors
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
     (validator-errors
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
     (validator-errors
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
     (validator-errors
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
     (validator-errors
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
}"))))

