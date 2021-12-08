(in-package #:gql-tests)

(deftest validation
  (testing "Only allows ExecutableDefintition in a Document"
    ;; https://spec.graphql.org/draft/#sec-Executable-Definitions
    (ok (string=  "[{\"message\":\"Each definition must be executable.\",\"locations\":[{\"line\":1,\"column\":40}],\"path\":null,\"extensions\":null}]"
                  (validator-test-helper
                   "query getDogName { dog { name color } } extend type Dog { color: String }")))
    (ok (string= "[{\"message\":\"Each definition must be executable.\",\"locations\":[{\"line\":1,\"column\":83}],\"path\":null,\"extensions\":null}]"
                 (validator-test-helper
                  "query getDogName { dog { name color } } mutation dogOperation { mutateDog { id } } extend type Dog { color: String }")))
    (ok (string= "null" (validator-test-helper "query getDogName { dog { name color } }")))
    (ok (string= "null" (validator-test-helper "query getDogName { dog { name color } } mutation dogOperation { mutateDog { id } } fragment friendFields on User { id name profilePic(size: 50) }")))
    (ok (string= "null" (validator-test-helper "query getDogName { dog { name } } query getOwnerName { dog { owner { name } } }")))
    (ok (string= "[{\"message\":\"Each operation must have a unique name.\",\"locations\":[{\"line\":1,\"column\":0},{\"line\":1,\"column\":31}],\"path\":null,\"extensions\":null}]"
         (validator-test-helper "query getName { dog { name } } query getName { dog { owner { name } } } ")))
    (ok (string= "[{\"message\":\"Each operation must have a unique name.\",\"locations\":[{\"line\":1,\"column\":0},{\"line\":1,\"column\":36}],\"path\":null,\"extensions\":null}]"
                 (validator-test-helper "query dogOperation { dog { name } } mutation dogOperation { mutateDog { id } } ")))
    (ok (string= "[{\"message\":\"An anonymous definition must be alone.\",\"locations\":[{\"line\":1,\"column\":0}],\"path\":null,\"extensions\":null}]"
                 (validator-test-helper "{ dog { name } } query getName { dog { owner { name } } }"))))
  (testing "Subscription validation"
    (ok (string= "null" (validator-test-helper "subscription sub { newMessage { body sender } } ")))
    (ok (string= "[{\"message\":\"A subscription must have exactly one entry.\",\"locations\":[{\"line\":1,\"column\":0}],\"path\":null,\"extensions\":null}]"
                 (validator-test-helper "subscription sub { newMessage { body sender } disallowedSecondRootField }" :no-schema t)))
    (ok (string= "[{\"message\":\"A subscription must have exactly one entry.\",\"locations\":[{\"line\":1,\"column\":0}],\"path\":null,\"extensions\":null}]"
                 (validator-test-helper "subscription sub { ...multipleSubscriptions } fragment multipleSubscriptions on Subscription { newMessage { body sender } disallowedSecondRootField }" :no-schema t)))
    (ok (string= "[{\"message\":\"Root field must not begin with \\\"__\\\"  which is reserved by GraphQL introspection.\",\"locations\":[{\"line\":1,\"column\":0}],\"path\":null,\"extensions\":null}]"
                 (validator-test-helper "subscription sub { __typename }" :no-schema t))))
  (testing "Each fragment’s name must be unique within a document"
    (ok (string= "[{\"message\":\"An anonymous definition must be alone.\",\"locations\":[{\"line\":1,\"column\":0}],\"path\":null,\"extensions\":null},{\"message\":\"Each operation must have a unique name.\",\"locations\":[{\"line\":1,\"column\":27},{\"line\":1,\"column\":64}],\"path\":null,\"extensions\":null}]"
                 (validator-test-helper "{ dog { ...fragmentOne } } fragment fragmentOne on Dog { name } fragment fragmentOne on Dog { owner { name } }")))))

