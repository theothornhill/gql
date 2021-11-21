#!/bin/bash

set -e

sbcl --non-interactive \
     --load gql.asd \
     --eval '(ql:quickload :rove)' \
     --eval '(ql:quickload :gql)' \
     --eval '(ql:quickload :gql-tests)' \
     --eval '(ql:quickload :gql-tests)' \
     --eval '(multiple-value-bind (foo bar fails) (rove:run :gql-tests) (when fails (sb-ext:exit :code 1)))'
