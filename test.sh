#!/bin/bash

set -e

sbcl --non-interactive --load test.lisp

if [ $? -eq 0 ]
then
  echo "Successful"
  exit 0
else
  echo "Failure" >&2
  exit 1
fi


