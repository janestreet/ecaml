#!/bin/bash

# Modify the jbuild and import.ml in this directory to run tests
# against Ecaml's Async_ecaml implementation.  This is used to
# manually test Async_ecaml.

set -e -u -o pipefail

cd $(hg root)/lib/async_kernel/test

query='
(if (pipe (index 0) (equals library))
    (change (seq
             (topdown (try (record (_ id) (libraries (rewrite (@X) (ecaml @X))))))
             (topdown (try (record (_ id) (inline_tests (rewrite (@X) ((only_shared_object true) @X))))))
            ))
  this)'

jbuild="$(cat jbuild)"
echo "$jbuild" | sexp query "$query" >jbuild

replacement='s#include Expect_test_helpers#include (Expect_test_helpers : module type of Expect_test_helpers with module Expect_test_config := Expect_test_helpers.Expect_test_config)\
\
module Expect_test_config = Ecaml.Async_ecaml.Expect_test_config#'

sed -i "$replacement" import.ml

echo "build the target (alias \${ROOT}/lib/async_kernel/test/runtest) to test async_kernel against async_ecaml."
