#include <stdio.h>
#include "caml/mlvalues.h"

void ecaml_test_clearerr_stdin() { clearerr(stdin); }
