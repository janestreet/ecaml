#include <stdio.h>
#include "caml/mlvalues.h"

value ecaml_test_clearerr_stdin() {
  clearerr(stdin);
  return Val_unit;
}
