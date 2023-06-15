#include "a.h"

struct {
  int i;
  my_ty *x;
} g_a;

extern int glob_var;
int glob_var;

static float f_static;


my_ty **** my_var;

void my_fun() {
  switch (glob_var) {
    case 1:
      ;
      ;
      break;
    case 2:
    case 3:
      ;
      ;
      break;
  }
}