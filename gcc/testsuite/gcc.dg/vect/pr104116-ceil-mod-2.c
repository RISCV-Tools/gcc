/* { dg-additional-options "-fgimple -fdump-tree-optimized" } */
/* { dg-require-effective-target vect_int} */
/* { dg-require-effective-target vect_condition} */
/* { dg-require-effective-target vect_shift} */



#include "pr104116.h"
#include "tree-vect.h"


TEST_FN(__CEIL_MOD, 2, div)

int main (void)
{
  check_vect ();
  int *a = (int*)&arr;
  init_arr(a, N);
  div(a);
  for (int i=0; i<N; i++)
  {
    int expected = cl_mod (i - N/2, 2);
    if (expected != a[i])
      abort();
  }

  return 0;
}

/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 1 "vect" } } */