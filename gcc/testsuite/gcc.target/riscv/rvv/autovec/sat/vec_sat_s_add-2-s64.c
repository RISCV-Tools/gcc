/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -ftree-vectorize -fdump-tree-optimized" } */

#include "vec_sat_arith.h"

DEF_VEC_SAT_S_ADD_FMT_2(int64_t, uint64_t, INT64_MIN, INT64_MAX)

/* { dg-final { scan-tree-dump-times ".SAT_ADD " 1 "optimized" { target { no-opts "-O2" } } } } */
/* { dg-final { scan-tree-dump-times ".SAT_ADD " 2 "optimized" { target { no-opts "-O3" } } } } */
/* { dg-final { scan-assembler-times {vsadd\.vv} 1 } } */
