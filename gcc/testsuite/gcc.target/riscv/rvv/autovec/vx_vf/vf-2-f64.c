/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d --param=fpr2vr-cost=1" } */

#include "vf-1-f64.c"

/* { dg-final { scan-assembler-not {vfmadd.vf} } } */
/* { dg-final { scan-assembler-not {vfmsub.vf} } } */
/* { dg-final { scan-assembler-not {vfnmadd.vf} } } */
/* { dg-final { scan-assembler-not {vfnmsub.vf} } } */
/* { dg-final { scan-assembler-not {vfmacc.vf} } } */
/* { dg-final { scan-assembler-not {vfmsac.vf} } } */
/* { dg-final { scan-assembler-not {vfnmacc.vf} } } */
/* { dg-final { scan-assembler-not {vfnmsac.vf} } } */
