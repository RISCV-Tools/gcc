/* { dg-do compile } */
/* { dg-require-ifunc "" } */
/* { dg-options "-O0" } */
/* { dg-additional-options "-Wno-experimental-fmv-target" } */

__attribute__((target_clones("default", "dotprod", "sve+sve2")))
int foo ();

__attribute__((target_clones("sve+sve2", "dotprod", "default")))
int foo (int);

/* { dg-final { scan-assembler-times "\n_Z3foov\.default:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._Mdotprod:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\._MsveMsve2:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3foov\.resolver:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\t_Z3foov, %gnu_indirect_function\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z3foov,_Z3foov\.resolver\n" 0 } } */

/* { dg-final { scan-assembler-times "\n_Z3fooi\.default:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\._Mdotprod:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\._MsveMsve2:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n_Z3fooi\.resolver:\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.type\t_Z3fooi, %gnu_indirect_function\n" 0 } } */
/* { dg-final { scan-assembler-times "\n\t\.set\t_Z3fooi,_Z3fooi\.resolver\n" 0 } } */
