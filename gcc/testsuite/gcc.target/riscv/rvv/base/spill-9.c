/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zve64d -mabi=ilp32 -fno-schedule-insns -fno-schedule-insns2 -O3" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "riscv_vector.h"

void f (char*);

/*
** stach_check_alloca_1:
**	addi\tsp,sp,-48
**	sw\tra,12\(sp\)
**	sw\ts0,8\(sp\)
**      addi\ts0,sp,16
**	csrr\tt0,vlenb
**	sub\tsp,sp,t0
**      vs1r.v\tv1,0\(sp\)
**	...
**	addi\ta0,sp,15
**	andi\ta0,a0,-16
**	call\tf(?:@plt)?
**	...
**	lw\tra,12\(sp\)
**	lw\ts0,8\(sp\)
**	addi\tsp,sp,48
**	jr\tra
*/
void stach_check_alloca_1 (vuint8m1_t data, uint8_t *base, int y, ...)
{
  vuint8m8_t v0, v8, v16, v24;
  asm volatile ("nop"
                : "=vr" (v0), "=vr" (v8), "=vr" (v16), "=vr" (v24)
                :
                :);
  asm volatile ("nop"
                :
                : "vr" (v0), "vr" (v8), "vr" (v16), "vr" (v24)
                :);
  *(vuint8m1_t *)base = data;
  char* pStr = (char*)__builtin_alloca(y);
  f(pStr);
}
