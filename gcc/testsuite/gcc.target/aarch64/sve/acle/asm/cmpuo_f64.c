/* { dg-final { check-function-bodies "**" "" "-DCHECK_ASM" } } */

#include "test_sve_acle.h"

/*
** cmpuo_f64_tied:
**	fcmuo	p0\.d, p0/z, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpuo_f64_tied, svfloat64_t,
		p0 = svcmpuo_f64 (p0, z0, z1),
		p0 = svcmpuo (p0, z0, z1))

/*
** cmpuo_f64_untied:
**	fcmuo	p0\.d, p1/z, (z0\.d, z1\.d|z1\.d, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpuo_f64_untied, svfloat64_t,
		p0 = svcmpuo_f64 (p1, z0, z1),
		p0 = svcmpuo (p1, z0, z1))

/*
** cmpuo_d4_f64:
**	mov	(z[0-9]+\.d), d4
**	fcmuo	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_ZD (cmpuo_d4_f64, svfloat64_t, float64_t,
		 p0 = svcmpuo_n_f64 (p1, z0, d4),
		 p0 = svcmpuo (p1, z0, d4))

/*
** cmpuo_0_f64:
**	movi?	[vdz]([0-9]+)\.?(?:[0-9]*[bhsd])?, #?0
**	fcmuo	p0\.d, p1/z, (z0\.d, z\1\.d|z\1\.d, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpuo_0_f64, svfloat64_t,
		p0 = svcmpuo_n_f64 (p1, z0, 0),
		p0 = svcmpuo (p1, z0, 0))

/*
** cmpuo_1_f64:
**	fmov	(z[0-9]+\.d), #1\.0(?:e\+0)?
**	fcmuo	p0\.d, p1/z, (z0\.d, \1|\1, z0\.d)
**	ret
*/
TEST_COMPARE_Z (cmpuo_1_f64, svfloat64_t,
		p0 = svcmpuo_n_f64 (p1, z0, 1),
		p0 = svcmpuo (p1, z0, 1))
