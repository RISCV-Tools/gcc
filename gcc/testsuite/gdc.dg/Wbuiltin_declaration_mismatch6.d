// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-do compile { target { avx_runtime || vect_sizes_16B_8B } } }
module gcc.simd;

alias int4 = __vector(int[4]);
alias short8 = __vector(short[8]);
alias float4 = __vector(float[4]);
alias byte16 = __vector(byte[16]);
struct fake4 { int[4] v; }
enum f = fake4();

void test_blendvector()
{
    blendvector!(int, int, int)(0, 0, 0); // { dg-warning "mismatch in return type" }
    blendvector!(double, int, int)(0, 0, 0); // { dg-warning "mismatch in return type" }
    blendvector!(fake4, int, int)(f, 0, 0); // { dg-warning "mismatch in return type" }

    blendvector!(int4, int, int)(0, 0, 0); // { dg-warning "mismatch in argument 2" }
    blendvector!(int4, double, int)(0, 0, 0); // { dg-warning "mismatch in argument 2" }
    blendvector!(int4, fake4, int)(0, f, 0); // { dg-warning "mismatch in argument 2" }

    blendvector!(int4, int4, int)(0, 0, 0); // { dg-warning "mismatch in argument 3" }
    blendvector!(int4, int4, double)(0, 0, 0); // { dg-warning "mismatch in argument 3" }
    blendvector!(int4, int4, fake4)(0, 0, f); // { dg-warning "mismatch in argument 3" }

    blendvector!(int4, int4, int4)(0, 0, 0);
    blendvector!(int4, short8, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(int4, float4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(int4, byte16, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(int4, int4, short8)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(int4, int4, float4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(int4, int4, byte16)(0, 0, 0); // { dg-error "mismatch in argument 3" }

    blendvector!(float4, int4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(float4, short8, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(float4, float4, int4)(0, 0, 0);
    blendvector!(float4, byte16, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(float4, float4, short8)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(float4, float4, float4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(float4, float4, byte16)(0, 0, 0); // { dg-error "mismatch in argument 3" }

    blendvector!(short8, int4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(short8, short8, int4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(short8, float4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(short8, byte16, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(short8, short8, short8)(0, 0, 0);
    blendvector!(short8, short8, float4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(short8, short8, byte16)(0, 0, 0); // { dg-error "mismatch in argument 3" }

    blendvector!(byte16, int4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(byte16, short8, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(byte16, float4, int4)(0, 0, 0); // { dg-error "mismatch in argument 2" }
    blendvector!(byte16, byte16, int4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(byte16, byte16, short8)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(byte16, byte16, float4)(0, 0, 0); // { dg-error "mismatch in argument 3" }
    blendvector!(byte16, byte16, byte16)(0, 0, 0);
}

// The following declarations of the simd intrinsics are without any guards
// to verify `d/intrinsics.cc` is doing checks to prevent invalid lowerings.
V0 blendvector(V0, V1, M)(V0, V1, M);
