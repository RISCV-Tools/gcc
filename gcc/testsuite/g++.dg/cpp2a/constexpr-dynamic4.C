// PR c++/88337 - Implement P1327R1: Allow dynamic_cast/typeid in constexpr.
// { dg-do compile { target c++20 } }
// { dg-additional-options "-fdelete-null-pointer-checks" }

// From clang's constant-expression-cxx2a.cpp.

#if __cpp_constexpr_exceptions >= 202411L
namespace std {
  struct exception {
    constexpr exception () noexcept {}
    constexpr virtual ~exception () noexcept {}
    constexpr exception (const exception &) = default;
    constexpr exception &operator= (const exception &) = default;
    constexpr virtual const char *what () const noexcept { return "std::exception"; }
  };
  struct bad_cast : public exception {
    constexpr virtual ~bad_cast () noexcept {}
    constexpr virtual const char *what () const noexcept { return "std::bad_cast"; }
  };
}
#endif

struct A2 { virtual void a2(); };
struct A : A2 { virtual void a(); };
struct B : A {}; 
struct C2 { virtual void c2(); };
struct C : A, C2 { A *c = dynamic_cast<A*>(static_cast<C2*>(this)); };
struct D { virtual void d(); };
struct E { virtual void e(); };
struct F : B, C, D, private E { void *f = dynamic_cast<void*>(static_cast<D*>(this)); };
struct Padding { virtual void padding(); };
struct G : Padding, F {}; 

constexpr G g;

// During construction of C, A is unambiguous subobject of dynamic type C.
static_assert(g.c == (C*)&g);
// ... but in the complete object, the same is not true, so the runtime fails.
static_assert(dynamic_cast<const A*>(static_cast<const C2*>(&g)) == nullptr);

// dynamic_cast<void*> produces a pointer to the object of the dynamic type.
static_assert(g.f == (void*)(F*)&g);
static_assert(dynamic_cast<const void*>(static_cast<const D*>(&g)) == &g);

constexpr int d_a = (dynamic_cast<const A&>(static_cast<const D&>(g)), 0); // { dg-error "reference .dynamic_cast. failed" "" { target c++23_down } }
// { dg-message ".A. is an ambiguous base class of dynamic type .G." "" { target c++23_down } .-1 }
// { dg-error "uncaught exception" "" { target c++26 } .-2 }
// Can navigate from A2 to its A...
static_assert(&dynamic_cast<A&>((A2&)(B&)g) == &(A&)(B&)g);
// ... and from B to its A ...
static_assert(&dynamic_cast<A&>((B&)g) == &(A&)(B&)g);
// ... but not from D.
static_assert(&dynamic_cast<A&>((D&)g) == &(A&)(B&)g); // { dg-error "non-constant condition for static assertion" }
// { dg-message ".A. is an ambiguous base class of dynamic type .G." "" { target c++23_down } .-1 }
// { dg-error "reference .dynamic_cast. failed" "" { target c++23_down } .-2 }
// { dg-error "uncaught exception" "" { target c++26 } .-3 }
// Can cast from A2 to sibling class D.
static_assert(&dynamic_cast<D&>((A2&)(B&)g) == &(D&)g);

// Cannot cast from private base E to derived class F.
constexpr int e_f = (dynamic_cast<F&>((E&)g), 0); // { dg-error "reference .dynamic_cast. failed" "" { target c++23_down } }
// { dg-message "static type .const E. of its operand is a non-public base class of dynamic type .G." "" { target c++23_down } .-1 }
// { dg-error "uncaught exception" "" { target c++26 } .-2 }

// Cannot cast from B to private sibling E.
constexpr int b_e = (dynamic_cast<E&>((B&)g), 0); // { dg-error "reference .dynamic_cast. failed" "" { target c++23_down } }
// { dg-message "dynamic type .G. of its operand does not have an unambiguous public base class .E." "" { target c++23_down } .-1 }
// { dg-error "uncaught exception" "" { target c++26 } .-2 }

struct Unrelated { virtual void unrelated(); };

constexpr int b_unrelated = (dynamic_cast<Unrelated&>((B&)g), 0); // { dg-error "reference .dynamic_cast. failed" "" { target c++23_down } }
// { dg-message "dynamic type .G. of its operand does not have an unambiguous public base class .Unrelated." "" { target c++23_down } .-1 }
// { dg-error "uncaught exception" "" { target c++26 } .-2 }
constexpr int e_unrelated = (dynamic_cast<Unrelated&>((E&)g), 0); // { dg-error "reference .dynamic_cast. failed" "" { target c++23_down } }
// { dg-message "static type .const E. of its operand is a non-public base class of dynamic type .G." "" { target c++23_down } .-1 }
// { dg-error "uncaught exception" "" { target c++26 } .-2 }
