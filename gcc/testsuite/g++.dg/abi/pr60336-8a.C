// { dg-do compile }
// { dg-options "-O2 -Wabi=12" }

struct dummy { struct{} a[7][3]; };

extern void test1 (struct dummy, ...);
extern void (*test2) (struct dummy, ...);

void
foo ()
{
  struct dummy a0;
  test1 (a0, 42);
  test2 (a0, 42);
}
