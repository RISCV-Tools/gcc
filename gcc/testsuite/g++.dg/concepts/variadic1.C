// PR c++/66712
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template <class T, class...Args>
concept _Constructible_ =
  requires (Args&&...args)
  {
    T{ ((Args&&)(args))... };
  };

template <class T, class...Args>
constexpr bool _constructible_() { return false; }

template<typename T, typename... Args>
  requires _Constructible_<T, Args...>
constexpr bool _constructible_() { return true; }

struct S
{
  S(int, char const *);
};

int main()
{
  static_assert(_constructible_<S, int, char const *>(), "");
}
