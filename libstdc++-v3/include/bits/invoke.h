// Implementation of INVOKE -*- C++ -*-

// Copyright (C) 2016-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file include/bits/invoke.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{functional}
 */

#ifndef _GLIBCXX_INVOKE_H
#define _GLIBCXX_INVOKE_H 1

#ifdef _GLIBCXX_SYSHDR
#pragma GCC system_header
#endif

#if __cplusplus < 201103L
# include <bits/c++0x_warning.h>
#else

#include <type_traits>
#include <bits/move.h> // forward

namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION

  /**
   *  @addtogroup utilities
   *  @{
   */

  // Used by __invoke_impl instead of std::forward<_Tp> so that a
  // reference_wrapper is converted to an lvalue-reference.
  template<typename _Tp, typename _Up = typename __inv_unwrap<_Tp>::type>
    constexpr _Up&&
    __invfwd(typename remove_reference<_Tp>::type& __t) noexcept
    { return static_cast<_Up&&>(__t); }

  template<typename _Res, typename _Fn, typename... _Args>
    constexpr _Res
    __invoke_impl(__invoke_other, _Fn&& __f, _Args&&... __args)
    { return std::forward<_Fn>(__f)(std::forward<_Args>(__args)...); }

  template<typename _Res, typename _MemFun, typename _Tp, typename... _Args>
    constexpr _Res
    __invoke_impl(__invoke_memfun_ref, _MemFun&& __f, _Tp&& __t,
		  _Args&&... __args)
    { return (__invfwd<_Tp>(__t).*__f)(std::forward<_Args>(__args)...); }

  template<typename _Res, typename _MemFun, typename _Tp, typename... _Args>
    constexpr _Res
    __invoke_impl(__invoke_memfun_deref, _MemFun&& __f, _Tp&& __t,
		  _Args&&... __args)
    {
      return ((*std::forward<_Tp>(__t)).*__f)(std::forward<_Args>(__args)...);
    }

  template<typename _Res, typename _MemPtr, typename _Tp>
    constexpr _Res
    __invoke_impl(__invoke_memobj_ref, _MemPtr&& __f, _Tp&& __t)
    { return __invfwd<_Tp>(__t).*__f; }

  template<typename _Res, typename _MemPtr, typename _Tp>
    constexpr _Res
    __invoke_impl(__invoke_memobj_deref, _MemPtr&& __f, _Tp&& __t)
    { return (*std::forward<_Tp>(__t)).*__f; }

  /// Invoke a callable object.
  template<typename _Callable, typename... _Args>
    constexpr typename __invoke_result<_Callable, _Args...>::type
    __invoke(_Callable&& __fn, _Args&&... __args)
    noexcept(__is_nothrow_invocable<_Callable, _Args...>::value)
    {
      using __result = __invoke_result<_Callable, _Args...>;
      using __type = typename __result::type;
      using __tag = typename __result::__invoke_type;
      return std::__invoke_impl<__type>(__tag{}, std::forward<_Callable>(__fn),
					std::forward<_Args>(__args)...);
    }

#if __cplusplus >= 201703L
  // INVOKE<R>: Invoke a callable object and convert the result to R.
  template<typename _Res, typename _Callable, typename... _Args>
    constexpr enable_if_t<is_invocable_r_v<_Res, _Callable, _Args...>, _Res>
    __invoke_r(_Callable&& __fn, _Args&&... __args)
    noexcept(is_nothrow_invocable_r_v<_Res, _Callable, _Args...>)
    {
      using __result = __invoke_result<_Callable, _Args...>;
      using __type = typename __result::type;
      using __tag = typename __result::__invoke_type;
      if constexpr (is_void_v<_Res>)
	std::__invoke_impl<__type>(__tag{}, std::forward<_Callable>(__fn),
					std::forward<_Args>(__args)...);
      else
	return std::__invoke_impl<__type>(__tag{},
					  std::forward<_Callable>(__fn),
					  std::forward<_Args>(__args)...);
    }
#else // C++11 or C++14
  // This is a non-SFINAE-friendly std::invoke_r<R>(fn, args...) for C++11/14.
  // It's used in std::function, std::bind, and std::packaged_task. Only
  // std::function is constrained on is_invocable_r, but that is checked on
  // construction so doesn't need to be checked again when calling __invoke_r.
  // Consequently, these __invoke_r overloads do not check for invocable
  // arguments, nor check that the invoke result is convertible to R.

  // INVOKE<R>: Invoke a callable object and convert the result to R.
  template<typename _Res, typename _Callable, typename... _Args>
    constexpr __enable_if_t<!is_void<_Res>::value, _Res>
    __invoke_r(_Callable&& __fn, _Args&&... __args)
    {
      using __result = __invoke_result<_Callable, _Args...>;
      using __type = typename __result::type;
#if __has_builtin(__reference_converts_from_temporary)
      static_assert(!__reference_converts_from_temporary(_Res, __type),
		    "INVOKE<R> must not create a dangling reference");
#endif
      using __tag = typename __result::__invoke_type;
      return std::__invoke_impl<__type>(__tag{}, std::forward<_Callable>(__fn),
					std::forward<_Args>(__args)...);
    }

  // INVOKE<R> when R is cv void
  template<typename _Res, typename _Callable, typename... _Args>
    _GLIBCXX14_CONSTEXPR __enable_if_t<is_void<_Res>::value, _Res>
    __invoke_r(_Callable&& __fn, _Args&&... __args)
    {
      using __result = __invoke_result<_Callable, _Args...>;
      using __type = typename __result::type;
      using __tag = typename __result::__invoke_type;
      std::__invoke_impl<__type>(__tag{}, std::forward<_Callable>(__fn),
				 std::forward<_Args>(__args)...);
    }
#endif // C++11 or C++14

_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std

#endif // C++11

#endif // _GLIBCXX_INVOKE_H
