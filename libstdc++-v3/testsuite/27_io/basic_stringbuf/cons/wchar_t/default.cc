// Copyright (C) 2018-2025 Free Software Foundation, Inc.
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

// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// C++11 27.8.2.1  basic_stringbuf constructors  [stringbuf.cons]

// { dg-options "-O0" }
// { dg-do run { target c++11 } }

#include <sstream>
#include <testsuite_common_types.h>

void test01()
{
  // P0935R0
  __gnu_test::implicitly_default_constructible test;
  test.operator()<std::wstringbuf>();
}

void test02()
{
  // PR libstdc++/87618
  // Compiled without optimisation to check this constructor is exported.
  std::wstringbuf sb;
}

int main()
{
  test01();
  test02();
}
