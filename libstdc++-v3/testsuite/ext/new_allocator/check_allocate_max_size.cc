//
// Copyright (C) 2004-2025 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

// { dg-require-effective-target exceptions_enabled }

// 20.4.1.1 allocator members

#include <ext/new_allocator.h>
#include <testsuite_allocator.h>

int main()
{ 
  typedef int value_type;
  typedef __gnu_cxx::new_allocator<value_type> allocator_type;
  __gnu_test::check_allocate_max_size<allocator_type>();
  return 0;
}
