/* dynamicstrings.h provides a minimal interface to a string library.

Copyright (C) 2012-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#if !defined(dynamicstrings_h)

#define dynamicstrings_h
#if defined(dynamicstrings_c)
#define EXTERN
#else /* !dynamicstrings_c.  */
#define EXTERN extern
#endif /* !dynamicstrings_c.  */

typedef void *dynamicstrings_string;

EXTERN dynamicstrings_string DynamicStrings_Mark (dynamicstrings_string s);
EXTERN dynamicstrings_string
DynamicStrings_InitStringCharStar (dynamicstrings_string s);

#undef EXTERN
#endif  /* !dynamicstrings_h.  */
