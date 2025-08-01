/* Declarations of core diagnostic functionality for code that does
   not need to deal with diagnostic contexts or diagnostic info
   structures.  These functions implicitly use global_dc.
   Copyright (C) 1998-2025 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_DIAGNOSTIC_CORE_H
#define GCC_DIAGNOSTIC_CORE_H

#include "bversion.h"
#include "diagnostics/kinds.h"
#include "diagnostics/option-id.h"

/* RAII-style class for grouping related diagnostics within global_dc.  */

class auto_diagnostic_group
{
 public:
  auto_diagnostic_group ();
  ~auto_diagnostic_group ();
};

/* RAII-style class for nesting hierarchical diagnostics within global_dc.
   Any diagnostics emitted within the lifetime of this object
   will be treated as one level of nesting deeper than diagnostics
   emitted outside the lifetime of the object.  */

class auto_diagnostic_nesting_level
{
 public:
  auto_diagnostic_nesting_level ();
  ~auto_diagnostic_nesting_level ();
};

/* Forward decl.  */
namespace diagnostics {
   class metadata; /* See diagnostics/metadata.h.  */
} // namespace diagnostics

extern const char *progname;

extern const char *trim_filename (const char *);

/* Various functions for emitting diagnostics follow.
   All of these implicitly use global_dc.  */

/* If we haven't already defined a front-end-specific diagnostics
   style, use the generic one.  */
#ifndef GCC_DIAG_STYLE
#define GCC_DIAG_STYLE __gcc_tdiag__
#endif
/* None of these functions are suitable for ATTRIBUTE_PRINTF, because
   each language front end can extend them with its own set of format
   specifiers.  We must use custom format checks.  */
#if (CHECKING_P && GCC_VERSION >= 4001) || GCC_VERSION == BUILDING_GCC_VERSION
#define ATTRIBUTE_GCC_DIAG(m, n) __attribute__ ((__format__ (GCC_DIAG_STYLE, m, n))) ATTRIBUTE_NONNULL(m)
#else
#define ATTRIBUTE_GCC_DIAG(m, n) ATTRIBUTE_NONNULL(m)
#endif
extern void internal_error (const char *, ...) ATTRIBUTE_GCC_DIAG(1,2)
     ATTRIBUTE_NORETURN;
extern void internal_error_no_backtrace (const char *, ...)
     ATTRIBUTE_GCC_DIAG(1,2) ATTRIBUTE_NORETURN;
/* Pass one of the OPT_W* from options.h as the first parameter.  */
extern bool warning (diagnostics::option_id,
		     const char *, ...) ATTRIBUTE_GCC_DIAG(2,3);
extern bool warning_n (location_t,
		       diagnostics::option_id,
		       unsigned HOST_WIDE_INT,
		       const char *, const char *, ...)
    ATTRIBUTE_GCC_DIAG(4,6) ATTRIBUTE_GCC_DIAG(5,6);
extern bool warning_n (rich_location *,
		       diagnostics::option_id,
		       unsigned HOST_WIDE_INT,
		       const char *, const char *, ...)
    ATTRIBUTE_GCC_DIAG(4, 6) ATTRIBUTE_GCC_DIAG(5, 6);
extern bool warning_at (location_t,
			diagnostics::option_id,
			const char *, ...)
    ATTRIBUTE_GCC_DIAG(3,4);
extern bool warning_at (rich_location *,
			diagnostics::option_id,
			const char *, ...)
    ATTRIBUTE_GCC_DIAG(3,4);
extern bool warning_meta (rich_location *,
			  const diagnostics::metadata &,
			  diagnostics::option_id,
			  const char *, ...)
    ATTRIBUTE_GCC_DIAG(4,5);
extern void error (const char *, ...) ATTRIBUTE_GCC_DIAG(1,2);
extern void error_n (location_t, unsigned HOST_WIDE_INT, const char *,
		     const char *, ...)
    ATTRIBUTE_GCC_DIAG(3,5) ATTRIBUTE_GCC_DIAG(4,5);
extern void error_at (location_t, const char *, ...) ATTRIBUTE_GCC_DIAG(2,3);
extern void error_at (rich_location *, const char *, ...)
  ATTRIBUTE_GCC_DIAG(2,3);
extern void error_meta (rich_location *, const diagnostics::metadata &,
			const char *, ...)
  ATTRIBUTE_GCC_DIAG(3,4);
extern void fatal_error (location_t, const char *, ...) ATTRIBUTE_GCC_DIAG(2,3)
     ATTRIBUTE_NORETURN;
/* Pass one of the OPT_W* from options.h as the second parameter.  */
extern bool pedwarn (location_t,
		     diagnostics::option_id,
		     const char *, ...)
     ATTRIBUTE_GCC_DIAG(3,4);
extern bool pedwarn (rich_location *,
		     diagnostics::option_id,
		     const char *, ...)
     ATTRIBUTE_GCC_DIAG(3,4);
extern bool permerror (location_t, const char *, ...) ATTRIBUTE_GCC_DIAG(2,3);
extern bool permerror (rich_location *, const char *,
				   ...) ATTRIBUTE_GCC_DIAG(2,3);
extern bool permerror_opt (location_t,
			   diagnostics::option_id,
			   const char *, ...)
  ATTRIBUTE_GCC_DIAG(3,4);
extern bool permerror_opt (rich_location *,
			   diagnostics::option_id,
			   const char *, ...)
  ATTRIBUTE_GCC_DIAG(3,4);
extern void sorry (const char *, ...) ATTRIBUTE_GCC_DIAG(1,2);
extern void sorry_at (location_t, const char *, ...) ATTRIBUTE_GCC_DIAG(2,3);
extern void inform (location_t, const char *, ...) ATTRIBUTE_GCC_DIAG(2,3);
extern void inform (rich_location *, const char *, ...) ATTRIBUTE_GCC_DIAG(2,3);
extern void inform_n (location_t, unsigned HOST_WIDE_INT, const char *,
		      const char *, ...)
    ATTRIBUTE_GCC_DIAG(3,5) ATTRIBUTE_GCC_DIAG(4,5);
extern void verbatim (const char *, ...) ATTRIBUTE_GCC_DIAG(1,2);
extern bool emit_diagnostic (enum diagnostics::kind,
			     location_t,
			     diagnostics::option_id,
			     const char *, ...) ATTRIBUTE_GCC_DIAG(4,5);
extern bool emit_diagnostic (enum diagnostics::kind,
			     rich_location *,
			     diagnostics::option_id,
			     const char *, ...) ATTRIBUTE_GCC_DIAG(4,5);
extern bool emit_diagnostic_valist (enum diagnostics::kind,
				    location_t,
				    diagnostics::option_id,
				    const char *, va_list *)
  ATTRIBUTE_GCC_DIAG (4,0);
extern bool emit_diagnostic_valist_meta (enum diagnostics::kind,
					 rich_location *,
					 const diagnostics::metadata *,
					 diagnostics::option_id,
					 const char *,
					 va_list *) ATTRIBUTE_GCC_DIAG (5,0);
extern bool seen_error (void);

#ifdef BUFSIZ
  /* N.B. Unlike all the others, fnotice is just gettext+fprintf, and
     therefore it can have ATTRIBUTE_PRINTF.  */
extern void fnotice			(FILE *, const char *, ...)
     ATTRIBUTE_PRINTF_2;
#endif

#endif /* ! GCC_DIAGNOSTIC_CORE_H */
