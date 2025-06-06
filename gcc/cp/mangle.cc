/* Name mangling for the 3.0 -*- C++ -*- ABI.
   Copyright (C) 2000-2025 Free Software Foundation, Inc.
   Written by Alex Samuel <samuel@codesourcery.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This file implements mangling of C++ names according to the IA64
   C++ ABI specification.  A mangled name encodes a function or
   variable's name, scope, type, and/or template arguments into a text
   identifier.  This identifier is used as the function's or
   variable's linkage name, to preserve compatibility between C++'s
   language features (templates, scoping, and overloading) and C
   linkers.

   Additionally, g++ uses mangled names internally.  To support this,
   mangling of types is allowed, even though the mangled name of a
   type should not appear by itself as an exported name.  Ditto for
   uninstantiated templates.

   The primary entry point for this module is mangle_decl, which
   returns an identifier containing the mangled name for a decl.
   Additional entry points are provided to build mangled names of
   particular constructs when the appropriate decl for that construct
   is not available.  These are:

     mangle_typeinfo_for_type:		typeinfo data
     mangle_typeinfo_string_for_type:	typeinfo type name
     mangle_vtbl_for_type:		virtual table data
     mangle_vtt_for_type:		VTT data
     mangle_ctor_vtbl_for_type:		`C-in-B' constructor virtual table data
     mangle_thunk:			thunk function or entry  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "vtable-verify.h"
#include "cp-tree.h"
#include "stringpool.h"
#include "cgraph.h"
#include "stor-layout.h"
#include "flags.h"
#include "attribs.h"

/* Debugging support.  */

/* Define DEBUG_MANGLE to enable very verbose trace messages.  */
#ifndef DEBUG_MANGLE
#define DEBUG_MANGLE 0
#endif

/* Macros for tracing the write_* functions.  */
#if DEBUG_MANGLE
# define MANGLE_TRACE(FN, INPUT) \
  fprintf (stderr, "  %-24s: %-24s\n", (FN), (INPUT))
# define MANGLE_TRACE_TREE(FN, NODE) \
  fprintf (stderr, "  %-24s: %-24s (%p)\n", \
	   (FN), get_tree_code_name (TREE_CODE (NODE)), (void *) (NODE))
#else
# define MANGLE_TRACE(FN, INPUT)
# define MANGLE_TRACE_TREE(FN, NODE)
#endif

/* Nonzero if NODE is a class template-id.  We can't rely on
   CLASSTYPE_USE_TEMPLATE here because of tricky bugs in the parser
   that hard to distinguish A<T> from A, where A<T> is the type as
   instantiated outside of the template, and A is the type used
   without parameters inside the template.  */
#define CLASSTYPE_TEMPLATE_ID_P(NODE)					\
  (TREE_CODE (NODE) == BOUND_TEMPLATE_TEMPLATE_PARM			\
   || (CLASS_TYPE_P (NODE)						\
       && CLASSTYPE_TEMPLATE_INFO (NODE) != NULL			\
       && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (NODE))))

/* For deciding whether to set G.need_abi_warning, we need to consider both
   warn_abi_version and flag_abi_compat_version.  */
#define abi_warn_or_compat_version_crosses(N) \
  (abi_version_crosses (N) || abi_compat_version_crosses (N))

/* And sometimes we can simplify the code path if we don't need to worry about
   previous ABIs.  */
#define abi_flag_at_least(flag,N) (flag == 0 || flag >= N)
#define any_abi_below(N) \
  (!abi_version_at_least (N) \
   || !abi_flag_at_least (warn_abi_version, (N)) \
   || !abi_flag_at_least (flag_abi_compat_version, (N)))

/* Things we only need one of.  This module is not reentrant.  */
struct GTY(()) globals {
  /* An array of the current substitution candidates, in the order
     we've seen them.  Contains NULLS, which correspond to module
     substitutions.  */
  vec<tree, va_gc> *substitutions;

  /* The entity that is being mangled.  */
  tree GTY ((skip)) entity;

  /* How many parameter scopes we are inside.  */
  int parm_depth;

  /* True if the mangling will be different in a future version of the
     ABI.  */
  bool need_abi_warning;

  /* True if the mangling will be different in C++17 mode.  */
  bool need_cxx17_warning;

  /* True if we mangled a module name.  */
  bool mod;
};

static GTY (()) globals G;

/* The obstack on which we build mangled names.  */
static struct obstack *mangle_obstack;

/* The obstack on which we build mangled names that are not going to
   be IDENTIFIER_NODEs.  */
static struct obstack name_obstack;

/* The first object on the name_obstack; we use this to free memory
   allocated on the name_obstack.  */
static void *name_base;

/* Indices into subst_identifiers.  These are identifiers used in
   special substitution rules.  */
typedef enum
{
  SUBID_ALLOCATOR,
  SUBID_BASIC_STRING,
  SUBID_CHAR_TRAITS,
  SUBID_BASIC_ISTREAM,
  SUBID_BASIC_OSTREAM,
  SUBID_BASIC_IOSTREAM,
  SUBID_MAX
}
substitution_identifier_index_t;

/* For quick substitution checks, look up these common identifiers
   once only.  */
static GTY(()) tree subst_identifiers[SUBID_MAX];

/* Single-letter codes for builtin integer types, defined in
   <builtin-type>.  These are indexed by integer_type_kind values.  */
static const char
integer_type_codes[itk_none] =
{
  'c',  /* itk_char */
  'a',  /* itk_signed_char */
  'h',  /* itk_unsigned_char */
  's',  /* itk_short */
  't',  /* itk_unsigned_short */
  'i',  /* itk_int */
  'j',  /* itk_unsigned_int */
  'l',  /* itk_long */
  'm',  /* itk_unsigned_long */
  'x',  /* itk_long_long */
  'y',  /* itk_unsigned_long_long */
  /* __intN types are handled separately */
  '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0'
};

static tree maybe_template_info (const tree);

/* Functions for handling substitutions.  */

static inline tree canonicalize_for_substitution (tree);
static void add_substitution (tree);
static inline bool is_std_substitution (const tree,
				       const substitution_identifier_index_t);
static inline bool is_std_substitution_char (const tree,
					    const substitution_identifier_index_t);
static int find_substitution (tree);
static void mangle_call_offset (const tree, const tree);

/* Functions for emitting mangled representations of things.  */

static void write_mangled_name (const tree, bool);
static void write_encoding (const tree);
static void write_name (tree, const int);
static void write_abi_tags (tree);
static void write_unscoped_name (const tree);
static void write_unscoped_template_name (const tree);
static void write_nested_name (const tree);
static void write_prefix (const tree);
static void write_template_prefix (const tree);
static void write_unqualified_name (tree);
static void write_conversion_operator_name (const tree);
static void write_source_name (tree);
static void write_literal_operator_name (tree);
static void write_unnamed_type_name (const tree);
static void write_closure_type_name (const tree);
static int hwint_to_ascii (unsigned HOST_WIDE_INT, const unsigned int, char *,
			   const unsigned int);
static void write_number (unsigned HOST_WIDE_INT, const int,
			  const unsigned int);
static void write_compact_number (int num);
static void write_integer_cst (const tree);
static void write_real_cst (const tree);
static void write_identifier (const char *);
static void write_special_name_constructor (const tree);
static void write_special_name_destructor (const tree);
static void write_type (tree);
static int write_CV_qualifiers_for_type (const tree);
static void write_builtin_type (tree);
static void write_function_type (const tree);
static void write_bare_function_type (const tree, const int, const tree);
static void write_method_parms (tree, const int, const tree);
static void write_class_enum_type (const tree);
static void write_template_args (tree, tree = NULL_TREE);
static void write_expression (tree);
static void write_template_arg_literal (const tree);
static void write_template_arg (tree);
static void write_template_template_arg (const tree);
static void write_array_type (const tree);
static void write_pointer_to_member_type (const tree);
static void write_template_param (const tree);
static void write_template_template_param (const tree);
static void write_substitution (const int);
static int discriminator_for_local_entity (tree);
static int discriminator_for_string_literal (tree, tree);
static void write_discriminator (const int);
static void write_local_name (tree, const tree, const tree);
static void dump_substitution_candidates (void);
static tree mangle_decl_string (const tree);
static void maybe_check_abi_tags (tree, tree = NULL_TREE, int = 10);

/* Control functions.  */

static inline void start_mangling (const tree);
static tree mangle_special_for_type (const tree, const char *);

/* Append a single character to the end of the mangled
   representation.  */
#define write_char(CHAR)						\
  obstack_1grow (mangle_obstack, (CHAR))

/* Append a sized buffer to the end of the mangled representation.  */
#define write_chars(CHAR, LEN)						\
  obstack_grow (mangle_obstack, (CHAR), (LEN))

/* Append a NUL-terminated string to the end of the mangled
   representation.  */
#define write_string(STRING)						\
  obstack_grow (mangle_obstack, (STRING), strlen (STRING))

/* Nonzero if NODE1 and NODE2 are both TREE_LIST nodes and have the
   same purpose (context, which may be a type) and value (template
   decl).  See write_template_prefix for more information on what this
   is used for.  */
#define NESTED_TEMPLATE_MATCH(NODE1, NODE2)				\
  (TREE_CODE (NODE1) == TREE_LIST					\
   && TREE_CODE (NODE2) == TREE_LIST					\
   && ((TYPE_P (TREE_PURPOSE (NODE1))					\
	&& same_type_p (TREE_PURPOSE (NODE1), TREE_PURPOSE (NODE2)))	\
       || TREE_PURPOSE (NODE1) == TREE_PURPOSE (NODE2))			\
   && TREE_VALUE (NODE1) == TREE_VALUE (NODE2))

/* Write out an unsigned quantity in base 10.  */
#define write_unsigned_number(NUMBER)					\
  write_number ((NUMBER), /*unsigned_p=*/1, 10)

/* Check for -fabi-version dependent mangling and also set the need_abi_warning
   flag as appropriate.  */

static bool
abi_check (int ver)
{
  if (abi_warn_or_compat_version_crosses (ver))
    G.need_abi_warning = true;
  return abi_version_at_least (ver);
}

/* If DECL is a template instance (including the uninstantiated template
   itself), return its TEMPLATE_INFO.  Otherwise return NULL.  */

static tree
maybe_template_info (const tree decl)
{
  if (TREE_CODE (decl) == TYPE_DECL)
    {
      /* TYPE_DECLs are handled specially.  Look at its type to decide
	 if this is a template instantiation.  */
      const tree type = TREE_TYPE (decl);

      if (CLASS_TYPE_P (type) && CLASSTYPE_TEMPLATE_ID_P (type))
	return TYPE_TEMPLATE_INFO (type);
    }
  else
    {
      /* Check if the template is a primary template.  */
      if (DECL_LANG_SPECIFIC (decl) != NULL
	  && VAR_OR_FUNCTION_DECL_P (decl)
	  && DECL_TEMPLATE_INFO (decl)
	  && PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (decl)))
	return DECL_TEMPLATE_INFO (decl);
    }

  /* It's not a template id.  */
  return NULL_TREE;
}

/* Produce debugging output of current substitution candidates.  */

static void
dump_substitution_candidates (void)
{
  unsigned i;
  tree el;

  fprintf (stderr, "  ++ substitutions  ");
  FOR_EACH_VEC_ELT (*G.substitutions, i, el)
    {
      const char *name = "???";

      if (i > 0)
	fprintf (stderr, "                    ");
      if (!el)
	name = "module";
      else if (DECL_P (el))
	name = IDENTIFIER_POINTER (DECL_NAME (el));
      else if (TREE_CODE (el) == TREE_LIST)
	name = IDENTIFIER_POINTER (DECL_NAME (TREE_VALUE (el)));
      else if (TYPE_NAME (el))
	name = TYPE_NAME_STRING (el);
      fprintf (stderr, " S%d_ = ", i - 1);
      if (el)
	{
	  if (TYPE_P (el) &&
	      (CP_TYPE_RESTRICT_P (el)
	       || CP_TYPE_VOLATILE_P (el)
	       || CP_TYPE_CONST_P (el)))
	    fprintf (stderr, "CV-");
	  fprintf (stderr, "%s (%s at %p)",
		   name, get_tree_code_name (TREE_CODE (el)), (void *) el);
	}
      fprintf (stderr, "\n");
    }
}

/* <exception-spec> ::=
      Do  -- non-throwing exception specification
      DO <expression> E  -- computed (instantiation-dependent) noexcept
      Dw <type>* E  -- throw (types)  */

static void
write_exception_spec (tree spec)
{

  if (!spec || spec == noexcept_false_spec)
    /* Nothing.  */
    return;

  if (!flag_noexcept_type)
    {
      G.need_cxx17_warning = true;
      return;
    }

  if (spec == noexcept_true_spec || spec == empty_except_spec)
    write_string ("Do");
  else if (tree expr = TREE_PURPOSE (spec))
    {
      /* noexcept (expr)  */
      gcc_assert (uses_template_parms (expr));
      write_string ("DO");
      write_expression (expr);
      write_char ('E');
    }
  else
    {
      /* throw (type-list) */
      write_string ("Dw");
      for (tree t = spec; t; t = TREE_CHAIN (t))
	write_type (TREE_VALUE (t));
      write_char ('E');
    }
}

/* Both decls and types can be substitution candidates, but sometimes
   they refer to the same thing.  For instance, a TYPE_DECL and
   RECORD_TYPE for the same class refer to the same thing, and should
   be treated accordingly in substitutions.  This function returns a
   canonicalized tree node representing NODE that is used when adding
   and substitution candidates and finding matches.  */

static inline tree
canonicalize_for_substitution (tree node)
{
  /* For a TYPE_DECL, use the type instead.  */
  if (TREE_CODE (node) == TYPE_DECL)
    node = TREE_TYPE (node);
  if (TYPE_P (node)
      && TYPE_CANONICAL (node) != node
      && TYPE_MAIN_VARIANT (node) != node)
    {
      tree orig = node;
      /* Here we want to strip the topmost typedef only.
         We need to do that so is_std_substitution can do proper
         name matching.  */
      if (TREE_CODE (node) == FUNCTION_TYPE)
	/* Use build_qualified_type and TYPE_QUALS here to preserve
	   the old buggy mangling of attribute noreturn with abi<5.  */
	node = build_qualified_type (TYPE_MAIN_VARIANT (node),
				     TYPE_QUALS (node));
      else
	node = cp_build_qualified_type (TYPE_MAIN_VARIANT (node),
					cp_type_quals (node));
      if (FUNC_OR_METHOD_TYPE_P (node))
	{
	  node = build_ref_qualified_type (node, type_memfn_rqual (orig));
	  tree r = canonical_eh_spec (TYPE_RAISES_EXCEPTIONS (orig));
	  if (flag_noexcept_type)
	    node = build_exception_variant (node, r);
	  else
	    /* Set the warning flag if appropriate.  */
	    write_exception_spec (r);
	}
    }
  return node;
}

/* Add NODE as a substitution candidate.  NODE must not already be on
   the list of candidates.  */

static void
add_substitution (tree node)
{
  tree c;

  if (DEBUG_MANGLE)
    fprintf (stderr, "  ++ add_substitution (%s at %10p)\n",
	     get_tree_code_name (TREE_CODE (node)), (void *) node);

  /* Get the canonicalized substitution candidate for NODE.  */
  c = canonicalize_for_substitution (node);
  if (DEBUG_MANGLE && c != node)
    fprintf (stderr, "  ++ using candidate (%s at %10p)\n",
	     get_tree_code_name (TREE_CODE (node)), (void *) node);
  node = c;

  /* Make sure NODE isn't already a candidate.  */
  if (flag_checking)
    {
      int i;
      tree candidate;

      FOR_EACH_VEC_SAFE_ELT (G.substitutions, i, candidate)
	if (candidate)
	  {
	    gcc_assert (!(DECL_P (node) && node == candidate));
	    gcc_assert (!(TYPE_P (node) && TYPE_P (candidate)
			  && same_type_p (node, candidate)));
	  }
    }

  /* Put the decl onto the varray of substitution candidates.  */
  vec_safe_push (G.substitutions, node);

  if (DEBUG_MANGLE)
    dump_substitution_candidates ();
}

/* Helper function for find_substitution.  Returns nonzero if NODE,
   which may be a decl or a CLASS_TYPE, is a template-id with template
   name of substitution_index[INDEX] in the ::std namespace, with
   global module attachment.  */

static bool
is_std_substitution (const tree node,
		     const substitution_identifier_index_t index)
{
  tree type = NULL;
  tree decl = NULL;

  if (DECL_P (node))
    {
      type = TREE_TYPE (node);
      decl = node;
    }
  else if (CLASS_TYPE_P (node))
    {
      type = node;
      decl = TYPE_NAME (node);
    }
  else
    /* These are not the droids you're looking for.  */
    return false;

  if (!DECL_NAMESPACE_STD_P (CP_DECL_CONTEXT (decl)))
    return false;

  if (!(TYPE_LANG_SPECIFIC (type) && TYPE_TEMPLATE_INFO (type)))
    return false;

  tree tmpl = TYPE_TI_TEMPLATE (type);
  if (DECL_NAME (tmpl) != subst_identifiers[index])
    return false;

  if (modules_p () && get_originating_module (tmpl, true) >= 0)
    return false;

  return true;
}

/* Return the ABI tags (the TREE_VALUE of the "abi_tag" attribute entry) for T,
   which can be a decl or type.  */

static tree
get_abi_tags (tree t)
{
  if (!t || TREE_CODE (t) == NAMESPACE_DECL)
    return NULL_TREE;

  if (DECL_P (t) && DECL_DECLARES_TYPE_P (t))
    t = TREE_TYPE (t);

  if (TREE_CODE (t) == TEMPLATE_DECL && DECL_TEMPLATE_RESULT (t))
    {
      tree tags = get_abi_tags (DECL_TEMPLATE_RESULT (t));
      /* We used to overlook abi_tag on function and variable templates.  */
      if (tags && abi_check (19))
	return tags;
      else
	return NULL_TREE;
    }

  tree attrs;
  if (TYPE_P (t))
    attrs = TYPE_ATTRIBUTES (t);
  else
    attrs = DECL_ATTRIBUTES (t);

  tree tags = lookup_attribute ("abi_tag", attrs);
  if (tags)
    tags = TREE_VALUE (tags);
  return tags;
}

/* Helper function for find_substitution.  Returns nonzero if NODE,
   which may be a decl or a CLASS_TYPE, is the template-id
   ::std::identifier<char>, where identifier is
   substitution_index[INDEX].  */

static bool
is_std_substitution_char (const tree node,
			  const substitution_identifier_index_t index)
{
  tree args;
  /* Check NODE's name is ::std::identifier.  */
  if (!is_std_substitution (node, index))
    return 0;
  /* Figure out its template args.  */
  if (DECL_P (node))
    args = DECL_TI_ARGS (node);
  else if (CLASS_TYPE_P (node))
    args = CLASSTYPE_TI_ARGS (node);
  else
    /* Oops, not a template.  */
    return 0;
  /* NODE's template arg list should be <char>.  */
  return
    TREE_VEC_LENGTH (args) == 1
    && TREE_VEC_ELT (args, 0) == char_type_node;
}

/* Check whether a substitution should be used to represent NODE in
   the mangling.

   First, check standard special-case substitutions.

     <substitution> ::= St
	 # ::std

		    ::= Sa
	 # ::std::allocator

		    ::= Sb
	 # ::std::basic_string

		    ::= Ss
	 # ::std::basic_string<char,
			       ::std::char_traits<char>,
			       ::std::allocator<char> >

		    ::= Si
	 # ::std::basic_istream<char, ::std::char_traits<char> >

		    ::= So
	 # ::std::basic_ostream<char, ::std::char_traits<char> >

		    ::= Sd
	 # ::std::basic_iostream<char, ::std::char_traits<char> >

   Then examine the stack of currently available substitution
   candidates for entities appearing earlier in the same mangling

   If a substitution is found, write its mangled representation and
   return nonzero.  If none is found, just return zero.  */

static int
find_substitution (tree node)
{
  int i;
  const int size = vec_safe_length (G.substitutions);
  tree decl;
  tree type;
  const char *abbr = NULL;

  if (DEBUG_MANGLE)
    fprintf (stderr, "  ++ find_substitution (%s at %p)\n",
	     get_tree_code_name (TREE_CODE (node)), (void *) node);

  /* Obtain the canonicalized substitution representation for NODE.
     This is what we'll compare against.  */
  node = canonicalize_for_substitution (node);

  /* Check for builtin substitutions.  */

  decl = TYPE_P (node) ? TYPE_NAME (node) : node;
  type = TYPE_P (node) ? node : TREE_TYPE (node);

  /* Check for std::allocator.  */
  if (decl
      && is_std_substitution (decl, SUBID_ALLOCATOR)
      && !CLASSTYPE_USE_TEMPLATE (TREE_TYPE (decl)))
    abbr = "Sa";

  /* Check for std::basic_string.  */
  else if (decl && is_std_substitution (decl, SUBID_BASIC_STRING))
    {
      if (TYPE_P (node))
	{
	  /* If this is a type (i.e. a fully-qualified template-id),
	     check for
		 std::basic_string <char,
				    std::char_traits<char>,
				    std::allocator<char> > .  */
	  if (cp_type_quals (type) == TYPE_UNQUALIFIED
	      && CLASSTYPE_USE_TEMPLATE (type))
	    {
	      tree args = CLASSTYPE_TI_ARGS (type);
	      if (TREE_VEC_LENGTH (args) == 3
		  && template_args_equal (TREE_VEC_ELT (args, 0), char_type_node)
		  && is_std_substitution_char (TREE_VEC_ELT (args, 1),
					       SUBID_CHAR_TRAITS)
		  && is_std_substitution_char (TREE_VEC_ELT (args, 2),
					       SUBID_ALLOCATOR))
		abbr = "Ss";
	    }
	}
      else
	/* Substitute for the template name only if this isn't a type.  */
	abbr = "Sb";
    }

  /* Check for basic_{i,o,io}stream.  */
  else if (TYPE_P (node)
	   && cp_type_quals (type) == TYPE_UNQUALIFIED
	   && CLASS_TYPE_P (type)
	   && CLASSTYPE_USE_TEMPLATE (type)
	   && CLASSTYPE_TEMPLATE_INFO (type) != NULL)
    {
      /* First, check for the template
	 args <char, std::char_traits<char> > .  */
      tree args = CLASSTYPE_TI_ARGS (type);
      if (TREE_VEC_LENGTH (args) == 2
	  && template_args_equal (TREE_VEC_ELT (args, 0), char_type_node)
	  && is_std_substitution_char (TREE_VEC_ELT (args, 1),
				       SUBID_CHAR_TRAITS))
	{
	  /* Got them.  Is this basic_istream?  */
	  if (is_std_substitution (decl, SUBID_BASIC_ISTREAM))
	    abbr = "Si";
	  /* Or basic_ostream?  */
	  else if (is_std_substitution (decl, SUBID_BASIC_OSTREAM))
	    abbr = "So";
	  /* Or basic_iostream?  */
	  else if (is_std_substitution (decl, SUBID_BASIC_IOSTREAM))
	    abbr = "Sd";
	}
    }

  /* Check for namespace std.  */
  else if (decl && DECL_NAMESPACE_STD_P (decl))
    {
      write_string ("St");
      return 1;
    }

  tree tags = NULL_TREE;
  if (OVERLOAD_TYPE_P (node) || DECL_CLASS_TEMPLATE_P (node))
    tags = get_abi_tags (type);
  /* Now check the list of available substitutions for this mangling
     operation.  */
  if (!abbr || tags)
    for (i = 0; i < size; ++i)
      if (tree candidate = (*G.substitutions)[i])
	{
	  /* NODE is a matched to a candidate if it's the same decl node or
	     if it's the same type.  */
	  if (decl == candidate
	      || (TYPE_P (candidate) && type && TYPE_P (node)
		  && same_type_p (type, candidate))
	      || NESTED_TEMPLATE_MATCH (node, candidate))
	    {
	      write_substitution (i);
	      return 1;
	    }
	}

  if (!abbr)
    /* No substitution found.  */
    return 0;

  write_string (abbr);
  if (tags)
    {
      /* If there are ABI tags on the abbreviation, it becomes
	 a substitution candidate.  */
      write_abi_tags (tags);
      add_substitution (node);
    }
  return 1;
}

/* Returns whether DECL's symbol name should be the plain unqualified-id
   rather than a more complicated mangled name.  */

static bool
unmangled_name_p (const tree decl)
{
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* The names of `extern "C"' functions are not mangled.  */
      return (DECL_EXTERN_C_FUNCTION_P (decl)
	      /* But overloaded operator names *are* mangled.  */
	      && !DECL_OVERLOADED_OPERATOR_P (decl));
    }
  else if (VAR_P (decl))
    {
      /* static variables are mangled.  */
      if (!DECL_EXTERNAL_LINKAGE_P (decl))
	return false;

      /* extern "C" declarations aren't mangled.  */
      if (DECL_EXTERN_C_P (decl))
	return true;

      /* Other variables at non-global scope are mangled.  */
      if (CP_DECL_CONTEXT (decl) != global_namespace)
	return false;

      /* Variable template instantiations are mangled.  */
      if (DECL_LANG_SPECIFIC (decl) && DECL_TEMPLATE_INFO (decl)
	  && variable_template_p (DECL_TI_TEMPLATE (decl)))
	return false;

      /* Declarations with ABI tags are mangled.  */
      if (get_abi_tags (decl))
	return false;

      // Declarations attached to a named module are mangled
      if (modules_p () && get_originating_module (decl, true) >= 0)
	return false;

      /* The names of non-static global variables aren't mangled.  */
      return true;
    }

  return false;
}

/* TOP_LEVEL is true, if this is being called at outermost level of
  mangling. It should be false when mangling a decl appearing in an
  expression within some other mangling.

  <mangled-name>      ::= _Z <encoding>  */

static void
write_mangled_name (const tree decl, bool top_level)
{
  MANGLE_TRACE_TREE ("mangled-name", decl);

  check_abi_tags (decl);

  if (unmangled_name_p (decl))
    {
      if (top_level)
	write_string (IDENTIFIER_POINTER (DECL_NAME (decl)));
      else
	{
	  /* The standard notes: "The <encoding> of an extern "C"
	     function is treated like global-scope data, i.e. as its
	     <source-name> without a type."  We cannot write
	     overloaded operators that way though, because it contains
	     characters invalid in assembler.  */
	  write_string ("_Z");
	  write_source_name (DECL_NAME (decl));
	}
    }
  else
    {
      write_string ("_Z");
      write_encoding (decl);
    }

  /* If this is the pre/post function for a guarded function, append
     .pre/post, like something from create_virtual_clone.  */
  if (DECL_IS_PRE_FN_P (decl))
    write_string (".pre");
  else if (DECL_IS_POST_FN_P (decl))
    write_string (".post");

  /* If this is a coroutine helper, then append an appropriate string to
     identify which.  */
  if (tree ramp = DECL_RAMP_FN (decl))
    {
      if (DECL_ACTOR_FN (ramp) == decl)
	write_string (JOIN_STR "actor");
      else if (DECL_DESTROY_FN (ramp) == decl)
	write_string (JOIN_STR "destroy");
      else
	gcc_unreachable ();
    }
}

/* Returns true if the return type of DECL is part of its signature, and
   therefore its mangling.  */

bool
mangle_return_type_p (tree decl)
{
  return (!DECL_CONSTRUCTOR_P (decl)
	  && !DECL_DESTRUCTOR_P (decl)
	  && !DECL_CONV_FN_P (decl)
	  && maybe_template_info (decl));
}

/* <constraint-expression> ::= <expression> */

static void
write_constraint_expression (tree expr)
{
  write_expression (expr);
}

/* Mangle a requires-clause following a template-head, if any.

   Q <constraint_expression> E  */

static void
write_tparms_constraints (tree constraints)
{
  /* In a declaration with shorthand constraints in the template-head, followed
     by a requires-clause, followed by shorthand constraints in the
     function-parameter-list, the full constraints will be some && with the
     parameter constraints on the RHS, around an && with the requires-clause on
     the RHS.  Find the requires-clause, if any.

     This logic relies on the && and ... from combine_constraint_expressions,
     finish_shorthand_constraint, and convert_generic_types_to_packs having
     UNKNOWN_LOCATION.  If they need to have an actual location, we could move
     to using a TREE_LANG_FLAG.  */
  if (constraints && abi_check (19))
    {
      tree probe = constraints;
      while (probe
	     && !EXPR_LOCATION (probe)
	     && TREE_CODE (probe) == TRUTH_ANDIF_EXPR)
	{
	  tree op1 = TREE_OPERAND (probe, 1);
	  probe = (EXPR_LOCATION (op1) ? op1
		   : TREE_OPERAND (probe, 0));
	}
      if (probe && EXPR_LOCATION (probe))
	{
	  write_char ('Q');
	  write_constraint_expression (probe);
	}
    }
}

/* <type-constraint> ::= <name> */

static void
write_type_constraint (tree cnst)
{
  if (!cnst)
    return;

  gcc_checking_assert (TREE_CODE (cnst) == TEMPLATE_ID_EXPR);

  tree concept_decl = get_concept_check_template (cnst);
  write_name (concept_decl, 0);
  tree args = TREE_OPERAND (cnst, 1);
  if (TREE_VEC_LENGTH (args) > 1)
    {
      TEMPLATE_ARGS_TYPE_CONSTRAINT_P (args) = true;
      write_template_args (args);
    }
}

/*   <encoding>		::= <function name> <bare-function-type>
			::= <data name>  */

static void
write_encoding (const tree decl)
{
  MANGLE_TRACE_TREE ("encoding", decl);

  if (DECL_LANG_SPECIFIC (decl) && DECL_EXTERN_C_FUNCTION_P (decl))
    {
      /* For overloaded operators write just the mangled name
	 without arguments.  */
      if (DECL_OVERLOADED_OPERATOR_P (decl))
	write_name (decl, /*ignore_local_scope=*/0);
      else
	write_source_name (DECL_NAME (decl));
      return;
    }

  write_name (decl, /*ignore_local_scope=*/0);
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      tree fn_type;
      tree d;

      if (maybe_template_info (decl))
	{
	  fn_type = get_mostly_instantiated_function_type (decl);
	  /* FN_TYPE will not have parameter types for in-charge or
	     VTT parameters.  Therefore, we pass NULL_TREE to
	     write_bare_function_type -- otherwise, it will get
	     confused about which artificial parameters to skip.  */
	  d = NULL_TREE;
	}
      else
	{
	  fn_type = TREE_TYPE (decl);
	  d = decl;
	}

      write_bare_function_type (fn_type,
				mangle_return_type_p (decl),
				d);

      if (tree c = get_trailing_function_requirements (decl))
	if (abi_check (19))
	  {
	    ++G.parm_depth;
	    write_char ('Q');
	    write_constraint_expression (c);
	    --G.parm_depth;
	  }
    }
}

/* Interface to substitution and identifier mangling, used by the
   module name mangler.  */

void
mangle_module_substitution (int v)
{
  write_substitution (v - 1);
}

int
mangle_module_component (tree comp, bool partition_p)
{
  write_char ('W');
  if (partition_p)
    write_char ('P');
  write_source_name (comp);

  // Module substitutions use the same number-space as entity
  // substitutions, but are orthogonal.
  vec_safe_push (G.substitutions, NULL_TREE);
  return G.substitutions->length ();
}

/* If the outermost non-namespace context (including DECL itself) is
   a module-linkage decl, mangle the module information.  For module
   global initializers we need to include the partition part.

   <module-name> ::= <module-sub>
		 || <subst>
                 || <module-name> <module-sub>
   <module-sub> :: W [P] <unqualified-name>
*/

static void
write_module (int m, bool include_partition)
{
  G.mod = true;
  mangle_module (m, include_partition);
}

static void
maybe_write_module (tree decl)
{
  if (!DECL_NAMESPACE_SCOPE_P (decl))
    return;

  if (!TREE_PUBLIC (STRIP_TEMPLATE (decl)))
    return;

  if (TREE_CODE (decl) == NAMESPACE_DECL)
    return;

  int m = get_originating_module (decl, true);
  if (m >= 0)
    write_module (m, false);
}

/* Lambdas can have a bit more context for mangling, specifically VAR_DECL
   or PARM_DECL context, which doesn't belong in DECL_CONTEXT.  */

static tree
decl_mangling_context (tree decl)
{
  tree tcontext = targetm.cxx.decl_mangling_context (decl);

  if (tcontext != NULL_TREE)
    return tcontext;

  if (TREE_CODE (decl) == TEMPLATE_DECL
      && DECL_TEMPLATE_RESULT (decl))
    decl = DECL_TEMPLATE_RESULT (decl);

  if (TREE_CODE (decl) == TYPE_DECL
      && LAMBDA_TYPE_P (TREE_TYPE (decl)))
    {
      tree extra = LAMBDA_TYPE_EXTRA_SCOPE (TREE_TYPE (decl));
      if (extra)
	return extra;
      tcontext = CP_DECL_CONTEXT (decl);
      if (LAMBDA_TYPE_P (tcontext))
	/* Lambda type context means this lambda appears between the
	   lambda-introducer and the open brace of another lambda (c++/119175).
	   That isn't a real scope; look further into the enclosing scope.  */
	return decl_mangling_context (TYPE_NAME (tcontext));
    }
  else if (template_type_parameter_p (decl))
     /* template type parms have no mangling context.  */
      return NULL_TREE;

  tcontext = CP_DECL_CONTEXT (decl);

  if (member_like_constrained_friend_p (decl))
    tcontext = DECL_FRIEND_CONTEXT (decl);

  /* Ignore the artificial declare reduction functions.  */
  if (tcontext
      && TREE_CODE (tcontext) == FUNCTION_DECL
      && DECL_OMP_DECLARE_REDUCTION_P (tcontext))
    return decl_mangling_context (tcontext);

  return tcontext;
}

/* <name> ::= <unscoped-name>
	  ::= <unscoped-template-name> <template-args>
	  ::= <nested-name>
	  ::= <local-name>

   If IGNORE_LOCAL_SCOPE is nonzero, this production of <name> is
   called from <local-name>, which mangles the enclosing scope
   elsewhere and then uses this function to mangle just the part
   underneath the function scope.  So don't use the <local-name>
   production, to avoid an infinite recursion.  */

static void
write_name (tree decl, const int ignore_local_scope)
{
  tree context;

  MANGLE_TRACE_TREE ("name", decl);

  if (TREE_CODE (decl) == TYPE_DECL)
    {
      /* In case this is a typedef, fish out the corresponding
	 TYPE_DECL for the main variant.  */
      decl = TYPE_NAME (TYPE_MAIN_VARIANT (TREE_TYPE (decl)));
    }

  context = decl_mangling_context (decl);

  gcc_assert (context != NULL_TREE);

  if (abi_warn_or_compat_version_crosses (7)
      && ignore_local_scope
      && TREE_CODE (context) == PARM_DECL)
    G.need_abi_warning = 1;

  /* A decl in :: or ::std scope is treated specially.  The former is
     mangled using <unscoped-name> or <unscoped-template-name>, the
     latter with a special substitution.  Also, a name that is
     directly in a local function scope is also mangled with
     <unscoped-name> rather than a full <nested-name>.  */
  if (context == global_namespace
      || DECL_NAMESPACE_STD_P (context)
      || (ignore_local_scope
	  && (TREE_CODE (context) == FUNCTION_DECL
	      || (abi_version_at_least (7)
		  && TREE_CODE (context) == PARM_DECL))))
    {
      /* Is this a template instance?  */
      if (tree info = maybe_template_info (decl))
	{
	  /* Yes: use <unscoped-template-name>.  */
	  write_unscoped_template_name (TI_TEMPLATE (info));
	  /* Pass down the parms of a function template in case we need to
	     mangle them; we don't mangle the parms of a non-overloadable
	     template.  */
	  tree parms = (TREE_CODE (decl) == FUNCTION_DECL
			? DECL_TEMPLATE_PARMS (TI_TEMPLATE (info))
			: NULL_TREE);
	  write_template_args (TI_ARGS (info), parms);
	}
      else
	/* Everything else gets an <unqualified-name>.  */
	write_unscoped_name (decl);
    }
  else
    {
      /* Handle local names, unless we asked not to (that is, invoked
	 under <local-name>, to handle only the part of the name under
	 the local scope).  */
      if (!ignore_local_scope)
	{
	  /* Scan up the list of scope context, looking for a
	     function.  If we find one, this entity is in local
	     function scope.  local_entity tracks context one scope
	     level down, so it will contain the element that's
	     directly in that function's scope, either decl or one of
	     its enclosing scopes.  */
	  tree local_entity = decl;
	  while (context != global_namespace)
	    {
	      /* Make sure we're always dealing with decls.  */
	      if (TYPE_P (context))
		context = TYPE_NAME (context);
	      /* Is this a function?  */
	      if (TREE_CODE (context) == FUNCTION_DECL
		  || TREE_CODE (context) == PARM_DECL)
		{
		  /* Yes, we have local scope.  Use the <local-name>
		     production for the innermost function scope.  */
		  write_local_name (context, local_entity, decl);
		  return;
		}
	      /* Up one scope level.  */
	      local_entity = context;
	      context = decl_mangling_context (context);
	    }

	  /* No local scope found?  Fall through to <nested-name>.  */
	}

      /* Other decls get a <nested-name> to encode their scope.  */
      write_nested_name (decl);
    }
}

/* <unscoped-name> ::= <unqualified-name>
		   ::= St <unqualified-name>   # ::std::  */

static void
write_unscoped_name (const tree decl)
{
  tree context = decl_mangling_context (decl);

  MANGLE_TRACE_TREE ("unscoped-name", decl);

  /* Is DECL in ::std?  */
  if (DECL_NAMESPACE_STD_P (context))
    {
      write_string ("St");
      write_unqualified_name (decl);
    }
  else
    {
      /* If not, it should be either in the global namespace, or directly
	 in a local function scope.  A lambda can also be mangled in the
	 scope of a default argument.  */
      gcc_assert (context == global_namespace
		  || TREE_CODE (context) == PARM_DECL
		  || TREE_CODE (context) == FUNCTION_DECL);

      write_unqualified_name (decl);
    }
}

/* <unscoped-template-name> ::= <unscoped-name>
			    ::= <substitution>  */

static void
write_unscoped_template_name (const tree decl)
{
  MANGLE_TRACE_TREE ("unscoped-template-name", decl);

  if (find_substitution (decl))
    return;
  write_unscoped_name (decl);
  add_substitution (decl);
}

/* Write the nested name, including CV-qualifiers, of DECL.

   <nested-name> ::= N [<CV-qualifiers>] [<ref-qualifier>] <prefix> <unqualified-name> E
		 ::= N [<CV-qualifiers>] [<ref-qualifier>] <template-prefix> <template-args> E

   <ref-qualifier> ::= R # & ref-qualifier
                   ::= O # && ref-qualifier
   <CV-qualifiers> ::= [r] [V] [K]  */

static void
write_nested_name (const tree decl)
{
  MANGLE_TRACE_TREE ("nested-name", decl);

  write_char ('N');

  /* Write CV-qualifiers, if this is an iobj member function.  */
  if (TREE_CODE (decl) == FUNCTION_DECL
      && DECL_IOBJ_MEMBER_FUNCTION_P (decl))
    {
      if (DECL_VOLATILE_MEMFUNC_P (decl))
	write_char ('V');
      if (DECL_CONST_MEMFUNC_P (decl))
	write_char ('K');
      if (FUNCTION_REF_QUALIFIED (TREE_TYPE (decl)))
	{
	  if (FUNCTION_RVALUE_QUALIFIED (TREE_TYPE (decl)))
	    write_char ('O');
	  else
	    write_char ('R');
	}
    }
  else if (DECL_DECLARES_FUNCTION_P (decl)
	   && DECL_XOBJ_MEMBER_FUNCTION_P (decl))
    write_char ('H');

  /* Is this a template instance?  */
  if (tree info = maybe_template_info (decl))
    {
      /* Yes, use <template-prefix>.  */
      write_template_prefix (decl);
      write_template_args (TI_ARGS (info));
    }
  else if ((!abi_version_at_least (10) || TREE_CODE (decl) == TYPE_DECL)
	   && TREE_CODE (TREE_TYPE (decl)) == TYPENAME_TYPE)
    {
      tree name = TYPENAME_TYPE_FULLNAME (TREE_TYPE (decl));
      if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
	{
	  write_template_prefix (decl);
	  write_template_args (TREE_OPERAND (name, 1));
	}
      else
	{
	  write_prefix (decl_mangling_context (decl));
	  write_unqualified_name (decl);
	}
    }
  else
    {
      /* No, just use <prefix>  */
      write_prefix (decl_mangling_context (decl));
      write_unqualified_name (decl);
    }
  write_char ('E');
}

/* <prefix> ::= <prefix> <unqualified-name>
	    ::= <template-param>
	    ::= <template-prefix> <template-args>
	    ::= <decltype>
	    ::= # empty
	    ::= <substitution>  */

static void
write_prefix (const tree node)
{
  tree decl;

  if (node == NULL
      || node == global_namespace)
    return;

  MANGLE_TRACE_TREE ("prefix", node);

  if (TREE_CODE (node) == DECLTYPE_TYPE
      || TREE_CODE (node) == TRAIT_TYPE)
    {
      write_type (node);
      return;
    }

  if (find_substitution (node))
    return;

  tree template_info = NULL_TREE;
  if (DECL_P (node))
    {
      /* If this is a function or parm decl, that means we've hit function
	 scope, so this prefix must be for a local name.  In this
	 case, we're under the <local-name> production, which encodes
	 the enclosing function scope elsewhere.  So don't continue
	 here.  */
      if (TREE_CODE (node) == FUNCTION_DECL
	  || TREE_CODE (node) == PARM_DECL)
	return;

      decl = node;
      template_info = maybe_template_info (decl);
    }
  else
    {
      /* Node is a type.  */
      decl = TYPE_NAME (node);
      /* The DECL might not point at the node.  */
      if (CLASSTYPE_TEMPLATE_ID_P (node))
	template_info = TYPE_TEMPLATE_INFO (node);
    }

  if (TREE_CODE (node) == TEMPLATE_TYPE_PARM)
    write_template_param (node);
  else if (template_info)
    /* Templated.  */
    {
      write_template_prefix (decl);
      write_template_args (TI_ARGS (template_info));
    }
  else if (TREE_CODE (TREE_TYPE (decl)) == TYPENAME_TYPE)
    {
      tree name = TYPENAME_TYPE_FULLNAME (TREE_TYPE (decl));
      if (TREE_CODE (name) == TEMPLATE_ID_EXPR)
	{
	  write_template_prefix (decl);
	  write_template_args (TREE_OPERAND (name, 1));
	}
      else
	{
	  write_prefix (decl_mangling_context (decl));
	  write_unqualified_name (decl);
	}
    }
  else
    /* Not templated.  */
    {
      write_prefix (decl_mangling_context (decl));
      write_unqualified_name (decl);
      if (VAR_P (decl)
	  || TREE_CODE (decl) == FIELD_DECL)
	{
	  /* <data-member-prefix> := <member source-name> M */
	  write_char ('M');

	  /* Before ABI 18, we did not count these as substitution
	     candidates.  This leads to incorrect demanglings (and
	     ABI divergence to other compilers).  */
	  if (!abi_check (18))
	    return;
	}
    }

  add_substitution (node);
}

/* <template-prefix> ::= <prefix> <template component>
		     ::= <template-param>
		     ::= <substitution>  */

static void
write_template_prefix (const tree node)
{
  tree decl = DECL_P (node) ? node : TYPE_NAME (node);
  tree type = DECL_P (node) ? TREE_TYPE (node) : node;
  tree context = decl_mangling_context (decl);
  tree templ;
  tree substitution;

  MANGLE_TRACE_TREE ("template-prefix", node);

  /* Find the template decl.  */
  if (tree info = maybe_template_info (decl))
    templ = TI_TEMPLATE (info);
  else if (TREE_CODE (type) == TYPENAME_TYPE)
    /* For a typename type, all we have is the name.  */
    templ = DECL_NAME (decl);
  else
    {
      gcc_assert (CLASSTYPE_TEMPLATE_ID_P (type));

      templ = TYPE_TI_TEMPLATE (type);
    }

  /* For a member template, though, the template name for the
     innermost name must have all the outer template levels
     instantiated.  For instance, consider

       template<typename T> struct Outer {
	 template<typename U> struct Inner {};
       };

     The template name for `Inner' in `Outer<int>::Inner<float>' is
     `Outer<int>::Inner<U>'.  In g++, we don't instantiate the template
     levels separately, so there's no TEMPLATE_DECL available for this
     (there's only `Outer<T>::Inner<U>').

     In order to get the substitutions right, we create a special
     TREE_LIST to represent the substitution candidate for a nested
     template.  The TREE_PURPOSE is the template's context, fully
     instantiated, and the TREE_VALUE is the TEMPLATE_DECL for the inner
     template.

     So, for the example above, `Outer<int>::Inner' is represented as a
     substitution candidate by a TREE_LIST whose purpose is `Outer<int>'
     and whose value is `Outer<T>::Inner<U>'.  */
  if (context && TYPE_P (context))
    substitution = build_tree_list (context, templ);
  else
    substitution = templ;

  if (find_substitution (substitution))
    return;

  if (TREE_TYPE (templ)
      && TREE_CODE (TREE_TYPE (templ)) == TEMPLATE_TEMPLATE_PARM)
    write_template_param (TREE_TYPE (templ));
  else
    {
      write_prefix (context);
      write_unqualified_name (decl);
    }

  add_substitution (substitution);
}

/* "For the purposes of mangling, the name of an anonymous union is considered
   to be the name of the first named data member found by a pre-order,
   depth-first, declaration-order walk of the data members of the anonymous
   union. If there is no such data member (i.e., if all of the data members in
   the union are unnamed), then there is no way for a program to refer to the
   anonymous union, and there is therefore no need to mangle its name."  */

static tree
anon_aggr_naming_decl (tree type)
{
  tree field = next_aggregate_field (TYPE_FIELDS (type));
  for (; field; field = next_aggregate_field (DECL_CHAIN (field)))
    {
      if (DECL_NAME (field))
	return field;
      if (ANON_AGGR_TYPE_P (TREE_TYPE (field)))
	if (tree sub = anon_aggr_naming_decl (TREE_TYPE (field)))
	  return sub;
    }
  return NULL_TREE;
}

/* We don't need to handle thunks, vtables, or VTTs here.  Those are
   mangled through special entry points.

    <unqualified-name>  ::= [<module-name>] <operator-name>
			::= <special-name>
			::= [<module-name>] <source-name>
			::= [<module-name>] <unnamed-type-name>
			::= <local-source-name>
			::= F <source-name> # member-like constrained friend

    <local-source-name>	::= L <source-name> <discriminator> */

static void
write_unqualified_id (tree identifier)
{
  if (IDENTIFIER_CONV_OP_P (identifier))
    write_conversion_operator_name (TREE_TYPE (identifier));
  else if (IDENTIFIER_OVL_OP_P (identifier))
    {
      const ovl_op_info_t *ovl_op = IDENTIFIER_OVL_OP_INFO (identifier);
      write_string (ovl_op->mangled_name);
    }
  else if (UDLIT_OPER_P (identifier))
    write_literal_operator_name (identifier);
  else
    write_source_name (identifier);
}

static void
write_unqualified_name (tree decl)
{
  MANGLE_TRACE_TREE ("unqualified-name", decl);

  if (modules_p ())
    maybe_write_module (decl);

  if (identifier_p (decl))
    {
      write_unqualified_id (decl);
      return;
    }

  bool found = false;

  if (DECL_NAME (decl) == NULL_TREE
      && ANON_AGGR_TYPE_P (TREE_TYPE (decl)))
    decl = anon_aggr_naming_decl (TREE_TYPE (decl));
  else if (DECL_NAME (decl) == NULL_TREE)
    {
      found = true;
      gcc_assert (DECL_ASSEMBLER_NAME_SET_P (decl));
      write_source_name (DECL_ASSEMBLER_NAME (decl));
    }
  else if (DECL_DECLARES_FUNCTION_P (decl))
    {
      found = true;

      /* A constrained hidden friend is mangled like a member function, with
	 the name prefixed by 'F'.  */
      if (member_like_constrained_friend_p (decl))
	write_char ('F');

      if (DECL_CONSTRUCTOR_P (decl))
	write_special_name_constructor (decl);
      else if (DECL_DESTRUCTOR_P (decl))
	write_special_name_destructor (decl);
      else if (DECL_CONV_FN_P (decl))
	{
	  /* Conversion operator. Handle it right here.
	     <operator> ::= cv <type>  */
	  tree type;
	  if (maybe_template_info (decl))
	    {
	      tree fn_type;
	      fn_type = get_mostly_instantiated_function_type (decl);
	      type = TREE_TYPE (fn_type);
	    }
	  else if (FNDECL_USED_AUTO (decl))
	    type = DECL_SAVED_AUTO_RETURN_TYPE (decl);
	  else
	    type = DECL_CONV_FN_TYPE (decl);
	  write_conversion_operator_name (type);
	}
      else if (DECL_OVERLOADED_OPERATOR_P (decl))
	{
	  tree t;
	  if (!(t = DECL_RAMP_FN (decl)))
	    t = decl;
	  const char *mangled_name
	    = (ovl_op_info[DECL_ASSIGNMENT_OPERATOR_P (t)]
	       [DECL_OVERLOADED_OPERATOR_CODE_RAW (t)].mangled_name);
	  write_string (mangled_name);
	}
      else if (UDLIT_OPER_P (DECL_NAME (decl)))
	write_literal_operator_name (DECL_NAME (decl));
      else
	found = false;
    }

  if (found)
    /* OK */;
  else if (VAR_OR_FUNCTION_DECL_P (decl) && ! TREE_PUBLIC (decl)
	   && DECL_NAMESPACE_SCOPE_P (decl)
	   && decl_linkage (decl) == lk_internal)
    {
      MANGLE_TRACE_TREE ("local-source-name", decl);
      write_char ('L');
      write_source_name (DECL_NAME (decl));
      /* The default discriminator is 1, and that's all we ever use,
	 so there's no code to output one here.  */
    }
  else
    {
      tree type = TREE_TYPE (decl);

      if (TREE_CODE (decl) == TYPE_DECL
          && TYPE_UNNAMED_P (type))
        write_unnamed_type_name (type);
      else if (TREE_CODE (decl) == TYPE_DECL && LAMBDA_TYPE_P (type))
        write_closure_type_name (type);
      else
        write_source_name (DECL_NAME (decl));
    }

  /* We use the ABI tags from the primary class template, ignoring tags on any
     specializations.  This is necessary because C++ doesn't require a
     specialization to be declared before it is used unless the use requires a
     complete type, but we need to get the tags right on incomplete types as
     well.  */
  if (tree tmpl = most_general_template (decl))
    {
      tree res = DECL_TEMPLATE_RESULT (tmpl);
      if (res == NULL_TREE)
	/* UNBOUND_CLASS_TEMPLATE.  */;
      else if (DECL_DECLARES_TYPE_P (decl))
	decl = res;
      else if (any_abi_below (11))
	{
	  /* ABI v10 implicit tags on the template.  */
	  tree mtags = missing_abi_tags (res);
	  /* Explicit tags on the template.  */
	  tree ttags = get_abi_tags (res);
	  /* Tags on the instantiation.  */
	  tree dtags = get_abi_tags (decl);

	  if (mtags && abi_warn_or_compat_version_crosses (10))
	    G.need_abi_warning = 1;

	  /* Add the v10 tags to the explicit tags now.  */
	  mtags = chainon (mtags, ttags);

	  if (!G.need_abi_warning
	      && abi_warn_or_compat_version_crosses (11)
	      && !equal_abi_tags (dtags, mtags))
	    G.need_abi_warning = 1;

	  if (!abi_version_at_least (10))
	    /* In abi <10, we only got the explicit tags.  */
	    decl = res;
	  else if (flag_abi_version == 10)
	    {
	      /* In ABI 10, we want explict and implicit tags.  */
	      write_abi_tags (mtags);
	      return;
	    }
	}
    }

  tree tags = get_abi_tags (decl);
  if (TREE_CODE (decl) == FUNCTION_DECL && DECL_CONV_FN_P (decl)
      && any_abi_below (11))
    if (tree mtags = missing_abi_tags (decl))
      {
	if (!abi_check (11))
	  tags = chainon (mtags, tags);
      }
  write_abi_tags (tags);
}

/* Write the unqualified-name for a conversion operator to TYPE.  */

static void
write_conversion_operator_name (const tree type)
{
  write_string ("cv");
  write_type (type);
}

/* Non-terminal <source-name>.  IDENTIFIER is an IDENTIFIER_NODE.

     <source-name> ::= </length/ number> <identifier>  */

static void
write_source_name (tree identifier)
{
  MANGLE_TRACE_TREE ("source-name", identifier);

  write_unsigned_number (IDENTIFIER_LENGTH (identifier));
  write_identifier (IDENTIFIER_POINTER (identifier));
}

/* Compare two TREE_STRINGs like strcmp.  */

static int
tree_string_cmp (const void *p1, const void *p2)
{
  if (p1 == p2)
    return 0;
  tree s1 = *(const tree*)p1;
  tree s2 = *(const tree*)p2;
  return strcmp (TREE_STRING_POINTER (s1),
		 TREE_STRING_POINTER (s2));
}

/* Return the TREE_LIST of TAGS as a sorted VEC.  */

static vec<tree, va_gc> *
sorted_abi_tags (tree tags)
{
  vec<tree, va_gc> * vec = make_tree_vector();

  for (tree t = tags; t; t = TREE_CHAIN (t))
    {
      if (ABI_TAG_IMPLICIT (t))
	continue;
      tree str = TREE_VALUE (t);
      vec_safe_push (vec, str);
    }

  vec->qsort (tree_string_cmp);

  return vec;
}

/* ID is the name of a function or type with abi_tags attribute TAGS.
   Write out the name, suitably decorated.  */

static void
write_abi_tags (tree tags)
{
  if (tags == NULL_TREE)
    return;

  vec<tree, va_gc> * vec = sorted_abi_tags (tags);

  unsigned i; tree str;
  FOR_EACH_VEC_ELT (*vec, i, str)
    {
      write_string ("B");
      write_unsigned_number (TREE_STRING_LENGTH (str) - 1);
      write_identifier (TREE_STRING_POINTER (str));
    }

  release_tree_vector (vec);
}

/* True iff the TREE_LISTS T1 and T2 of ABI tags are equivalent.  */

bool
equal_abi_tags (tree t1, tree t2)
{
  releasing_vec v1 = sorted_abi_tags (t1);
  releasing_vec v2 = sorted_abi_tags (t2);

  unsigned len1 = v1->length();
  if (len1 != v2->length())
    return false;
  for (unsigned i = 0; i < len1; ++i)
    if (strcmp (TREE_STRING_POINTER (v1[i]),
		TREE_STRING_POINTER (v2[i])) != 0)
      return false;
  return true;
}

/* Write a user-defined literal operator.
          ::= li <source-name>    # "" <source-name>
   IDENTIFIER is an LITERAL_IDENTIFIER_NODE.  */

static void
write_literal_operator_name (tree identifier)
{
  const char* suffix = UDLIT_OP_SUFFIX (identifier);
  write_identifier (UDLIT_OP_MANGLED_PREFIX);
  write_unsigned_number (strlen (suffix));
  write_identifier (suffix);
}

/* Encode 0 as _, and 1+ as n-1_.  */

static void
write_compact_number (int num)
{
  gcc_checking_assert (num >= 0);
  if (num > 0)
    write_unsigned_number (num - 1);
  write_char ('_');
}

/* Return how many unnamed types precede TYPE in its enclosing class.  */

static int
nested_anon_class_index (tree type)
{
  int index = 0;
  tree member = TYPE_FIELDS (TYPE_CONTEXT (type));
  for (; member; member = DECL_CHAIN (member))
    if (DECL_IMPLICIT_TYPEDEF_P (member))
      {
	tree memtype = TREE_TYPE (member);
	if (memtype == type)
	  return index;
	else if (TYPE_UNNAMED_P (memtype))
	  ++index;
      }

  if (seen_error ())
    return -1;

  gcc_unreachable ();
}

/* <unnamed-type-name> ::= Ut [ <nonnegative number> ] _ */

static void
write_unnamed_type_name (const tree type)
{
  int discriminator;
  MANGLE_TRACE_TREE ("unnamed-type-name", type);

  if (TYPE_FUNCTION_SCOPE_P (type))
    discriminator = discriminator_for_local_entity (TYPE_NAME (type));
  else if (TYPE_CLASS_SCOPE_P (type))
    discriminator = nested_anon_class_index (type);
  else
    {
      gcc_assert (no_linkage_check (type, /*relaxed_p=*/true));
      /* Just use the old mangling at namespace scope.  */
      write_source_name (TYPE_IDENTIFIER (type));
      return;
    }

  write_string ("Ut");
  write_compact_number (discriminator);
}

/* ABI issue #47: if a function template parameter is not "natural" for its
   argument we must mangle the parameter.  */

static bool
template_parm_natural_p (tree arg, tree parm)
{
  tree decl = TREE_VALUE (parm);

  /* A template parameter is "natural" if: */

  if (template_parameter_pack_p (decl))
    {
      tree args = ARGUMENT_PACK_ARGS (arg);
      if (TREE_VEC_LENGTH (args) == 0)
	{
#if 0
	  /* the argument is an empty pack and the parameter is an
	     unconstrained template type parameter pack; */
	  if (TREE_CODE (decl) != TYPE_DECL)
	    return false;
#else
	  /* Defer changing the mangling of C++11 code like
	     template <int i> int max();
	     template <int i, int j, int... rest> int max();  */
	  return true;
#endif
	}
      else
	/* the argument is a non-empty pack and a non-pack variant of the
	   parameter would be natural for the first element of the pack; */
	arg = TREE_VEC_ELT (args, 0);
    }

  /* the argument is a template and the parameter has the exact
     same template head; */
  if (TREE_CODE (decl) == TEMPLATE_DECL)
    return template_heads_equivalent_p (arg, decl);

  /* the argument is a type and the parameter is unconstrained; or */
  else if (TREE_CODE (decl) == TYPE_DECL)
    return !TEMPLATE_PARM_CONSTRAINTS (parm);

  /* the argument is a non-type template argument and the declared parameter
     type neither is instantiation dependent nor contains deduced types.  */
  else if (TREE_CODE (decl) == PARM_DECL)
    {
#if 0
      return !uses_template_parms (TREE_TYPE (decl));
#else
      /* Defer changing the mangling of C++98 code like
	 template <class T, T V> ....  */
      return !type_uses_auto (TREE_TYPE (decl));
#endif
    }

  gcc_unreachable ();
}

/* Used for lambda template head and non-natural function template parameters.

   <template-param-decl> ::= Ty               # template type parameter
	::= Tk <type-constraint>              # constrained type parameter
	::= Tn <type>                         # template non-type parameter
	::= Tt <template-param-decl>* [Q <constraint-expression] E  # ttp
	::= Tp <non-pack template-param-decl> # template parameter pack */

static void
write_template_param_decl (tree parm)
{
  tree decl = TREE_VALUE (parm);

  if (template_parameter_pack_p (decl))
    write_string ("Tp");

  switch (TREE_CODE (decl))
    {
    case PARM_DECL:
      {
	write_string ("Tn");

	tree type = TREE_TYPE (decl);
	if (tree c = (is_auto (type)
		      ? PLACEHOLDER_TYPE_CONSTRAINTS (type)
		      : NULL_TREE))
	  {
	    if (AUTO_IS_DECLTYPE (type))
	      write_string ("DK");
	    else
	      write_string ("Dk");
	    write_type_constraint (c);
	  }
	else
	  write_type (type);
      }
      break;

    case TEMPLATE_DECL:
      {
	write_string ("Tt");
	tree parms = DECL_INNERMOST_TEMPLATE_PARMS (decl);
	for (tree node : tree_vec_range (parms))
	  write_template_param_decl (node);
	write_char ('E');
      }
      break;

    case TYPE_DECL:
      if (tree c = TEMPLATE_PARM_CONSTRAINTS (parm))
	{
	  if (TREE_CODE (c) == UNARY_LEFT_FOLD_EXPR)
	    {
	      c = FOLD_EXPR_PACK (c);
	      c = PACK_EXPANSION_PATTERN (c);
	    }
	  if (TREE_CODE (decl) == TYPE_DECL)
	    {
	      write_string ("Tk");
	      write_type_constraint (c);
	    }
	}
      else
	write_string ("Ty");
      break;

    default:
      gcc_unreachable ();
    }
}

// A template head, for templated lambdas.
// New in ABI=18. Returns true iff we emitted anything -- used for ABI
// version warning.

static bool
write_closure_template_head (tree tmpl)
{
  bool any = false;

  // We only need one level of template parms
  tree parms = DECL_TEMPLATE_PARMS (tmpl);
  tree inner = INNERMOST_TEMPLATE_PARMS (parms);

  for (int ix = 0, len = TREE_VEC_LENGTH (inner); ix != len; ix++)
    {
      tree parm = TREE_VEC_ELT (inner, ix);
      if (parm == error_mark_node)
	continue;

      if (DECL_IMPLICIT_TEMPLATE_PARM_P (TREE_VALUE (parm)))
	// A synthetic parm, we're done.
	break;

      any = true;
      if (abi_version_at_least (18))
	write_template_param_decl (parm);
    }

  write_tparms_constraints (TEMPLATE_PARMS_CONSTRAINTS (parms));

  return any;
}

/* <closure-type-name> ::= Ul <lambda-sig> E [ <nonnegative number> ] _
   <lambda-sig> ::= <parameter type>+  # Parameter types or "v" if the lambda has no parameters */

static void
write_closure_type_name (const tree type)
{
  tree fn = lambda_function (type);
  tree lambda = CLASSTYPE_LAMBDA_EXPR (type);
  tree parms = TYPE_ARG_TYPES (TREE_TYPE (fn));

  MANGLE_TRACE_TREE ("closure-type-name", type);

  write_string ("Ul");

  if (auto ti = maybe_template_info (fn))
    if (write_closure_template_head (TI_TEMPLATE (ti)))
      // If there were any explicit template parms, we may need to
      // issue a mangling diagnostic.
      if (abi_warn_or_compat_version_crosses (18))
	G.need_abi_warning = true;

  write_method_parms (parms, TREE_CODE (TREE_TYPE (fn)) == METHOD_TYPE, fn);
  write_char ('E');
  if ((LAMBDA_EXPR_SCOPE_SIG_DISCRIMINATOR (lambda)
       != LAMBDA_EXPR_SCOPE_ONLY_DISCRIMINATOR (lambda))
      && abi_warn_or_compat_version_crosses (18))
    G.need_abi_warning = true;
  write_compact_number (abi_version_at_least (18)
			? LAMBDA_EXPR_SCOPE_SIG_DISCRIMINATOR (lambda)
			: LAMBDA_EXPR_SCOPE_ONLY_DISCRIMINATOR (lambda));
}

/* Convert NUMBER to ascii using base BASE and generating at least
   MIN_DIGITS characters. BUFFER points to the _end_ of the buffer
   into which to store the characters. Returns the number of
   characters generated (these will be laid out in advance of where
   BUFFER points).  */

static int
hwint_to_ascii (unsigned HOST_WIDE_INT number, const unsigned int base,
		char *buffer, const unsigned int min_digits)
{
  static const char base_digits[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  unsigned digits = 0;

  while (number)
    {
      unsigned HOST_WIDE_INT d = number / base;

      *--buffer = base_digits[number - d * base];
      digits++;
      number = d;
    }
  while (digits < min_digits)
    {
      *--buffer = base_digits[0];
      digits++;
    }
  return digits;
}

/* Non-terminal <number>.

     <number> ::= [n] </decimal integer/>  */

static void
write_number (unsigned HOST_WIDE_INT number, const int unsigned_p,
	      const unsigned int base)
{
  char buffer[sizeof (HOST_WIDE_INT) * 8];
  unsigned count = 0;

  if (!unsigned_p && (HOST_WIDE_INT) number < 0)
    {
      write_char ('n');
      number = -((HOST_WIDE_INT) number);
    }
  count = hwint_to_ascii (number, base, buffer + sizeof (buffer), 1);
  write_chars (buffer + sizeof (buffer) - count, count);
}

/* Write out an integral CST in decimal. Most numbers are small, and
   representable in a HOST_WIDE_INT. Occasionally we'll have numbers
   bigger than that, which we must deal with.  */

static inline void
write_integer_cst (const tree cst)
{
  int sign = tree_int_cst_sgn (cst);
  widest_int abs_value = wi::abs (wi::to_widest (cst));
  if (!wi::fits_uhwi_p (abs_value))
    {
      /* A bignum. We do this in chunks, each of which fits in a
	 HOST_WIDE_INT.  */
      char buffer[sizeof (HOST_WIDE_INT) * 8 * 2];
      unsigned HOST_WIDE_INT chunk;
      unsigned chunk_digits;
      char *ptr = buffer + sizeof (buffer);
      unsigned count = 0;
      tree n, base, type;
      int done;

      /* HOST_WIDE_INT must be at least 32 bits, so 10^9 is
	 representable.  */
      chunk = 1000000000;
      chunk_digits = 9;

      if (sizeof (HOST_WIDE_INT) >= 8)
	{
	  /* It is at least 64 bits, so 10^18 is representable.  */
	  chunk_digits = 18;
	  chunk *= chunk;
	}

      type = c_common_signed_or_unsigned_type (1, TREE_TYPE (cst));
      base = build_int_cstu (type, chunk);
      n = wide_int_to_tree (type, wi::to_wide (cst));

      if (sign < 0)
	{
	  write_char ('n');
	  n = fold_build1_loc (input_location, NEGATE_EXPR, type, n);
	}
      do
	{
	  tree d = fold_build2_loc (input_location, FLOOR_DIV_EXPR, type, n, base);
	  tree tmp = fold_build2_loc (input_location, MULT_EXPR, type, d, base);
	  unsigned c;

	  done = integer_zerop (d);
	  tmp = fold_build2_loc (input_location, MINUS_EXPR, type, n, tmp);
	  c = hwint_to_ascii (TREE_INT_CST_LOW (tmp), 10, ptr,
			      done ? 1 : chunk_digits);
	  ptr -= c;
	  count += c;
	  n = d;
	}
      while (!done);
      write_chars (ptr, count);
    }
  else
    {
      /* A small num.  */
      if (sign < 0)
	write_char ('n');
      write_unsigned_number (abs_value.to_uhwi ());
    }
}

/* Write out a floating-point literal.

    "Floating-point literals are encoded using the bit pattern of the
    target processor's internal representation of that number, as a
    fixed-length lowercase hexadecimal string, high-order bytes first
    (even if the target processor would store low-order bytes first).
    The "n" prefix is not used for floating-point literals; the sign
    bit is encoded with the rest of the number.

    Here are some examples, assuming the IEEE standard representation
    for floating point numbers.  (Spaces are for readability, not
    part of the encoding.)

	1.0f			Lf 3f80 0000 E
       -1.0f			Lf bf80 0000 E
	1.17549435e-38f		Lf 0080 0000 E
	1.40129846e-45f		Lf 0000 0001 E
	0.0f			Lf 0000 0000 E"

   Caller is responsible for the Lx and the E.  */
static void
write_real_cst (const tree value)
{
  long target_real[4];  /* largest supported float */
  /* Buffer for eight hex digits in a 32-bit number but big enough
     even for 64-bit long to avoid warnings.  */
  char buffer[17];
  int i, limit, dir;

  tree type = TREE_TYPE (value);
  int words = GET_MODE_BITSIZE (SCALAR_FLOAT_TYPE_MODE (type)) / 32;

  real_to_target (target_real, &TREE_REAL_CST (value),
		  TYPE_MODE (type));

  /* The value in target_real is in the target word order,
     so we must write it out backward if that happens to be
     little-endian.  write_number cannot be used, it will
     produce uppercase.  */
  if (FLOAT_WORDS_BIG_ENDIAN)
    i = 0, limit = words, dir = 1;
  else
    i = words - 1, limit = -1, dir = -1;

  for (; i != limit; i += dir)
    {
      sprintf (buffer, "%08lx", (unsigned long) target_real[i]);
      write_chars (buffer, 8);
    }
}

/* Non-terminal <identifier>.

     <identifier> ::= </unqualified source code identifier>  */

static void
write_identifier (const char *identifier)
{
  MANGLE_TRACE ("identifier", identifier);
  write_string (identifier);
}

/* Handle constructor productions of non-terminal <special-name>.
   CTOR is a constructor FUNCTION_DECL.

     <special-name> ::= C1   # complete object constructor
		    ::= C2   # base object constructor
		    ::= C3   # complete object allocating constructor

   Currently, allocating constructors are never used.  */

static void
write_special_name_constructor (const tree ctor)
{
  write_char ('C');
  bool new_inh = (flag_new_inheriting_ctors
		  && DECL_INHERITED_CTOR (ctor));
  if (new_inh)
    write_char ('I');
  if (DECL_BASE_CONSTRUCTOR_P (ctor))
    write_char ('2');
  /* This is the old-style "[unified]" constructor.
     In some cases, we may emit this function and call
     it from the clones in order to share code and save space.  */
  else if (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (ctor))
    write_char ('4');
  else
    {
      gcc_assert (DECL_COMPLETE_CONSTRUCTOR_P (ctor));
      write_char ('1');
    }
  if (new_inh)
    write_type (DECL_INHERITED_CTOR_BASE (ctor));
}

/* Handle destructor productions of non-terminal <special-name>.
   DTOR is a destructor FUNCTION_DECL.

     <special-name> ::= D0 # deleting (in-charge) destructor
		    ::= D1 # complete object (in-charge) destructor
		    ::= D2 # base object (not-in-charge) destructor  */

static void
write_special_name_destructor (const tree dtor)
{
  if (DECL_DELETING_DESTRUCTOR_P (dtor))
    write_string ("D0");
  else if (DECL_BASE_DESTRUCTOR_P (dtor))
    write_string ("D2");
  else if (DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (dtor))
    /* This is the old-style "[unified]" destructor.
       In some cases, we may emit this function and call
       it from the clones in order to share code and save space.  */
    write_string ("D4");
  else
    {
      gcc_assert (DECL_COMPLETE_DESTRUCTOR_P (dtor));
      write_string ("D1");
    }
}

/* Return the discriminator for ENTITY appearing inside
   FUNCTION.  The discriminator is the lexical ordinal of VAR or TYPE among
   entities with the same name and kind in the same FUNCTION.  */

static int
discriminator_for_local_entity (tree entity)
{
  if (!DECL_LANG_SPECIFIC (entity))
    {
      /* Some decls, like __FUNCTION__, don't need a discriminator.  */
      gcc_checking_assert (DECL_ARTIFICIAL (entity));
      return 0;
    }
  else if (tree disc = DECL_DISCRIMINATOR (entity))
    return TREE_INT_CST_LOW (disc);
  else
    /* The first entity with a particular name doesn't get
       DECL_DISCRIMINATOR set up.  */
    return 0;
}

/* Return the discriminator for STRING, a string literal used inside
   FUNCTION.  The discriminator is the lexical ordinal of STRING among
   string literals used in FUNCTION.  */

static int
discriminator_for_string_literal (tree /*function*/,
				  tree /*string*/)
{
  /* For now, we don't discriminate amongst string literals.  */
  return 0;
}

/*   <discriminator> := _ <number>    # when number < 10
                     := __ <number> _ # when number >= 10

   The discriminator is used only for the second and later occurrences
   of the same name within a single function. In this case <number> is
   n - 2, if this is the nth occurrence, in lexical order.  */

static void
write_discriminator (const int discriminator)
{
  /* If discriminator is zero, don't write anything.  Otherwise...  */
  if (discriminator > 0)
    {
      write_char ('_');
      if (discriminator - 1 >= 10)
	{
	  if (abi_check (11))
	    write_char ('_');
	}
      write_unsigned_number (discriminator - 1);
      if (abi_version_at_least (11) && discriminator - 1 >= 10)
	write_char ('_');
    }
}

/* Mangle the name of a function-scope entity.  FUNCTION is the
   FUNCTION_DECL for the enclosing function, or a PARM_DECL for lambdas in
   default argument scope.  ENTITY is the decl for the entity itself.
   LOCAL_ENTITY is the entity that's directly scoped in FUNCTION_DECL,
   either ENTITY itself or an enclosing scope of ENTITY.

     <local-name> := Z <function encoding> E <entity name> [<discriminator>]
		  := Z <function encoding> E s [<discriminator>]
		  := Z <function encoding> Ed [ <parameter number> ] _ <entity name> */

static void
write_local_name (tree function, const tree local_entity,
		  const tree entity)
{
  tree parm = NULL_TREE;

  MANGLE_TRACE_TREE ("local-name", entity);

  if (TREE_CODE (function) == PARM_DECL)
    {
      parm = function;
      function = DECL_CONTEXT (parm);
    }

  write_char ('Z');
  write_encoding (function);
  write_char ('E');

  /* For this purpose, parameters are numbered from right-to-left.  */
  if (parm)
    {
      int i = list_length (parm);
      write_char ('d');
      write_compact_number (i - 1);
    }

  if (TREE_CODE (entity) == STRING_CST)
    {
      write_char ('s');
      write_discriminator (discriminator_for_string_literal (function,
							     entity));
    }
  else
    {
      /* Now the <entity name>.  Let write_name know its being called
	 from <local-name>, so it doesn't try to process the enclosing
	 function scope again.  */
      write_name (entity, /*ignore_local_scope=*/1);
      if (DECL_DISCRIMINATOR_P (local_entity)
	  && !(TREE_CODE (local_entity) == TYPE_DECL
	       && TYPE_ANON_P (TREE_TYPE (local_entity))))
	write_discriminator (discriminator_for_local_entity (local_entity));
    }
}

/* Non-terminals <type> and <CV-qualifier>.

     <type> ::= <builtin-type>
	    ::= <function-type>
	    ::= <class-enum-type>
	    ::= <array-type>
	    ::= <pointer-to-member-type>
	    ::= <template-param>
	    ::= <substitution>
	    ::= <CV-qualifier>
	    ::= P <type>    # pointer-to
	    ::= R <type>    # reference-to
	    ::= C <type>    # complex pair (C 2000)
	    ::= G <type>    # imaginary (C 2000)     [not supported]
	    ::= U <source-name> <type>   # vendor extended type qualifier

   C++0x extensions

     <type> ::= RR <type>   # rvalue reference-to
     <type> ::= Dt <expression> # decltype of an id-expression or
                                # class member access
     <type> ::= DT <expression> # decltype of an expression
     <type> ::= Dn              # decltype of nullptr

   TYPE is a type node.  */

static void
write_type (tree type)
{
  /* This gets set to nonzero if TYPE turns out to be a (possibly
     CV-qualified) builtin type.  */
  int is_builtin_type = 0;

  MANGLE_TRACE_TREE ("type", type);

  if (type == error_mark_node)
    return;

  type = canonicalize_for_substitution (type);
  if (find_substitution (type))
    return;


  if (write_CV_qualifiers_for_type (type) > 0)
    /* If TYPE was CV-qualified, we just wrote the qualifiers; now
       mangle the unqualified type.  The recursive call is needed here
       since both the qualified and unqualified types are substitution
       candidates.  */
    {
      tree t = TYPE_MAIN_VARIANT (type);
      if (TYPE_ATTRIBUTES (t) && !OVERLOAD_TYPE_P (t))
	{
	  tree attrs = NULL_TREE;
	  if (tx_safe_fn_type_p (type))
	    attrs = tree_cons (get_identifier ("transaction_safe"),
			       NULL_TREE, attrs);
	  t = cp_build_type_attribute_variant (t, attrs);
	}
      gcc_assert (t != type);
      if (FUNC_OR_METHOD_TYPE_P (t))
	{
	  t = build_ref_qualified_type (t, type_memfn_rqual (type));
	  if (flag_noexcept_type)
	    {
	      tree r = TYPE_RAISES_EXCEPTIONS (type);
	      t = build_exception_variant (t, r);
	    }
	  if (abi_version_at_least (8)
	      || type == TYPE_MAIN_VARIANT (type))
	    /* Avoid adding the unqualified function type as a substitution.  */
	    write_function_type (t);
	  else
	    write_type (t);
	  if (abi_warn_or_compat_version_crosses (8))
	    G.need_abi_warning = 1;
	}
      else
	write_type (t);
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    /* It is important not to use the TYPE_MAIN_VARIANT of TYPE here
       so that the cv-qualification of the element type is available
       in write_array_type.  */
    write_array_type (type);
  else
    {
      tree type_orig = type;

      /* See through any typedefs.  */
      type = TYPE_MAIN_VARIANT (type);
      if (FUNC_OR_METHOD_TYPE_P (type))
	type = cxx_copy_lang_qualifiers (type, type_orig);

      /* According to the C++ ABI, some library classes are passed the
	 same as the scalar type of their single member and use the same
	 mangling.  */
      if (TREE_CODE (type) == RECORD_TYPE && TYPE_TRANSPARENT_AGGR (type))
	type = TREE_TYPE (first_field (type));

      if (TYPE_PTRDATAMEM_P (type))
	write_pointer_to_member_type (type);
      else
        {
	  /* Handle any target-specific fundamental types.  */
	  const char *target_mangling
	    = targetm.mangle_type (type_orig);

	  if (target_mangling)
	    {
	      write_string (target_mangling);
	      /* Add substitutions for types other than fundamental
		 types.  */
	      if (!VOID_TYPE_P (type)
		  && TREE_CODE (type) != INTEGER_TYPE
		  && TREE_CODE (type) != REAL_TYPE
		  && TREE_CODE (type) != BOOLEAN_TYPE)
		add_substitution (type);
	      return;
	    }

	  switch (TREE_CODE (type))
	    {
	    case VOID_TYPE:
	    case BOOLEAN_TYPE:
	    case INTEGER_TYPE:  /* Includes wchar_t.  */
	    case REAL_TYPE:
	    case FIXED_POINT_TYPE:
	      {
		/* If this is a typedef, TYPE may not be one of
		   the standard builtin type nodes, but an alias of one.  Use
		   TYPE_MAIN_VARIANT to get to the underlying builtin type.  */
		write_builtin_type (TYPE_MAIN_VARIANT (type));
		++is_builtin_type;
	      }
	      break;

	    case COMPLEX_TYPE:
	      write_char ('C');
	      write_type (TREE_TYPE (type));
	      break;

	    case FUNCTION_TYPE:
	    case METHOD_TYPE:
	      write_function_type (type);
	      break;

	    case UNION_TYPE:
	    case RECORD_TYPE:
	    case ENUMERAL_TYPE:
	      /* A pointer-to-member function is represented as a special
		 RECORD_TYPE, so check for this first.  */
	      if (TYPE_PTRMEMFUNC_P (type))
		write_pointer_to_member_type (type);
	      else
		write_class_enum_type (type);
	      break;

	    case TYPENAME_TYPE:
	    case UNBOUND_CLASS_TEMPLATE:
	      /* We handle TYPENAME_TYPEs and UNBOUND_CLASS_TEMPLATEs like
		 ordinary nested names.  */
	      write_nested_name (TYPE_STUB_DECL (type));
	      break;

	    case POINTER_TYPE:
	    case REFERENCE_TYPE:
	      if (TYPE_PTR_P (type))
		write_char ('P');
	      else if (TYPE_REF_IS_RVALUE (type))
		write_char ('O');
              else
                write_char ('R');
	      {
		tree target = TREE_TYPE (type);
		/* Attribute const/noreturn are not reflected in mangling.
		   We strip them here rather than at a lower level because
		   a typedef or template argument can have function type
		   with function-cv-quals (that use the same representation),
		   but you can't have a pointer/reference to such a type.  */
		if (TREE_CODE (target) == FUNCTION_TYPE)
		  {
		    if (abi_warn_or_compat_version_crosses (5)
			&& TYPE_QUALS (target) != TYPE_UNQUALIFIED)
		      G.need_abi_warning = 1;
		    if (abi_version_at_least (5))
		      target = build_qualified_type (target, TYPE_UNQUALIFIED);
		  }
		write_type (target);
	      }
	      break;

	    case TEMPLATE_TYPE_PARM:
	      if (is_auto (type))
		{
		  if (template_placeholder_p (type)
		      && abi_check (19))
		    {
		      /* ABI #109: placeholder is mangled as its template.  */
		      type = CLASS_PLACEHOLDER_TEMPLATE (type);
		      if (find_substitution (type))
			return;
		      write_name (type, 0);
		      break;
		    }
		  if (AUTO_IS_DECLTYPE (type))
		    write_identifier ("Dc");
		  else
		    write_identifier ("Da");
		  ++is_builtin_type;
		  break;
		}
	      /* fall through.  */
	    case TEMPLATE_PARM_INDEX:
	      write_template_param (type);
	      break;

	    case TEMPLATE_TEMPLATE_PARM:
	      write_template_template_param (type);
	      break;

	    case BOUND_TEMPLATE_TEMPLATE_PARM:
	      write_template_template_param (type);
	      write_template_args
		(TI_ARGS (TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (type)));
	      break;

	    case VECTOR_TYPE:
	      if (abi_version_at_least (4))
		{
		  write_string ("Dv");
		  /* Non-constant vector size would be encoded with
		     _ expression, but we don't support that yet.  */
		  write_unsigned_number (TYPE_VECTOR_SUBPARTS (type)
					 .to_constant ());
		  write_char ('_');
		}
	      else
		write_string ("U8__vector");
	      if (abi_warn_or_compat_version_crosses (4))
		G.need_abi_warning = 1;
	      write_type (TREE_TYPE (type));
	      break;

            case TYPE_PACK_EXPANSION:
              write_string ("Dp");
              write_type (PACK_EXPANSION_PATTERN (type));
              break;

            case DECLTYPE_TYPE:
	      /* These shouldn't make it into mangling.  */
	      gcc_assert (!DECLTYPE_FOR_LAMBDA_CAPTURE (type)
			  && !DECLTYPE_FOR_LAMBDA_PROXY (type));

	      /* In ABI <5, we stripped decltype of a plain decl.  */
	      if (DECLTYPE_TYPE_ID_EXPR_OR_MEMBER_ACCESS_P (type))
		{
		  tree expr = DECLTYPE_TYPE_EXPR (type);
		  tree etype = NULL_TREE;
		  switch (TREE_CODE (expr))
		    {
		    case VAR_DECL:
		    case PARM_DECL:
		    case RESULT_DECL:
		    case FUNCTION_DECL:
		    case CONST_DECL:
		    case TEMPLATE_PARM_INDEX:
		      etype = TREE_TYPE (expr);
		      break;

		    default:
		      break;
		    }

		  if (etype && !type_uses_auto (etype))
		    {
		      if (!abi_check (5))
			{
			  write_type (etype);
			  return;
			}
		    }
		}

              write_char ('D');
              if (DECLTYPE_TYPE_ID_EXPR_OR_MEMBER_ACCESS_P (type))
                write_char ('t');
              else
                write_char ('T');
	      ++cp_unevaluated_operand;
              write_expression (DECLTYPE_TYPE_EXPR (type));
	      --cp_unevaluated_operand;
              write_char ('E');
              break;

	    case NULLPTR_TYPE:
	      write_string ("Dn");
	      if (abi_check (7))
		++is_builtin_type;
	      break;

	    case TYPEOF_TYPE:
	      sorry ("mangling %<typeof%>, use %<decltype%> instead");
	      break;

	    case TRAIT_TYPE:
	      error ("use of built-in trait %qT in function signature; "
		     "use library traits instead", type);
	      break;

	    case PACK_INDEX_TYPE:
	      /* TODO Mangle pack indexing
		 <https://github.com/itanium-cxx-abi/cxx-abi/issues/175>.  */
	      sorry ("mangling type pack index");
	      break;

	    case LANG_TYPE:
	      /* fall through.  */

	    default:
	      gcc_unreachable ();
	    }
	}
    }

  /* Types other than builtin types are substitution candidates.  */
  if (!is_builtin_type)
    add_substitution (type);
}

/* qsort callback for sorting a vector of attribute entries.  */

static int
attr_strcmp (const void *p1, const void *p2)
{
  tree a1 = *(const tree*)p1;
  tree a2 = *(const tree*)p2;

  const attribute_spec *as1 = lookup_attribute_spec (get_attribute_name (a1));
  const attribute_spec *as2 = lookup_attribute_spec (get_attribute_name (a2));

  return strcmp (as1->name, as2->name);
}

/* Return true if we should mangle a type attribute with name NAME.  */

static bool
mangle_type_attribute_p (tree name)
{
  const attribute_spec *as = lookup_attribute_spec (name);
  if (!as || !as->affects_type_identity)
    return false;

  /* Skip internal-only attributes, which are distinguished from others
     by having a space.  At present, all internal-only attributes that
     affect type identity are target-specific and are handled by
     targetm.mangle_type instead.

     Another reason to do this is that a space isn't a valid identifier
     character for most file formats.  */
  if (strchr (IDENTIFIER_POINTER (name), ' '))
    return false;

  /* The following attributes are mangled specially.  */
  if (is_attribute_p ("transaction_safe", name))
    return false;
  if (is_attribute_p ("abi_tag", name))
    return false;

  return true;
}

/* Non-terminal <CV-qualifiers> for type nodes.  Returns the number of
   CV-qualifiers written for TYPE.

     <CV-qualifiers> ::= [r] [V] [K]  */

static int
write_CV_qualifiers_for_type (const tree type)
{
  int num_qualifiers = 0;

  /* The order is specified by:

       "In cases where multiple order-insensitive qualifiers are
       present, they should be ordered 'K' (closest to the base type),
       'V', 'r', and 'U' (farthest from the base type) ..."  */

  /* Mangle attributes that affect type identity as extended qualifiers.

     We don't do this with classes and enums because their attributes
     are part of their definitions, not something added on.  */

  if (!OVERLOAD_TYPE_P (type))
    {
      auto_vec<tree> vec;
      for (tree a = TYPE_ATTRIBUTES (type); a; a = TREE_CHAIN (a))
	if (mangle_type_attribute_p (get_attribute_name (a)))
	  vec.safe_push (a);
      if (abi_warn_or_compat_version_crosses (10) && !vec.is_empty ())
	G.need_abi_warning = true;
      if (abi_version_at_least (10))
	{
	  vec.qsort (attr_strcmp);
	  while (!vec.is_empty())
	    {
	      tree a = vec.pop();
	      const attribute_spec *as
		= lookup_attribute_spec (get_attribute_name (a));

	      write_char ('U');
	      write_unsigned_number (strlen (as->name));
	      write_string (as->name);
	      if (TREE_VALUE (a))
		{
		  write_char ('I');
		  for (tree args = TREE_VALUE (a); args;
		       args = TREE_CHAIN (args))
		    {
		      tree arg = TREE_VALUE (args);
		      write_template_arg (arg);
		    }
		  write_char ('E');
		}

	      ++num_qualifiers;
	    }
	}
    }

  /* Note that we do not use cp_type_quals below; given "const
     int[3]", the "const" is emitted with the "int", not with the
     array.  */
  cp_cv_quals quals = TYPE_QUALS (type);

  if (quals & TYPE_QUAL_RESTRICT)
    {
      write_char ('r');
      ++num_qualifiers;
    }
  if (quals & TYPE_QUAL_VOLATILE)
    {
      write_char ('V');
      ++num_qualifiers;
    }
  if (quals & TYPE_QUAL_CONST)
    {
      write_char ('K');
      ++num_qualifiers;
    }

  return num_qualifiers;
}

/* Non-terminal <builtin-type>.

     <builtin-type> ::= v   # void
		    ::= b   # bool
		    ::= w   # wchar_t
		    ::= c   # char
		    ::= a   # signed char
		    ::= h   # unsigned char
		    ::= s   # short
		    ::= t   # unsigned short
		    ::= i   # int
		    ::= j   # unsigned int
		    ::= l   # long
		    ::= m   # unsigned long
		    ::= x   # long long, __int64
		    ::= y   # unsigned long long, __int64
		    ::= n   # __int128
		    ::= o   # unsigned __int128
		    ::= f   # float
		    ::= d   # double
		    ::= e   # long double, __float80
		    ::= g   # __float128          [not supported]
		    ::= u <source-name>  # vendor extended type */

static void
write_builtin_type (tree type)
{
  if (TYPE_CANONICAL (type))
    type = TYPE_CANONICAL (type);

  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
      write_char ('v');
      break;

    case BOOLEAN_TYPE:
      write_char ('b');
      break;

    case INTEGER_TYPE:
      /* TYPE may still be wchar_t, char8_t, char16_t, or char32_t, since that
	 isn't in integer_type_nodes.  */
      if (type == wchar_type_node)
	write_char ('w');
      else if (type == char8_type_node)
	write_string ("Du");
      else if (type == char16_type_node)
	write_string ("Ds");
      else if (type == char32_type_node)
	write_string ("Di");
      else
	{
	  size_t itk;
	  /* Assume TYPE is one of the shared integer type nodes.  Find
	     it in the array of these nodes.  */
	iagain:
	  for (itk = 0; itk < itk_none; ++itk)
	    if (integer_types[itk] != NULL_TREE
		&& integer_type_codes[itk] != '\0'
		&& type == integer_types[itk])
	      {
		/* Print the corresponding single-letter code.  */
		write_char (integer_type_codes[itk]);
		break;
	      }

	  if (itk == itk_none)
	    {
	      tree t = c_common_type_for_mode (TYPE_MODE (type),
					       TYPE_UNSIGNED (type));
	      if (type != t)
		{
		  type = t;
		  goto iagain;
		}

	      if (TYPE_PRECISION (type) == 128)
		write_char (TYPE_UNSIGNED (type) ? 'o' : 'n');
	      else
		{
		  /* Allow for cases where TYPE is not one of the shared
		     integer type nodes and write a "vendor extended builtin
		     type" with a name the form intN or uintN, respectively.
		     Situations like this can happen if you have an
		     __attribute__((__mode__(__SI__))) type and use exotic
		     switches like '-mint8' on AVR.  Of course, this is
		     undefined by the C++ ABI (and '-mint8' is not even
		     Standard C conforming), but when using such special
		     options you're pretty much in nowhere land anyway.  */
		  const char *prefix;
		  char prec[11];	/* up to ten digits for an unsigned */

		  prefix = TYPE_UNSIGNED (type) ? "uint" : "int";
		  sprintf (prec, "%u", (unsigned) TYPE_PRECISION (type));
		  write_char ('u');	/* "vendor extended builtin type" */
		  write_unsigned_number (strlen (prefix) + strlen (prec));
		  write_string (prefix);
		  write_string (prec);
		}
	    }
	}
      break;

    case REAL_TYPE:
      if (type == float_type_node)
	write_char ('f');
      else if (type == double_type_node)
	write_char ('d');
      else if (type == long_double_type_node)
	write_char ('e');
      else if (type == dfloat32_type_node)
	write_string ("Df");
      else if (type == dfloat64_type_node)
	write_string ("Dd");
      else if (type == dfloat128_type_node)
	write_string ("De");
      else if (type == float16_type_node)
	write_string ("DF16_");
      else if (type == float32_type_node)
	write_string ("DF32_");
      else if (type == float64_type_node)
	write_string ("DF64_");
      else if (type == float128_type_node)
	write_string ("DF128_");
      else if (type == float32x_type_node)
	write_string ("DF32x");
      else if (type == float64x_type_node)
	write_string ("DF64x");
      else if (type == float128x_type_node)
	write_string ("DF128x");
      else if (type == bfloat16_type_node)
	write_string ("DF16b");
      else
	gcc_unreachable ();
      break;

    default:
      gcc_unreachable ();
    }
}

/* Non-terminal <function-type>.  NODE is a FUNCTION_TYPE or
   METHOD_TYPE.  The return type is mangled before the parameter
   types.

     <function-type> ::= F [Y] <bare-function-type> [<ref-qualifier>] E   */

static void
write_function_type (const tree type)
{
  MANGLE_TRACE_TREE ("function-type", type);

  /* For a pointer to member function, the function type may have
     cv-qualifiers, indicating the quals for the artificial 'this'
     parameter.  */
  if (TREE_CODE (type) == METHOD_TYPE)
    {
      /* The first parameter must be a POINTER_TYPE pointing to the
	 `this' parameter.  */
      tree this_type = class_of_this_parm (type);
      write_CV_qualifiers_for_type (this_type);
    }

  write_exception_spec (TYPE_RAISES_EXCEPTIONS (type));

  if (tx_safe_fn_type_p (type))
    write_string ("Dx");

  write_char ('F');
  /* We don't track whether or not a type is `extern "C"'.  Note that
     you can have an `extern "C"' function that does not have
     `extern "C"' type, and vice versa:

       extern "C" typedef void function_t();
       function_t f; // f has C++ linkage, but its type is
		     // `extern "C"'

       typedef void function_t();
       extern "C" function_t f; // Vice versa.

     See [dcl.link].  */
  write_bare_function_type (type, /*include_return_type_p=*/1,
			    /*decl=*/NULL);
  if (FUNCTION_REF_QUALIFIED (type))
    {
      if (FUNCTION_RVALUE_QUALIFIED (type))
	write_char ('O');
      else
	write_char ('R');
    }
  write_char ('E');
}

/* Non-terminal <bare-function-type>.  TYPE is a FUNCTION_TYPE or
   METHOD_TYPE.  If INCLUDE_RETURN_TYPE is nonzero, the return value
   is mangled before the parameter types.  If non-NULL, DECL is
   FUNCTION_DECL for the function whose type is being emitted.  */

static void
write_bare_function_type (const tree type, const int include_return_type_p,
			  const tree decl)
{
  MANGLE_TRACE_TREE ("bare-function-type", type);

  /* Mangle the return type, if requested.  */
  if (include_return_type_p)
    write_type (TREE_TYPE (type));

  /* Now mangle the types of the arguments.  */
  ++G.parm_depth;
  write_method_parms (TYPE_ARG_TYPES (type),
		      TREE_CODE (type) == METHOD_TYPE,
		      decl);
  --G.parm_depth;
}

/* Write the mangled representation of a method parameter list of
   types given in PARM_TYPES.  If METHOD_P is nonzero, the function is
   considered a non-static method, and the this parameter is omitted.
   If non-NULL, DECL is the FUNCTION_DECL for the function whose
   parameters are being emitted.  */

static void
write_method_parms (tree parm_types, const int method_p, const tree decl)
{
  tree first_parm_type;
  tree parm_decl = decl ? DECL_ARGUMENTS (decl) : NULL_TREE;

  /* Assume this parameter type list is variable-length.  If it ends
     with a void type, then it's not.  */
  int varargs_p = 1;

  /* If this is a member function, skip the first arg, which is the
     this pointer.
       "Member functions do not encode the type of their implicit this
       parameter."

     Similarly, there's no need to mangle artificial parameters, like
     the VTT parameters for constructors and destructors.  */
  if (method_p)
    {
      parm_types = TREE_CHAIN (parm_types);
      parm_decl = parm_decl ? DECL_CHAIN (parm_decl) : NULL_TREE;

      while (parm_decl && DECL_ARTIFICIAL (parm_decl))
	{
	  parm_types = TREE_CHAIN (parm_types);
	  parm_decl = DECL_CHAIN (parm_decl);
	}

      if (decl && ctor_omit_inherited_parms (decl))
	/* Bring back parameters omitted from an inherited ctor.  */
	parm_types = FUNCTION_FIRST_USER_PARMTYPE (DECL_ORIGIN (decl));
    }

  for (first_parm_type = parm_types;
       parm_types;
       parm_types = TREE_CHAIN (parm_types))
    {
      tree parm = TREE_VALUE (parm_types);
      if (parm == void_type_node)
	{
	  /* "Empty parameter lists, whether declared as () or
	     conventionally as (void), are encoded with a void parameter
	     (v)."  */
	  if (parm_types == first_parm_type)
	    write_type (parm);
	  /* If the parm list is terminated with a void type, it's
	     fixed-length.  */
	  varargs_p = 0;
	  /* A void type better be the last one.  */
	  gcc_assert (TREE_CHAIN (parm_types) == NULL);
	}
      else
	write_type (parm);
    }

  if (varargs_p)
    /* <builtin-type> ::= z  # ellipsis  */
    write_char ('z');
}

/* <class-enum-type> ::= <name>  */

static void
write_class_enum_type (const tree type)
{
  write_name (TYPE_NAME (type), /*ignore_local_scope=*/0);
}

/* Mangle a requirement REQ in a requires-expression.  */

static void
write_requirement (tree req)
{
  tree op = TREE_OPERAND (req, 0);

  switch (tree_code code = TREE_CODE (req))
    {
      /* # simple-requirement or compound-requirement
	 <requirement> ::= X <expression> [ N ] [ R <type-constraint> ] */
    case SIMPLE_REQ:
    case COMPOUND_REQ:
      write_char ('X');
      write_expression (op);
      if (code == SIMPLE_REQ)
	break;
      if (COMPOUND_REQ_NOEXCEPT_P (req))
	write_char ('N');
      if (tree constr = TREE_OPERAND (req, 1))
	{
	  write_char ('R');
	  write_type_constraint (PLACEHOLDER_TYPE_CONSTRAINTS (constr));
	}
      break;

      /* <requirement> ::= T <type> # type-requirement */
    case TYPE_REQ:
      write_char ('T');
      write_type (op);
      break;

      /* <requirement> ::= Q <constraint-expression> # nested-requirement */
    case NESTED_REQ:
      write_char ('Q');
      write_constraint_expression (op);
      break;

    default:
      gcc_unreachable ();
    }
}

/* # requires { ... }
   <expression> ::= rq <requirement>+ E
   # requires (...) { ... }
   <expression> ::= rQ <bare-function-type> _ <requirement>+ E */

static void
write_requires_expr (tree expr)
{
  tree parms = REQUIRES_EXPR_PARMS (expr);
  if (parms)
    {
      write_string ("rQ");
      ++G.parm_depth;
      for (; parms; parms = DECL_CHAIN (parms))
	write_type (cv_unqualified (TREE_TYPE (parms)));
      --G.parm_depth;
      write_char ('_');
    }
  else
    write_string ("rq");

  for (tree reqs = REQUIRES_EXPR_REQS (expr); reqs;
       reqs = TREE_CHAIN (reqs))
    write_requirement (TREE_VALUE (reqs));

  write_char ('E');
}

/* Non-terminal <template-args>.  ARGS is a TREE_VEC of template
   arguments.

     <template-args> ::= I <template-arg>* [Q <constraint-expr>] E  */

static void
write_template_args (tree args, tree parms /*= NULL_TREE*/)
{
  int i;
  int length = 0;

  MANGLE_TRACE_TREE ("template-args", args);

  write_char ('I');

  if (args)
    length = TREE_VEC_LENGTH (args);

  tree constraints = NULL_TREE;
  if (parms)
    {
      constraints = TEMPLATE_PARMS_CONSTRAINTS (parms);
      parms = INNERMOST_TEMPLATE_PARMS (parms);
    }

  if (args && length && TREE_CODE (TREE_VEC_ELT (args, 0)) == TREE_VEC)
    {
      /* We have nested template args.  We want the innermost template
	 argument list.  */
      args = TREE_VEC_ELT (args, length - 1);
      length = TREE_VEC_LENGTH (args);
    }
  if (TEMPLATE_ARGS_TYPE_CONSTRAINT_P (args))
    /* Skip the constrained type.  */
    i = 1;
  else
    i = 0;
  bool implicit_parm_scope = false;
  for (; i < length; ++i)
    {
      tree arg = TREE_VEC_ELT (args, i);
      if (parms)
	{
	  tree parm = TREE_VEC_ELT (parms, i);
	  tree decl = TREE_VALUE (parm);
	  if (DECL_IMPLICIT_TEMPLATE_PARM_P (decl)
	      && !implicit_parm_scope)
	    {
	      /* The rest of the template parameters are based on generic
		 function parameters, so any expressions in their
		 type-constraints are in parameter scope.  */
	      implicit_parm_scope = true;
	      ++G.parm_depth;
	    }
	  if (!template_parm_natural_p (arg, parm)
	      && abi_check (19))
	    write_template_param_decl (parm);
	}
      write_template_arg (arg);
    }
  if (implicit_parm_scope)
    --G.parm_depth;

  write_tparms_constraints (constraints);

  write_char ('E');
}

/* Write out the
   <unqualified-name>
   <unqualified-name> <template-args>
   part of SCOPE_REF or COMPONENT_REF mangling.  */

static void
write_member_name (tree member)
{
  if (identifier_p (member))
    {
      if (IDENTIFIER_ANY_OP_P (member))
	{
	  if (abi_check (11))
	    write_string ("on");
	}
      write_unqualified_id (member);
    }
  else if (DECL_P (member))
    {
      if (ANON_AGGR_TYPE_P (TREE_TYPE (member)))
	;
      else if (DECL_OVERLOADED_OPERATOR_P (member))
	{
	  if (abi_check (16))
	    write_string ("on");
	}
      write_unqualified_name (member);
    }
  else if (TREE_CODE (member) == TEMPLATE_ID_EXPR)
    {
      tree name = TREE_OPERAND (member, 0);
      name = OVL_FIRST (name);
      write_member_name (name);
      write_template_args (TREE_OPERAND (member, 1));
    }
  else
    write_expression (member);
}

/* EXPR is a base COMPONENT_REF; write the minimized base conversion path for
   converting to BASE, or just the conversion of EXPR if BASE is null.

   "Given a fully explicit base path P := C_n -> ... -> C_0, the minimized base
   path Min(P) is defined as follows: let C_i be the last element for which the
   conversion to C_0 is unambiguous; if that element is C_n, the minimized path
   is C_n -> C_0; otherwise, the minimized path is Min(C_n -> ... -> C_i) ->
   C_0."

   We mangle the conversion to C_i if it's different from C_n.  */

static bool
write_base_ref (tree expr, tree base = NULL_TREE)
{
  if (TREE_CODE (expr) != COMPONENT_REF)
    return false;

  tree field = TREE_OPERAND (expr, 1);

  if (TREE_CODE (field) != FIELD_DECL || !DECL_FIELD_IS_BASE (field))
    return false;

  tree object = TREE_OPERAND (expr, 0);

  tree binfo = NULL_TREE;
  if (base)
    {
      tree cur = TREE_TYPE (object);
      binfo = lookup_base (cur, base, ba_unique, NULL, tf_none);
    }
  else
    /* We're at the end of the base conversion chain, so it can't be
       ambiguous.  */
    base = TREE_TYPE (field);

  if (binfo == error_mark_node)
    {
      /* cur->base is ambiguous, so make the conversion to
	 last explicit, expressed as a cast (last&)object.  */
      tree last = TREE_TYPE (expr);
      write_string (OVL_OP_INFO (false, CAST_EXPR)->mangled_name);
      write_type (build_reference_type (last));
      write_expression (object);
    }
  else if (write_base_ref (object, base))
    /* cur->base is unambiguous, but we had another base conversion
       underneath and wrote it out.  */;
  else
    /* No more base conversions, just write out the object.  */
    write_expression (object);

  return true;
}

/* The number of elements spanned by a RANGE_EXPR.  */

unsigned HOST_WIDE_INT
range_expr_nelts (tree expr)
{
  tree lo = TREE_OPERAND (expr, 0);
  tree hi = TREE_OPERAND (expr, 1);
  return tree_to_uhwi (hi) - tree_to_uhwi (lo) + 1;
}

/* <expression> ::= <unary operator-name> <expression>
		::= <binary operator-name> <expression> <expression>
		::= <expr-primary>

   <expr-primary> ::= <template-param>
		  ::= L <type> <value number> E		# literal
		  ::= L <mangled-name> E		# external name
		  ::= st <type>				# sizeof
		  ::= sr <type> <unqualified-name>	# dependent name
		  ::= sr <type> <unqualified-name> <template-args> */

static void
write_expression (tree expr)
{
  enum tree_code code = TREE_CODE (expr);

  if (TREE_CODE (expr) == TARGET_EXPR)
    {
      expr = TARGET_EXPR_INITIAL (expr);
      code = TREE_CODE (expr);
    }

  /* Skip NOP_EXPR and CONVERT_EXPR.  They can occur when (say) a pointer
     argument is converted (via qualification conversions) to another type.  */
  while (CONVERT_EXPR_CODE_P (code)
	 || code == IMPLICIT_CONV_EXPR
	 || location_wrapper_p (expr)
	 /* Parentheses aren't mangled.  */
	 || code == PAREN_EXPR
	 || code == NON_LVALUE_EXPR
	 || (code == VIEW_CONVERT_EXPR
	     && TREE_CODE (TREE_OPERAND (expr, 0)) == TEMPLATE_PARM_INDEX))
    {
      expr = TREE_OPERAND (expr, 0);
      code = TREE_CODE (expr);
    }

  if (code == BASELINK
      && (!type_unknown_p (expr)
	  || !BASELINK_QUALIFIED_P (expr)))
    {
      expr = BASELINK_FUNCTIONS (expr);
      code = TREE_CODE (expr);
    }

  /* Handle pointers-to-members by making them look like expression
     nodes.  */
  if (code == PTRMEM_CST)
    {
      expr = build_nt (ADDR_EXPR,
		       build_qualified_name (/*type=*/NULL_TREE,
					     PTRMEM_CST_CLASS (expr),
					     PTRMEM_CST_MEMBER (expr),
					     /*template_p=*/false));
      code = TREE_CODE (expr);
    }

  /* Handle template parameters.  */
  if (code == TEMPLATE_TYPE_PARM
      || code == TEMPLATE_TEMPLATE_PARM
      || code == BOUND_TEMPLATE_TEMPLATE_PARM
      || code == TEMPLATE_PARM_INDEX)
    write_template_param (expr);
  /* Handle literals.  */
  else if (TREE_CODE_CLASS (code) == tcc_constant
	   || code == CONST_DECL)
    write_template_arg_literal (expr);
  else if (code == EXCESS_PRECISION_EXPR
	   && TREE_CODE (TREE_OPERAND (expr, 0)) == REAL_CST)
    write_template_arg_literal (fold_convert (TREE_TYPE (expr),
					      TREE_OPERAND (expr, 0)));
  else if (code == PARM_DECL && DECL_ARTIFICIAL (expr))
    {
      gcc_assert (id_equal (DECL_NAME (expr), "this"));
      write_string ("fpT");
    }
  else if (code == PARM_DECL)
    {
      /* A function parameter used in a late-specified return type.  */
      int index = DECL_PARM_INDEX (expr);
      int level = DECL_PARM_LEVEL (expr);
      int delta = G.parm_depth - level + 1;
      gcc_assert (index >= 1);
      write_char ('f');
      if (delta != 0)
	{
	  gcc_checking_assert (delta > 0);
	  if (abi_check (5))
	    {
	      /* Let L be the number of function prototype scopes from the
		 innermost one (in which the parameter reference occurs) up
		 to (and including) the one containing the declaration of
		 the referenced parameter.  If the parameter declaration
		 clause of the innermost function prototype scope has been
		 completely seen, it is not counted (in that case -- which
		 is perhaps the most common -- L can be zero).  */
	      write_char ('L');
	      write_unsigned_number (delta - 1);
	    }
	}
      write_char ('p');
      write_compact_number (index - 1);
    }
  else if (DECL_P (expr))
    {
      write_char ('L');
      write_mangled_name (expr, false);
      write_char ('E');
    }
  else if (TREE_CODE (expr) == SIZEOF_EXPR)
    {
      tree op = TREE_OPERAND (expr, 0);

      if (PACK_EXPANSION_P (op))
	{
    sizeof_pack:
	  if (abi_check (11))
	    {
	      /* sZ rather than szDp.  */
	      write_string ("sZ");
	      write_expression (PACK_EXPANSION_PATTERN (op));
	      return;
	    }
	}

      if (SIZEOF_EXPR_TYPE_P (expr))
	{
	  write_string ("st");
	  write_type (TREE_TYPE (op));
	}
      else if (ARGUMENT_PACK_P (op))
	{
	  tree args = ARGUMENT_PACK_ARGS (op);
	  int length = TREE_VEC_LENGTH (args);
	  if (abi_check (10))
	    {
	      /* Before v19 we wrongly mangled all single pack expansions with
		 sZ, but now only for expressions, as types ICEd (95298).  */
	      if (length == 1)
		{
		  tree arg = TREE_VEC_ELT (args, 0);
		  if (TREE_CODE (arg) == EXPR_PACK_EXPANSION
		      && !abi_check (19))
		    {
		      op = arg;
		      goto sizeof_pack;
		    }
		}

	      /* sP <template-arg>* E # sizeof...(T), size of a captured
		 template parameter pack from an alias template */
	      write_string ("sP");
	      for (int i = 0; i < length; ++i)
		write_template_arg (TREE_VEC_ELT (args, i));
	      write_char ('E');
	    }
	  else
	    {
	      /* In GCC 5 we represented this sizeof wrong, with the effect
		 that we mangled it as the last element of the pack.  */
	      tree arg = TREE_VEC_ELT (args, length-1);
	      if (TYPE_P (op))
		{
		  write_string ("st");
		  write_type (arg);
		}
	      else
		{
		  write_string ("sz");
		  write_expression (arg);
		}
	    }
	}
      else if (TYPE_P (TREE_OPERAND (expr, 0)))
	{
	  write_string ("st");
	  write_type (TREE_OPERAND (expr, 0));
	}
      else
	goto normal_expr;
    }
  else if (TREE_CODE (expr) == ALIGNOF_EXPR)
    {
      if (!ALIGNOF_EXPR_STD_P (expr))
	{
	  if (abi_check (16))
	    {
	      /* We used to mangle __alignof__ like alignof.  */
	      write_string ("u11__alignof__");
	      write_template_arg (TREE_OPERAND (expr, 0));
	      write_char ('E');
	      return;
	    }
	}
      if (TYPE_P (TREE_OPERAND (expr, 0)))
	{
	  write_string ("at");
	  write_type (TREE_OPERAND (expr, 0));
	}
      else
	goto normal_expr;
    }
  else if (code == SCOPE_REF
	   || code == BASELINK)
    {
      tree scope, member;
      if (code == SCOPE_REF)
	{
	  scope = TREE_OPERAND (expr, 0);
	  member = TREE_OPERAND (expr, 1);
	  if (BASELINK_P (member))
	    member = BASELINK_FUNCTIONS (member);
	}
      else
	{
	  scope = BINFO_TYPE (BASELINK_ACCESS_BINFO (expr));
	  member = BASELINK_FUNCTIONS (expr);
	}

      /* If the MEMBER is a real declaration, then the qualifying
	 scope was not dependent.  Ideally, we would not have a
	 SCOPE_REF in those cases, but sometimes we do.  If the second
	 argument is a DECL, then the name must not have been
	 dependent.  */
      if (DECL_P (member))
	write_expression (member);
      else
	{
	  gcc_assert (code != BASELINK || BASELINK_QUALIFIED_P (expr));
	  write_string ("sr");
	  write_type (scope);
	  write_member_name (member);
	}
    }
  else if (INDIRECT_REF_P (expr)
	   && TREE_TYPE (TREE_OPERAND (expr, 0))
	   && TYPE_REF_P (TREE_TYPE (TREE_OPERAND (expr, 0))))
    {
      write_expression (TREE_OPERAND (expr, 0));
    }
  else if (identifier_p (expr))
    {
      /* An operator name appearing as a dependent name needs to be
	 specially marked to disambiguate between a use of the operator
	 name and a use of the operator in an expression.  */
      if (IDENTIFIER_ANY_OP_P (expr))
	write_string ("on");
      write_unqualified_id (expr);
    }
  else if (TREE_CODE (expr) == TEMPLATE_ID_EXPR)
    {
      tree fn = TREE_OPERAND (expr, 0);
      if (!identifier_p (fn))
	fn = OVL_NAME (fn);
      if (IDENTIFIER_ANY_OP_P (fn))
	write_string ("on");
      write_unqualified_id (fn);
      write_template_args (TREE_OPERAND (expr, 1));
    }
  else if (TREE_CODE (expr) == MODOP_EXPR)
    {
      enum tree_code subop = TREE_CODE (TREE_OPERAND (expr, 1));
      const char *name = OVL_OP_INFO (true, subop)->mangled_name;

      write_string (name);
      write_expression (TREE_OPERAND (expr, 0));
      write_expression (TREE_OPERAND (expr, 2));
    }
  else if (code == NEW_EXPR || code == VEC_NEW_EXPR)
    {
      /* ::= [gs] nw <expression>* _ <type> E
	 ::= [gs] nw <expression>* _ <type> <initializer>
	 ::= [gs] na <expression>* _ <type> E
	 ::= [gs] na <expression>* _ <type> <initializer>
	 <initializer> ::= pi <expression>* E  */
      tree placement = TREE_OPERAND (expr, 0);
      tree type = TREE_OPERAND (expr, 1);
      tree nelts = TREE_OPERAND (expr, 2);
      tree init = TREE_OPERAND (expr, 3);
      tree t;

      gcc_assert (code == NEW_EXPR);
      if (TREE_OPERAND (expr, 2))
	code = VEC_NEW_EXPR;

      if (NEW_EXPR_USE_GLOBAL (expr))
	write_string ("gs");

      write_string (OVL_OP_INFO (false, code)->mangled_name);

      for (t = placement; t; t = TREE_CHAIN (t))
	write_expression (TREE_VALUE (t));

      write_char ('_');

      if (nelts)
	{
	  ++processing_template_decl;
	  /* Avoid compute_array_index_type complaints about
	     non-constant nelts.  */
	  tree max = cp_build_binary_op (input_location, MINUS_EXPR,
					 fold_convert (sizetype, nelts),
					 size_one_node,
					 tf_warning_or_error);
	  max = maybe_constant_value (max);
	  tree domain = build_index_type (max);
	  type = build_cplus_array_type (type, domain);
	  --processing_template_decl;
	}
      write_type (type);

      if (init && TREE_CODE (init) == TREE_LIST
	  && DIRECT_LIST_INIT_P (TREE_VALUE (init)))
	write_expression (TREE_VALUE (init));
      else
	{
	  if (init)
	    write_string ("pi");
	  if (init && init != void_node)
	    for (t = init; t; t = TREE_CHAIN (t))
	      write_expression (TREE_VALUE (t));
	  write_char ('E');
	}
    }
  else if (code == DELETE_EXPR || code == VEC_DELETE_EXPR)
    {
      gcc_assert (code == DELETE_EXPR);
      if (DELETE_EXPR_USE_VEC (expr))
	code = VEC_DELETE_EXPR;

      if (DELETE_EXPR_USE_GLOBAL (expr))
	write_string ("gs");

      write_string (OVL_OP_INFO (false, code)->mangled_name);

      write_expression (TREE_OPERAND (expr, 0));
    }
  else if (code == THROW_EXPR)
    {
      tree op = TREE_OPERAND (expr, 0);
      if (op)
	{
	  write_string ("tw");
	  write_expression (op);
	}
      else
	write_string ("tr");
    }
  else if (code == NOEXCEPT_EXPR)
    {
      write_string ("nx");
      write_expression (TREE_OPERAND (expr, 0));
    }
  else if (code == CONSTRUCTOR)
    {
      bool braced_init = BRACE_ENCLOSED_INITIALIZER_P (expr);
      tree etype = TREE_TYPE (expr);

      if (braced_init)
	write_string ("il");
      else
	{
	  write_string ("tl");
	  write_type (etype);
	}

      /* If this is an undigested initializer, mangle it as written.
	 COMPOUND_LITERAL_P doesn't actually distinguish between digested and
	 undigested braced casts, but it should work to use it to distinguish
	 between braced casts in a template signature (undigested) and template
	 parm object values (digested), and all CONSTRUCTORS that get here
	 should be one of those two cases.  */
      bool undigested = braced_init || COMPOUND_LITERAL_P (expr);
      if (undigested || !zero_init_expr_p (expr))
	{
	  /* Convert braced initializer lists to STRING_CSTs so that
	     A<"Foo"> mangles the same as A<{'F', 'o', 'o', 0}> while
	     still using the latter mangling for strings that
	     originated as braced initializer lists.  */
	  expr = braced_lists_to_strings (etype, expr);

	  if (TREE_CODE (expr) == CONSTRUCTOR)
	    {
	      vec<constructor_elt, va_gc> *elts = CONSTRUCTOR_ELTS (expr);
	      unsigned last_nonzero = UINT_MAX;
	      constructor_elt *ce;

	      if (!undigested)
		for (HOST_WIDE_INT i = 0; vec_safe_iterate (elts, i, &ce); ++i)
		  if ((TREE_CODE (etype) == UNION_TYPE
		       && ce->index != first_field (etype))
		      || !zero_init_expr_p (ce->value))
		    last_nonzero = i;

	      if (undigested || last_nonzero != UINT_MAX)
		for (HOST_WIDE_INT i = 0; vec_safe_iterate (elts, i, &ce); ++i)
		  {
		    if (i > last_nonzero)
		      break;
		    if (!undigested && TREE_CODE (etype) == UNION_TYPE)
		      {
			/* Express the active member as a designator.  */
			write_string ("di");
			write_unqualified_name (ce->index);
		      }
		    unsigned reps = 1;
		    if (ce->index && TREE_CODE (ce->index) == RANGE_EXPR)
		      reps = range_expr_nelts (ce->index);
		    if (TREE_CODE (ce->value) == RAW_DATA_CST)
		      {
			gcc_assert (reps == 1);
			unsigned int len = RAW_DATA_LENGTH (ce->value);
			/* If this is the last non-zero element, skip
			   zeros at the end.  */
			if (i == last_nonzero)
			  while (len)
			    {
			      if (RAW_DATA_POINTER (ce->value)[len - 1])
				break;
			      --len;
			    }
			tree valtype = TREE_TYPE (ce->value);
			for (unsigned int i = 0; i < len; ++i)
			  {
			    write_char ('L');
			    write_type (valtype);
			    unsigned HOST_WIDE_INT v;
			    if (!TYPE_UNSIGNED (valtype)
				&& TYPE_PRECISION (valtype) == BITS_PER_UNIT
				&& RAW_DATA_SCHAR_ELT (ce->value, i) < 0)
			      {
				write_char ('n');
				v = -RAW_DATA_SCHAR_ELT (ce->value, i);
			      }
			    else
			      v = RAW_DATA_UCHAR_ELT (ce->value, i);
			    write_unsigned_number (v);
			    write_char ('E');
			  }
		      }
		    else
		      for (unsigned j = 0; j < reps; ++j)
			write_expression (ce->value);
		  }
	    }
	  else
	    {
	      gcc_assert (TREE_CODE (expr) == STRING_CST);
	      write_expression (expr);
	    }
	}
      write_char ('E');
    }
  else if (code == LAMBDA_EXPR)
    {
      /* [temp.over.link] Two lambda-expressions are never considered
	 equivalent.

	 So just use the closure type mangling.  */
      write_char ('L');
      write_type (LAMBDA_EXPR_CLOSURE (expr));
      write_char ('E');
    }
  else if (code == REQUIRES_EXPR)
    write_requires_expr (expr);
  else if (dependent_name (expr))
    {
      tree name = dependent_name (expr);
      if (IDENTIFIER_ANY_OP_P (name))
	{
	  if (abi_check (16))
	    write_string ("on");
	}
      write_unqualified_id (name);
    }
  else
    {
    normal_expr:
      int i, len;
      const char *name;

      /* When we bind a variable or function to a non-type template
	 argument with reference type, we create an ADDR_EXPR to show
	 the fact that the entity's address has been taken.  But, we
	 don't actually want to output a mangling code for the `&'.  */
      if (TREE_CODE (expr) == ADDR_EXPR
	  && TREE_TYPE (expr)
	  && TYPE_REF_P (TREE_TYPE (expr)))
	{
	  expr = TREE_OPERAND (expr, 0);
	  if (DECL_P (expr))
	    {
	      write_expression (expr);
	      return;
	    }

	  code = TREE_CODE (expr);
	}

      if (code == COMPONENT_REF)
	{
	  tree ob = TREE_OPERAND (expr, 0);

	  if (TREE_CODE (ob) == ARROW_EXPR)
	    {
	      write_string (OVL_OP_INFO (false, code)->mangled_name);
	      ob = TREE_OPERAND (ob, 0);
	      write_expression (ob);
	    }
	  else if (write_base_ref (expr))
	    return;
	  else if (!is_dummy_object (ob))
	    {
	      write_string ("dt");
	      write_expression (ob);
	    }
	  /* else, for a non-static data member with no associated object (in
	     unevaluated context), use the unresolved-name mangling.  */

	  write_member_name (TREE_OPERAND (expr, 1));
	  return;
	}

      /* If it wasn't any of those, recursively expand the expression.  */
      name = OVL_OP_INFO (false, code)->mangled_name;

      /* We used to mangle const_cast and static_cast like a C cast.  */
      if (code == CONST_CAST_EXPR
	  || code == STATIC_CAST_EXPR)
	{
	  if (!abi_check (6))
	    name = OVL_OP_INFO (false, CAST_EXPR)->mangled_name;
	}

      if (name == NULL)
	{
	  switch (code)
	    {
	    case TRAIT_EXPR:
	      error ("use of built-in trait %qE in function signature; "
		     "use library traits instead", expr);
	      break;

	    default:
	      sorry ("mangling %C", code);
	      break;
	    }
	  return;
	}
      else
	write_string (name);

      switch (code)
	{
	case CALL_EXPR:
	  {
	    tree fn = CALL_EXPR_FN (expr);

	    if (TREE_CODE (fn) == ADDR_EXPR)
	      fn = TREE_OPERAND (fn, 0);

	    /* Mangle a dependent name as the name, not whatever happens to
	       be the first function in the overload set.  */
	    if (OVL_P (fn)
		&& type_dependent_expression_p_push (expr))
	      fn = OVL_NAME (fn);

	    write_expression (fn);
	  }

	  for (i = 0; i < call_expr_nargs (expr); ++i)
	    write_expression (CALL_EXPR_ARG (expr, i));
	  write_char ('E');
	  break;

	case CAST_EXPR:
	  write_type (TREE_TYPE (expr));
	  if (list_length (TREE_OPERAND (expr, 0)) == 1)
	    write_expression (TREE_VALUE (TREE_OPERAND (expr, 0)));
	  else
	    {
	      tree args = TREE_OPERAND (expr, 0);
	      write_char ('_');
	      for (; args; args = TREE_CHAIN (args))
		write_expression (TREE_VALUE (args));
	      write_char ('E');
	    }
	  break;

	case DYNAMIC_CAST_EXPR:
	case REINTERPRET_CAST_EXPR:
	case STATIC_CAST_EXPR:
	case CONST_CAST_EXPR:
	  write_type (TREE_TYPE (expr));
	  write_expression (TREE_OPERAND (expr, 0));
	  break;

	case PREINCREMENT_EXPR:
	case PREDECREMENT_EXPR:
	  if (abi_check (6))
	    write_char ('_');
	  /* Fall through.  */

	default:
	  /* In the middle-end, some expressions have more operands than
	     they do in templates (and mangling).  */
	  len = cp_tree_operand_length (expr);

	  for (i = 0; i < len; ++i)
	    {
	      tree operand = TREE_OPERAND (expr, i);
	      /* As a GNU extension, the middle operand of a
		 conditional may be omitted.  Since expression
		 manglings are supposed to represent the input token
		 stream, there's no good way to mangle such an
		 expression without extending the C++ ABI.  */
	      if (code == COND_EXPR && i == 1 && !operand)
		{
		  error ("omitted middle operand to %<?:%> operand "
			 "cannot be mangled");
		  continue;
		}
	      else if (FOLD_EXPR_P (expr))
		{
		  /* The first 'operand' of a fold-expression is the operator
		     that it folds over.  */
		  if (i == 0)
		    {
		      int fcode = TREE_INT_CST_LOW (operand);
		      write_string (OVL_OP_INFO (false, fcode)->mangled_name);
		      continue;
		    }
		  else if (code == BINARY_LEFT_FOLD_EXPR)
		    {
		      /* The order of operands of the binary left and right
			 folds is the same, but we want to mangle them in
			 lexical order, i.e. non-pack first.  */
		      if (i == 1)
			operand = FOLD_EXPR_INIT (expr);
		      else
			operand = FOLD_EXPR_PACK (expr);
		    }
		  if (PACK_EXPANSION_P (operand))
		    operand = PACK_EXPANSION_PATTERN (operand);
		}
	      write_expression (operand);
	    }
	}
    }
}

/* Literal subcase of non-terminal <template-arg>.

     "Literal arguments, e.g. "A<42L>", are encoded with their type
     and value. Negative integer values are preceded with "n"; for
     example, "A<-42L>" becomes "1AILln42EE". The bool value false is
     encoded as 0, true as 1."  */

static void
write_template_arg_literal (const tree value)
{
  if (TREE_CODE (value) == STRING_CST)
    /* Temporarily mangle strings as braced initializer lists.  */
    write_string ("tl");
  else
    write_char ('L');

  tree valtype = TREE_TYPE (value);
  write_type (valtype);

  /* Write a null member pointer value as (type)0, regardless of its
     real representation.  */
  if (null_member_pointer_value_p (value))
    write_integer_cst (integer_zero_node);
  else
    switch (TREE_CODE (value))
      {
      case CONST_DECL:
	write_integer_cst (DECL_INITIAL (value));
	break;

      case INTEGER_CST:
	gcc_assert (!same_type_p (TREE_TYPE (value), boolean_type_node)
		    || integer_zerop (value) || integer_onep (value));
	if (!(abi_version_at_least (14)
	      && NULLPTR_TYPE_P (TREE_TYPE (value))))
	  write_integer_cst (value);
	break;

      case REAL_CST:
	write_real_cst (value);
	break;

      case COMPLEX_CST:
	if (TREE_CODE (TREE_REALPART (value)) == INTEGER_CST
	    && TREE_CODE (TREE_IMAGPART (value)) == INTEGER_CST)
	  {
	    write_integer_cst (TREE_REALPART (value));
	    write_char ('_');
	    write_integer_cst (TREE_IMAGPART (value));
	  }
	else if (TREE_CODE (TREE_REALPART (value)) == REAL_CST
		 && TREE_CODE (TREE_IMAGPART (value)) == REAL_CST)
	  {
	    write_real_cst (TREE_REALPART (value));
	    write_char ('_');
	    write_real_cst (TREE_IMAGPART (value));
	  }
	else
	  gcc_unreachable ();
	break;

      case STRING_CST:
	{
	  /* Mangle strings the same as braced initializer lists.  */
	  unsigned n = TREE_STRING_LENGTH (value);
	  const char *str = TREE_STRING_POINTER (value);

	  /* Count the number of trailing nuls and subtract them from
	     STRSIZE because they don't need to be mangled.  */
	  for (const char *p = str + n - 1; ; --p)
	    {
	      if (*p || p == str)
		{
		  n -= str + n - !!*p - p;
		  break;
		}
	    }
	  tree eltype = TREE_TYPE (valtype);
	  for (const char *p = str; n--; ++p)
	    {
	      write_char ('L');
	      write_type (eltype);
	      write_unsigned_number (*(const unsigned char*)p);
	      write_string ("E");
	    }
	  break;
	}

      default:
	gcc_unreachable ();
      }

  write_char ('E');
}

/* Non-terminal <template-arg>.

     <template-arg> ::= <type>				# type
		    ::= L <type> </value/ number> E	# literal
		    ::= LZ <name> E			# external name
		    ::= X <expression> E		# expression  */

static void
write_template_arg (tree node)
{
  enum tree_code code = TREE_CODE (node);

  MANGLE_TRACE_TREE ("template-arg", node);

  /* A template template parameter's argument list contains TREE_LIST
     nodes of which the value field is the actual argument.  */
  if (code == TREE_LIST)
    {
      node = TREE_VALUE (node);
      /* If it's a decl, deal with its type instead.  */
      if (DECL_P (node))
	{
	  node = TREE_TYPE (node);
	  code = TREE_CODE (node);
	}
    }

  if (VAR_P (node) && DECL_NTTP_OBJECT_P (node))
    /* We want to mangle the argument, not the var we stored it in.  */
    node = tparm_object_argument (node);

  /* Strip a conversion added by convert_nontype_argument.  */
  if (TREE_CODE (node) == IMPLICIT_CONV_EXPR)
    node = TREE_OPERAND (node, 0);
  if (REFERENCE_REF_P (node))
    node = TREE_OPERAND (node, 0);
  if (TREE_CODE (node) == NOP_EXPR
      && TYPE_REF_P (TREE_TYPE (node)))
    {
      /* Template parameters can be of reference type. To maintain
	 internal consistency, such arguments use a conversion from
	 address of object to reference type.  */
      gcc_assert (TREE_CODE (TREE_OPERAND (node, 0)) == ADDR_EXPR);
      node = TREE_OPERAND (TREE_OPERAND (node, 0), 0);
    }

  if (TREE_CODE (node) == BASELINK
      && !type_unknown_p (node))
    {
      /* Before v6 we wrongly wrapped a class-scope function in X/E.  */
      if (abi_check (6))
	node = BASELINK_FUNCTIONS (node);
    }

  if (ARGUMENT_PACK_P (node))
    {
      /* Expand the template argument pack. */
      tree args = ARGUMENT_PACK_ARGS (node);
      int i, length = TREE_VEC_LENGTH (args);
      if (abi_check (6))
	write_char ('J');
      else
	write_char ('I');
      for (i = 0; i < length; ++i)
        write_template_arg (TREE_VEC_ELT (args, i));
      write_char ('E');
    }
  else if (TYPE_P (node))
    write_type (node);
  else if (code == TEMPLATE_DECL)
    /* A template appearing as a template arg is a template template arg.  */
    write_template_template_arg (node);
  else if ((TREE_CODE_CLASS (code) == tcc_constant && code != PTRMEM_CST)
	   || code == CONST_DECL
	   || null_member_pointer_value_p (node))
    write_template_arg_literal (node);
  else if (code == EXCESS_PRECISION_EXPR
	   && TREE_CODE (TREE_OPERAND (node, 0)) == REAL_CST)
    write_template_arg_literal (fold_convert (TREE_TYPE (node),
					      TREE_OPERAND (node, 0)));
  else if (DECL_P (node))
    {
      write_char ('L');
      /* Until ABI version 3, the underscore before the mangled name
	 was incorrectly omitted.  */
      if (!abi_check (3))
	write_char ('Z');
      else
	write_string ("_Z");
      write_encoding (node);
      write_char ('E');
    }
  else
    {
      /* Template arguments may be expressions.  */
      write_char ('X');
      write_expression (node);
      write_char ('E');
    }
}

/*  <template-template-arg>
			::= <name>
			::= <substitution>  */

static void
write_template_template_arg (const tree decl)
{
  MANGLE_TRACE_TREE ("template-template-arg", decl);

  if (find_substitution (decl))
    return;
  write_name (decl, /*ignore_local_scope=*/0);
  add_substitution (decl);
}


/* Non-terminal <array-type>.  TYPE is an ARRAY_TYPE.

     <array-type> ::= A [</dimension/ number>] _ </element/ type>
		  ::= A <expression> _ </element/ type>

     "Array types encode the dimension (number of elements) and the
     element type.  For variable length arrays, the dimension (but not
     the '_' separator) is omitted."
     Note that for flexible array members, like for other arrays of
     unspecified size, the dimension is also omitted.  */

static void
write_array_type (const tree type)
{
  write_char ('A');
  if (TYPE_DOMAIN (type))
    {
      tree index_type;

      index_type = TYPE_DOMAIN (type);
      /* The INDEX_TYPE gives the upper and lower bounds of the array.
	 It's null for flexible array members which have no upper bound
	 (this is a change from GCC 5 and prior where such members were
	 incorrectly mangled as zero-length arrays).  */
      if (tree max = TYPE_MAX_VALUE (index_type))
	{
	  if (TREE_CODE (max) == INTEGER_CST)
	    {
	      /* The ABI specifies that we should mangle the number of
		 elements in the array, not the largest allowed index.  */
	      offset_int wmax = wi::to_offset (max) + 1;
	      /* Truncate the result - this will mangle [0, SIZE_INT_MAX]
		 number of elements as zero.  */
	      wmax = wi::zext (wmax, TYPE_PRECISION (TREE_TYPE (max)));
	      gcc_assert (wi::fits_uhwi_p (wmax));
	      write_unsigned_number (wmax.to_uhwi ());
	    }
	  else
	    {
	      gcc_checking_assert (TREE_CODE (max) == MINUS_EXPR
				   && integer_onep (TREE_OPERAND (max, 1)));
	      max = TREE_OPERAND (max, 0);
	      write_expression (max);
	    }
	}
    }
  write_char ('_');
  write_type (TREE_TYPE (type));
}

/* Non-terminal <pointer-to-member-type> for pointer-to-member
   variables.  TYPE is a pointer-to-member POINTER_TYPE.

     <pointer-to-member-type> ::= M </class/ type> </member/ type>  */

static void
write_pointer_to_member_type (const tree type)
{
  write_char ('M');
  write_type (TYPE_PTRMEM_CLASS_TYPE (type));
  write_type (TYPE_PTRMEM_POINTED_TO_TYPE (type));
}

/* Non-terminal <template-param>.  PARM is a TEMPLATE_TYPE_PARM,
   TEMPLATE_TEMPLATE_PARM, BOUND_TEMPLATE_TEMPLATE_PARM or a
   TEMPLATE_PARM_INDEX.

     <template-param> ::= T </parameter/ number> _  */

static void
write_template_param (const tree parm)
{
  int parm_index;
  int level;

  MANGLE_TRACE_TREE ("template-parm", parm);

  switch (TREE_CODE (parm))
    {
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
      parm_index = TEMPLATE_TYPE_IDX (parm);
      level = TEMPLATE_TYPE_LEVEL (parm);
      break;

    case TEMPLATE_PARM_INDEX:
      parm_index = TEMPLATE_PARM_IDX (parm);
      level = TEMPLATE_PARM_LEVEL (parm);
      break;

    default:
      gcc_unreachable ();
    }

  write_char ('T');
  if (level > 1)
    {
      if (abi_check (19))
	{
	  write_char ('L');
	  write_compact_number (level - 1);
	}
    }
  /* NUMBER as it appears in the mangling is (-1)-indexed, with the
     earliest template param denoted by `_'.  */
  write_compact_number (parm_index);
}

/*  <template-template-param>
			::= <template-param>
			::= <substitution>  */

static void
write_template_template_param (const tree parm)
{
  tree templ = NULL_TREE;

  /* PARM, a TEMPLATE_TEMPLATE_PARM, is an instantiation of the
     template template parameter.  The substitution candidate here is
     only the template.  */
  if (TREE_CODE (parm) == BOUND_TEMPLATE_TEMPLATE_PARM)
    {
      templ
	= TI_TEMPLATE (TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (parm));
      if (find_substitution (templ))
	return;
    }

  /* <template-param> encodes only the template parameter position,
     not its template arguments, which is fine here.  */
  write_template_param (parm);
  if (templ)
    add_substitution (templ);
}

/* Non-terminal <substitution>.

      <substitution> ::= S <seq-id> _
		     ::= S_  */

static void
write_substitution (const int seq_id)
{
  MANGLE_TRACE ("substitution", "");

  write_char ('S');
  if (seq_id > 0)
    write_number (seq_id - 1, /*unsigned=*/1, 36);
  write_char ('_');
}

/* Start mangling ENTITY.  */

static inline void
start_mangling (const tree entity)
{
  G = {};
  G.entity = entity;
  obstack_free (&name_obstack, name_base);
  mangle_obstack = &name_obstack;
  name_base = obstack_alloc (&name_obstack, 0);
}

/* Done with mangling.  Release the data.  */

static void
finish_mangling_internal (void)
{
  /* Clear all the substitutions.  */
  vec_safe_truncate (G.substitutions, 0);

  if (G.mod)
    mangle_module_fini ();

  /* Null-terminate the string.  */
  write_char ('\0');
}


/* Like finish_mangling_internal, but return the mangled string.  */

static inline const char *
finish_mangling (void)
{
  finish_mangling_internal ();
  return (const char *) obstack_finish (mangle_obstack);
}

/* Like finish_mangling_internal, but return an identifier.  */

static tree
finish_mangling_get_identifier (void)
{
  finish_mangling_internal ();
  /* Don't obstack_finish here, and the next start_mangling will
     remove the identifier.  */
  return get_identifier ((const char *) obstack_base (mangle_obstack));
}

/* Initialize data structures for mangling.  */

void
init_mangle (void)
{
  gcc_obstack_init (&name_obstack);
  name_base = obstack_alloc (&name_obstack, 0);
  vec_alloc (G.substitutions, 0);

  /* Cache these identifiers for quick comparison when checking for
     standard substitutions.  */
  subst_identifiers[SUBID_ALLOCATOR] = get_identifier ("allocator");
  subst_identifiers[SUBID_BASIC_STRING] = get_identifier ("basic_string");
  subst_identifiers[SUBID_CHAR_TRAITS] = get_identifier ("char_traits");
  subst_identifiers[SUBID_BASIC_ISTREAM] = get_identifier ("basic_istream");
  subst_identifiers[SUBID_BASIC_OSTREAM] = get_identifier ("basic_ostream");
  subst_identifiers[SUBID_BASIC_IOSTREAM] = get_identifier ("basic_iostream");
}

/* Generate a mangling for MODULE's global initializer fn.  */

tree
mangle_module_global_init (int module)
{
  start_mangling (NULL_TREE);

  write_string ("_ZGI");
  write_module (module, true);

  return finish_mangling_get_identifier ();
}

/* Generate the mangled name of DECL.  */

static tree
mangle_decl_string (const tree decl)
{
  tree result;
  tree saved_fn = current_function_decl;

  /* We shouldn't be trying to mangle an uninstantiated template.  */
  gcc_assert (!type_dependent_expression_p (decl));

  current_function_decl = NULL_TREE;
  iloc_sentinel ils (DECL_SOURCE_LOCATION (decl));

  start_mangling (decl);

  if (TREE_CODE (decl) == TYPE_DECL)
    write_type (TREE_TYPE (decl));
  else
    write_mangled_name (decl, true);

  result = finish_mangling_get_identifier ();
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_decl_string = '%s'\n\n",
	     IDENTIFIER_POINTER (result));

  current_function_decl = saved_fn;
  return result;
}

/* Return an identifier for the external mangled name of DECL.  */

static tree
get_mangled_id (tree decl)
{
  tree id = mangle_decl_string (decl);
  return targetm.mangle_decl_assembler_name (decl, id);
}

/* Create an identifier for the external mangled name of DECL.  */

void
mangle_decl (const tree decl)
{
  tree id;
  bool dep;

  /* Don't bother mangling uninstantiated templates.  */
  ++processing_template_decl;
  if (TREE_CODE (decl) == TYPE_DECL)
    dep = dependent_type_p (TREE_TYPE (decl));
  else
    dep = (DECL_LANG_SPECIFIC (decl) && DECL_TEMPLATE_INFO (decl)
	   && any_dependent_template_arguments_p (DECL_TI_ARGS (decl)));
  --processing_template_decl;
  if (dep)
    return;

  /* During LTO we keep mangled names of TYPE_DECLs for ODR type merging.
     It is not needed to assign names to anonymous namespace, but we use the
     "<anon>" marker to be able to tell if type is C++ ODR type or type
     produced by other language.  */
  if (TREE_CODE (decl) == TYPE_DECL
      && TYPE_STUB_DECL (TREE_TYPE (decl))
      && !TREE_PUBLIC (TYPE_STUB_DECL (TREE_TYPE (decl))))
    id = get_identifier ("<anon>");
  else
    {
      gcc_assert (TREE_CODE (decl) != TYPE_DECL
		  || !no_linkage_check (TREE_TYPE (decl), true));
      if (abi_version_at_least (10))
	if (tree fn = decl_function_context (decl))
	  maybe_check_abi_tags (fn, decl);
      id = get_mangled_id (decl);
    }
  SET_DECL_ASSEMBLER_NAME (decl, id);

  if (G.need_cxx17_warning
      && (TREE_PUBLIC (decl) || DECL_REALLY_EXTERN (decl)))
    warning_at (DECL_SOURCE_LOCATION (decl), OPT_Wnoexcept_type,
		"mangled name for %qD will change in C++17 because the "
		"exception specification is part of a function type",
		decl);

  if (id != DECL_NAME (decl)
      /* Don't do this for a fake symbol we aren't going to emit anyway.  */
      && TREE_CODE (decl) != TYPE_DECL
      && !DECL_MAYBE_IN_CHARGE_CDTOR_P (decl))
    {
      int save_ver = flag_abi_version;
      tree id2 = NULL_TREE;

      if (!DECL_REALLY_EXTERN (decl))
	{
	  record_mangling (decl, G.need_abi_warning);

	  if (!G.need_abi_warning)
	    return;

	  flag_abi_version = flag_abi_compat_version;
	  id2 = mangle_decl_string (decl);
	  id2 = targetm.mangle_decl_assembler_name (decl, id2);
	  flag_abi_version = save_ver;

	  if (id2 != id)
	    note_mangling_alias (decl, id2);
	}

      if (warn_abi)
	{
	  const char fabi_version[] = "-fabi-version";

	  if (flag_abi_compat_version != warn_abi_version
	      || id2 == NULL_TREE)
	    {
	      flag_abi_version = warn_abi_version;
	      id2 = mangle_decl_string (decl);
	      id2 = targetm.mangle_decl_assembler_name (decl, id2);
	    }
	  flag_abi_version = save_ver;

	  if (id2 == id)
	    /* OK.  */;
	  else if (warn_abi_version != 0
		   && abi_version_at_least (warn_abi_version))
	    warning_at (DECL_SOURCE_LOCATION (G.entity), OPT_Wabi,
			"the mangled name of %qD changed between "
			"%<%s=%d%> (%qD) and %<%s=%d%> (%qD)",
			G.entity, fabi_version, warn_abi_version, id2,
			fabi_version, save_ver, id);
	  else
	    warning_at (DECL_SOURCE_LOCATION (G.entity), OPT_Wabi,
			"the mangled name of %qD changes between "
			"%<%s=%d%> (%qD) and %<%s=%d%> (%qD)",
			G.entity, fabi_version, save_ver, id,
			fabi_version, warn_abi_version, id2);
	}

      flag_abi_version = save_ver;
    }
}

/* Generate the mangled representation of TYPE.  */

const char *
mangle_type_string (const tree type)
{
  const char *result;

  start_mangling (type);
  write_type (type);
  result = finish_mangling ();
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_type_string = '%s'\n\n", result);
  return result;
}

/* Create an identifier for the mangled name of a special component
   for belonging to TYPE.  CODE is the ABI-specified code for this
   component.  */

static tree
mangle_special_for_type (const tree type, const char *code)
{
  tree result;

  /* We don't have an actual decl here for the special component, so
     we can't just process the <encoded-name>.  Instead, fake it.  */
  start_mangling (type);

  /* Start the mangling.  */
  write_string ("_Z");
  write_string (code);

  /* Add the type.  */
  write_type (type);
  result = finish_mangling_get_identifier ();

  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_special_for_type = %s\n\n",
	     IDENTIFIER_POINTER (result));

  return result;
}

/* Create an identifier for the mangled representation of the typeinfo
   structure for TYPE.  */

tree
mangle_typeinfo_for_type (const tree type)
{
  return mangle_special_for_type (type, "TI");
}

/* Create an identifier for the mangled name of the NTBS containing
   the mangled name of TYPE.  */

tree
mangle_typeinfo_string_for_type (const tree type)
{
  return mangle_special_for_type (type, "TS");
}

/* Create an identifier for the mangled name of the vtable for TYPE.  */

tree
mangle_vtbl_for_type (const tree type)
{
  return mangle_special_for_type (type, "TV");
}

/* Returns an identifier for the mangled name of the VTT for TYPE.  */

tree
mangle_vtt_for_type (const tree type)
{
  return mangle_special_for_type (type, "TT");
}

/* Returns an identifier for the mangled name of the decomposition
   artificial variable DECL.  DECLS is the vector of the VAR_DECLs
   for the identifier-list.  */

tree
mangle_decomp (const tree decl, vec<tree> &decls)
{
  gcc_assert (!type_dependent_expression_p (decl));

  location_t saved_loc = input_location;
  input_location = DECL_SOURCE_LOCATION (decl);

  check_abi_tags (decl);
  start_mangling (decl);
  write_string ("_Z");

  tree context = decl_mangling_context (decl);
  gcc_assert (context != NULL_TREE);

  bool nested = false;
  bool local = false;
  if (DECL_NAMESPACE_STD_P (context))
    write_string ("St");
  else if (TREE_CODE (context) == FUNCTION_DECL)
    {
      local = true;
      write_char ('Z');
      write_encoding (context);
      write_char ('E');
    }
  else if (context != global_namespace)
    {
      nested = true;
      write_char ('N');
      write_prefix (context);
    }

  write_string ("DC");
  unsigned int i;
  tree d;
  FOR_EACH_VEC_ELT (decls, i, d)
    write_unqualified_name (d);
  write_char ('E');

  if (tree tags = get_abi_tags (decl))
    {
      /* We didn't emit ABI tags for structured bindings before ABI 19.  */
      if (!G.need_abi_warning
	  && TREE_PUBLIC (decl)
	  && abi_warn_or_compat_version_crosses (19))
	G.need_abi_warning = 1;

      if (abi_version_at_least (19))
	write_abi_tags (tags);
    }

  if (nested)
    write_char ('E');
  else if (local && DECL_DISCRIMINATOR_P (decl))
    write_discriminator (discriminator_for_local_entity (decl));

  tree id = finish_mangling_get_identifier ();
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_decomp = '%s'\n\n",
             IDENTIFIER_POINTER (id));

  input_location = saved_loc;

  if (warn_abi && G.need_abi_warning)
    {
      const char fabi_version[] = "-fabi-version";
      tree id2 = id;
      int save_ver = flag_abi_version;

      if (flag_abi_version != warn_abi_version)
	{
	  flag_abi_version = warn_abi_version;
	  id2 = mangle_decomp (decl, decls);
	  flag_abi_version = save_ver;
	}

      if (id2 == id)
	/* OK.  */;
      else if (warn_abi_version != 0
	       && abi_version_at_least (warn_abi_version))
	warning_at (DECL_SOURCE_LOCATION (G.entity), OPT_Wabi,
		    "the mangled name of %qD changed between "
		    "%<%s=%d%> (%qD) and %<%s=%d%> (%qD)",
		    G.entity, fabi_version, warn_abi_version, id2,
		    fabi_version, save_ver, id);
      else
	warning_at (DECL_SOURCE_LOCATION (G.entity), OPT_Wabi,
		    "the mangled name of %qD changes between "
		    "%<%s=%d%> (%qD) and %<%s=%d%> (%qD)",
		    G.entity, fabi_version, save_ver, id,
		    fabi_version, warn_abi_version, id2);
    }

  return id;
}

/* Return an identifier for a construction vtable group.  TYPE is
   the most derived class in the hierarchy; BINFO is the base
   subobject for which this construction vtable group will be used.

   This mangling isn't part of the ABI specification; in the ABI
   specification, the vtable group is dumped in the same COMDAT as the
   main vtable, and is referenced only from that vtable, so it doesn't
   need an external name.  For binary formats without COMDAT sections,
   though, we need external names for the vtable groups.

   We use the production

    <special-name> ::= CT <type> <offset number> _ <base type>  */

tree
mangle_ctor_vtbl_for_type (const tree type, const tree binfo)
{
  tree result;

  start_mangling (type);

  write_string ("_Z");
  write_string ("TC");
  write_type (type);
  write_integer_cst (BINFO_OFFSET (binfo));
  write_char ('_');
  write_type (BINFO_TYPE (binfo));

  result = finish_mangling_get_identifier ();
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_ctor_vtbl_for_type = %s\n\n",
	     IDENTIFIER_POINTER (result));
  return result;
}

/* Mangle a this pointer or result pointer adjustment.

   <call-offset> ::= h <fixed offset number> _
		 ::= v <fixed offset number> _ <virtual offset number> _ */

static void
mangle_call_offset (const tree fixed_offset, const tree virtual_offset)
{
  write_char (virtual_offset ? 'v' : 'h');

  /* For either flavor, write the fixed offset.  */
  write_integer_cst (fixed_offset);
  write_char ('_');

  /* For a virtual thunk, add the virtual offset.  */
  if (virtual_offset)
    {
      write_integer_cst (virtual_offset);
      write_char ('_');
    }
}

/* Return an identifier for the mangled name of a this-adjusting or
   covariant thunk to FN_DECL.  FIXED_OFFSET is the initial adjustment
   to this used to find the vptr.  If VIRTUAL_OFFSET is non-NULL, this
   is a virtual thunk, and it is the vtbl offset in
   bytes. THIS_ADJUSTING is nonzero for a this adjusting thunk and
   zero for a covariant thunk. Note, that FN_DECL might be a covariant
   thunk itself. A covariant thunk name always includes the adjustment
   for the this pointer, even if there is none.

   <special-name> ::= T <call-offset> <base encoding>
		  ::= Tc <this_adjust call-offset> <result_adjust call-offset>
					<base encoding>  */

tree
mangle_thunk (tree fn_decl, const int this_adjusting, tree fixed_offset,
	      tree virtual_offset, tree thunk)
{
  tree result;

  if (abi_version_at_least (11))
    maybe_check_abi_tags (fn_decl, thunk, 11);

  start_mangling (fn_decl);

  write_string ("_Z");
  write_char ('T');

  if (!this_adjusting)
    {
      /* Covariant thunk with no this adjustment */
      write_char ('c');
      mangle_call_offset (integer_zero_node, NULL_TREE);
      mangle_call_offset (fixed_offset, virtual_offset);
    }
  else if (!DECL_THUNK_P (fn_decl))
    /* Plain this adjusting thunk.  */
    mangle_call_offset (fixed_offset, virtual_offset);
  else
    {
      /* This adjusting thunk to covariant thunk.  */
      write_char ('c');
      mangle_call_offset (fixed_offset, virtual_offset);
      fixed_offset = ssize_int (THUNK_FIXED_OFFSET (fn_decl));
      virtual_offset = THUNK_VIRTUAL_OFFSET (fn_decl);
      if (virtual_offset)
	virtual_offset = BINFO_VPTR_FIELD (virtual_offset);
      mangle_call_offset (fixed_offset, virtual_offset);
      fn_decl = THUNK_TARGET (fn_decl);
    }

  /* Scoped name.  */
  write_encoding (fn_decl);

  result = finish_mangling_get_identifier ();
  if (DEBUG_MANGLE)
    fprintf (stderr, "mangle_thunk = %s\n\n", IDENTIFIER_POINTER (result));
  return result;
}

/* Handle ABI backwards compatibility for past bugs where we didn't call
   check_abi_tags in places where it's needed: call check_abi_tags and warn if
   it makes a difference.  If FOR_DECL is non-null, it's the declaration
   that we're actually trying to mangle; if it's null, we're mangling the
   guard variable for T.  */

static void
maybe_check_abi_tags (tree t, tree for_decl, int ver)
{
  if (DECL_ASSEMBLER_NAME_SET_P (t))
    return;

  tree oldtags = get_abi_tags (t);

  mangle_decl (t);

  tree newtags = get_abi_tags (t);
  if (newtags && newtags != oldtags
      && abi_version_crosses (ver))
    {
      if (for_decl && DECL_THUNK_P (for_decl))
	warning_at (DECL_SOURCE_LOCATION (t), OPT_Wabi,
		    "the mangled name of a thunk for %qD changes between "
		    "%<-fabi-version=%d%> and %<-fabi-version=%d%>",
		    t, flag_abi_version, warn_abi_version);
      else if (for_decl)
	warning_at (DECL_SOURCE_LOCATION (for_decl), OPT_Wabi,
		    "the mangled name of %qD changes between "
		    "%<-fabi-version=%d%> and %<-fabi-version=%d%>",
		    for_decl, flag_abi_version, warn_abi_version);
      else
	warning_at (DECL_SOURCE_LOCATION (t), OPT_Wabi,
		    "the mangled name of the initialization guard variable "
		    "for %qD changes between %<-fabi-version=%d%> and "
		    "%<-fabi-version=%d%>",
		    t, flag_abi_version, warn_abi_version);
    }
}

/* Write out the appropriate string for this variable when generating
   another mangled name based on this one.  */

static void
write_guarded_var_name (const tree variable)
{
  if (DECL_NAME (variable)
      && startswith (IDENTIFIER_POINTER (DECL_NAME (variable)), "_ZGR"))
    /* The name of a guard variable for a reference temporary should refer
       to the reference, not the temporary.  */
    write_string (IDENTIFIER_POINTER (DECL_NAME (variable)) + 4);
  else if (DECL_DECOMPOSITION_P (variable)
	   && DECL_NAME (variable) == NULL_TREE
	   && startswith (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (variable)),
			  "_Z"))
    /* The name of a guard variable for a structured binding needs special
       casing.  */
    write_string (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (variable)) + 2);
  else
    write_name (variable, /*ignore_local_scope=*/0);
}

/* Return an identifier for the name of an initialization guard
   variable for indicated VARIABLE.  */

tree
mangle_guard_variable (const tree variable)
{
  if (abi_version_at_least (10))
    maybe_check_abi_tags (variable);
  start_mangling (variable);
  write_string ("_ZGV");
  write_guarded_var_name (variable);
  return finish_mangling_get_identifier ();
}

/* Return an identifier for the name of a thread_local initialization
   function for VARIABLE.  */

tree
mangle_tls_init_fn (const tree variable)
{
  check_abi_tags (variable);
  start_mangling (variable);
  write_string ("_ZTH");
  write_guarded_var_name (variable);
  return finish_mangling_get_identifier ();
}

/* Return an identifier for the name of a thread_local wrapper
   function for VARIABLE.  */

#define TLS_WRAPPER_PREFIX "_ZTW"

tree
mangle_tls_wrapper_fn (const tree variable)
{
  check_abi_tags (variable);
  start_mangling (variable);
  write_string (TLS_WRAPPER_PREFIX);
  write_guarded_var_name (variable);
  return finish_mangling_get_identifier ();
}

/* Return true iff FN is a thread_local wrapper function.  */

bool
decl_tls_wrapper_p (const tree fn)
{
  if (TREE_CODE (fn) != FUNCTION_DECL)
    return false;
  tree name = DECL_NAME (fn);
  return startswith (IDENTIFIER_POINTER (name), TLS_WRAPPER_PREFIX);
}

/* Return an identifier for the name of a temporary variable used to
   initialize a static reference.  This is now part of the ABI.  */

tree
mangle_ref_init_variable (const tree variable)
{
  start_mangling (variable);
  write_string ("_ZGR");
  check_abi_tags (variable);
  write_guarded_var_name (variable);
  /* Avoid name clashes with aggregate initialization of multiple
     references at once.  */
  write_compact_number (current_ref_temp_count++);
  return finish_mangling_get_identifier ();
}

/* Return an identifier for the mangled name of a C++20 template parameter
   object for template argument EXPR.  */

tree
mangle_template_parm_object (tree expr)
{
  start_mangling (expr);
  write_string ("_ZTAX");
  write_expression (expr);
  write_char ('E');
  return finish_mangling_get_identifier ();
}

/* Given a CLASS_TYPE, such as a record for std::bad_exception this
   function generates a mangled name for the vtable map variable of
   the class type.  For example, if the class type is
   "std::bad_exception", the mangled name for the class is
   "St13bad_exception".  This function would generate the name
   "_ZN4_VTVISt13bad_exceptionE12__vtable_mapE", which unmangles as:
   "_VTV<std::bad_exception>::__vtable_map".  */


char *
get_mangled_vtable_map_var_name (tree class_type)
{
  char *var_name = NULL;
  const char *prefix = "_ZN4_VTVI";
  const char *postfix = "E12__vtable_mapE";

  gcc_assert (TREE_CODE (class_type) == RECORD_TYPE);

  tree class_id = DECL_ASSEMBLER_NAME (TYPE_NAME (class_type));

  if (strstr (IDENTIFIER_POINTER (class_id), "<anon>") != NULL)
    {
      class_id = get_mangled_id (TYPE_NAME (class_type));
      vtbl_register_mangled_name (TYPE_NAME (class_type), class_id);
    }

  unsigned int len = strlen (IDENTIFIER_POINTER (class_id)) +
                     strlen (prefix) +
                     strlen (postfix) + 1;

  var_name = (char *) xmalloc (len);

  sprintf (var_name, "%s%s%s", prefix, IDENTIFIER_POINTER (class_id), postfix);

  return var_name;
}

#include "gt-cp-mangle.h"
