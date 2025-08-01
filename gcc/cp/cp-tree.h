/* Definitions for -*- C++ -*- parsing and type checking.
   Copyright (C) 1987-2025 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_CP_TREE_H
#define GCC_CP_TREE_H

#include "tm.h"
#include "hard-reg-set.h"
#include "function.h"
#include "tristate.h"
#include "contracts.h"

/* In order for the format checking to accept the C++ front end
   diagnostic framework extensions, you must include this file before
   diagnostic-core.h, not after.  We override the definition of GCC_DIAG_STYLE
   in c-common.h.  */
#undef GCC_DIAG_STYLE
#define GCC_DIAG_STYLE __gcc_cxxdiag__
#if defined(GCC_DIAGNOSTIC_CORE_H) || defined (GCC_C_COMMON_H)
#error \
In order for the format checking to accept the C++ front end diagnostic \
framework extensions, you must include this file before diagnostic-core.h and \
c-common.h, not after.
#endif
#include "c-family/c-common.h"
#include "diagnostic.h"

/* A tree node, together with a location, so that we can track locations
   (and ranges) during parsing.

   The location is redundant for node kinds that have locations,
   but not all node kinds do (e.g. constants, and references to
   params, locals, etc), so we stash a copy here.  */

inline location_t cp_expr_location		(const_tree);

class cp_expr
{
public:
  cp_expr () :
    m_value (NULL), m_loc (UNKNOWN_LOCATION),
    m_decimal (false)
  {}

  cp_expr (tree value) :
    m_value (value), m_loc (cp_expr_location (m_value)),
    m_decimal (false)
  {}

  cp_expr (tree value, location_t loc):
    m_value (value), m_loc (loc), m_decimal (false)
  {
    protected_set_expr_location (value, loc);
  }

  cp_expr (tree value, location_t loc, bool decimal):
    m_value (value), m_loc (loc), m_decimal (decimal)
  {
    protected_set_expr_location (value, loc);
  }

  /* Implicit conversions to tree.  */
  operator tree () const { return m_value; }
  tree & operator* () { return m_value; }
  tree operator* () const { return m_value; }
  tree & operator-> () { return m_value; }
  tree operator-> () const { return m_value; }

  tree get_value () const { return m_value; }
  location_t get_location () const { return m_loc; }
  location_t get_start () const
  {
    source_range src_range = get_range_from_loc (line_table, m_loc);
    return src_range.m_start;
  }
  location_t get_finish () const
  {
    source_range src_range = get_range_from_loc (line_table, m_loc);
    return src_range.m_finish;
  }

  void set_location (location_t loc)
  {
    protected_set_expr_location (m_value, loc);
    m_loc = loc;
  }

  void set_range (location_t start, location_t finish)
  {
    set_location (make_location (m_loc, start, finish));
  }

  cp_expr& maybe_add_location_wrapper ()
  {
    m_value = maybe_wrap_with_location (m_value, m_loc);
    return *this;
  }

  bool decimal_p () const { return m_decimal; }

 private:
  tree m_value;
  location_t m_loc;
  bool m_decimal : 1;
};

inline bool
operator == (const cp_expr &lhs, tree rhs)
{
  return lhs.get_value () == rhs;
}


enum cp_tree_index
{
    CPTI_WCHAR_DECL,
    CPTI_VTABLE_ENTRY_TYPE,
    CPTI_DELTA_TYPE,
    CPTI_VTABLE_INDEX_TYPE,
    CPTI_CLEANUP_TYPE,
    CPTI_VTT_PARM_TYPE,

    CPTI_CLASS_TYPE,
    CPTI_UNKNOWN_TYPE,
    CPTI_INIT_LIST_TYPE,
    CPTI_EXPLICIT_VOID_LIST,
    CPTI_VTBL_TYPE,
    CPTI_VTBL_PTR_TYPE,
    CPTI_GLOBAL,
    CPTI_ABORT_FNDECL,
    CPTI_AGGR_TAG,
    CPTI_CONV_OP_MARKER,

    CPTI_CTOR_IDENTIFIER,
    CPTI_COMPLETE_CTOR_IDENTIFIER,
    CPTI_BASE_CTOR_IDENTIFIER,
    CPTI_DTOR_IDENTIFIER,
    CPTI_COMPLETE_DTOR_IDENTIFIER,
    CPTI_BASE_DTOR_IDENTIFIER,
    CPTI_DELETING_DTOR_IDENTIFIER,
    CPTI_CONV_OP_IDENTIFIER,
    CPTI_DELTA_IDENTIFIER,
    CPTI_IN_CHARGE_IDENTIFIER,
    CPTI_VTT_PARM_IDENTIFIER,
    CPTI_AS_BASE_IDENTIFIER,
    CPTI_THIS_IDENTIFIER,
    CPTI_PFN_IDENTIFIER,
    CPTI_VPTR_IDENTIFIER,
    CPTI_GLOBAL_IDENTIFIER,
    CPTI_ANON_IDENTIFIER,
    CPTI_AUTO_IDENTIFIER,
    CPTI_DECLTYPE_AUTO_IDENTIFIER,
    CPTI_INIT_LIST_IDENTIFIER,
    CPTI_FOR_RANGE__IDENTIFIER,
    CPTI_FOR_BEGIN__IDENTIFIER,
    CPTI_FOR_END__IDENTIFIER,
    CPTI_FOR_RANGE_IDENTIFIER,
    CPTI_FOR_BEGIN_IDENTIFIER,
    CPTI_FOR_END_IDENTIFIER,
    CPTI_ABI_TAG_IDENTIFIER,
    CPTI_ALIGNED_IDENTIFIER,
    CPTI_BEGIN_IDENTIFIER,
    CPTI_END_IDENTIFIER,
    CPTI_GET_IDENTIFIER,
    CPTI_GNU_IDENTIFIER,
    CPTI_TUPLE_ELEMENT_IDENTIFIER,
    CPTI_TUPLE_SIZE_IDENTIFIER,
    CPTI_TYPE_IDENTIFIER,
    CPTI_VALUE_IDENTIFIER,
    CPTI_FUN_IDENTIFIER,
    CPTI_CLOSURE_IDENTIFIER,
    CPTI_HEAP_UNINIT_IDENTIFIER,
    CPTI_HEAP_IDENTIFIER,
    CPTI_HEAP_DELETED_IDENTIFIER,
    CPTI_HEAP_VEC_UNINIT_IDENTIFIER,
    CPTI_HEAP_VEC_IDENTIFIER,
    CPTI_OMP_IDENTIFIER,
    CPTI_INTERNAL_IDENTIFIER,

    CPTI_LANG_NAME_C,
    CPTI_LANG_NAME_CPLUSPLUS,

    CPTI_EMPTY_EXCEPT_SPEC,
    CPTI_NOEXCEPT_TRUE_SPEC,
    CPTI_NOEXCEPT_FALSE_SPEC,
    CPTI_NOEXCEPT_DEFERRED_SPEC,

    CPTI_ANY_TARG,

    CPTI_MODULE_HWM,
    /* Nodes after here change during compilation, or should not be in
       the module's global tree table.  Such nodes must be locatable
       via name lookup or type-construction, as those are the only
       cross-TU matching capabilities remaining.  */

    /* We must find these via the global namespace.  */
    CPTI_STD,
    CPTI_ABI,

    /* These are created at init time, but the library/headers provide
       definitions.  */
    CPTI_ALIGN_TYPE,
    CPTI_TERMINATE_FN,
    CPTI_CALL_TERMINATE_FN,
    CPTI_CALL_UNEXPECTED_FN,

    /* These are lazily inited.  */
    CPTI_CONST_TYPE_INFO_TYPE,
    CPTI_GET_EXCEPTION_PTR_FN,
    CPTI_BEGIN_CATCH_FN,
    CPTI_END_CATCH_FN,
    CPTI_ALLOCATE_EXCEPTION_FN,
    CPTI_FREE_EXCEPTION_FN,
    CPTI_THROW_FN,
    CPTI_RETHROW_FN,
    CPTI_ATEXIT_FN_PTR_TYPE,
    CPTI_ATEXIT,
    CPTI_THREAD_ATEXIT,
    CPTI_DSO_HANDLE,
    CPTI_DCAST,

    CPTI_PSEUDO_CONTRACT_VIOLATION,

    CPTI_MAX
};

extern GTY(()) tree cp_global_trees[CPTI_MAX];

#define wchar_decl_node			cp_global_trees[CPTI_WCHAR_DECL]
#define vtable_entry_type		cp_global_trees[CPTI_VTABLE_ENTRY_TYPE]
/* The type used to represent an offset by which to adjust the `this'
   pointer in pointer-to-member types.  */
#define delta_type_node			cp_global_trees[CPTI_DELTA_TYPE]
/* The type used to represent an index into the vtable.  */
#define vtable_index_type		cp_global_trees[CPTI_VTABLE_INDEX_TYPE]

#define class_type_node			cp_global_trees[CPTI_CLASS_TYPE]
#define unknown_type_node		cp_global_trees[CPTI_UNKNOWN_TYPE]
#define init_list_type_node		cp_global_trees[CPTI_INIT_LIST_TYPE]
#define explicit_void_list_node		cp_global_trees[CPTI_EXPLICIT_VOID_LIST]
#define vtbl_type_node			cp_global_trees[CPTI_VTBL_TYPE]
#define vtbl_ptr_type_node		cp_global_trees[CPTI_VTBL_PTR_TYPE]
#define std_node			cp_global_trees[CPTI_STD]
#define abi_node			cp_global_trees[CPTI_ABI]
#define global_namespace		cp_global_trees[CPTI_GLOBAL]
#define const_type_info_type_node	cp_global_trees[CPTI_CONST_TYPE_INFO_TYPE]
#define conv_op_marker			cp_global_trees[CPTI_CONV_OP_MARKER]
#define abort_fndecl			cp_global_trees[CPTI_ABORT_FNDECL]
#define current_aggr			cp_global_trees[CPTI_AGGR_TAG]
/* std::align_val_t */
#define align_type_node			cp_global_trees[CPTI_ALIGN_TYPE]
#define pseudo_contract_violation_type	cp_global_trees[CPTI_PSEUDO_CONTRACT_VIOLATION]

/* We cache these tree nodes so as to call get_identifier less frequently.
   For identifiers for functions, including special member functions such
   as ctors and assignment operators, the nodes can be used (among other
   things) to iterate over their overloads defined by/for a type.  For
   example:

     tree ovlid = assign_op_identifier;
     tree overloads = get_class_binding (type, ovlid);
     for (ovl_iterator it (overloads); it; ++it) { ... }

   iterates over the set of implicitly and explicitly defined overloads
   of the assignment operator for type (including the copy and move
   assignment operators, whether deleted or not).  */

/* The name of a constructor that takes an in-charge parameter to
   decide whether or not to construct virtual base classes.  */
#define ctor_identifier			cp_global_trees[CPTI_CTOR_IDENTIFIER]
/* The name of a constructor that constructs virtual base classes.  */
#define complete_ctor_identifier	cp_global_trees[CPTI_COMPLETE_CTOR_IDENTIFIER]
/* The name of a constructor that does not construct virtual base classes.  */
#define base_ctor_identifier		cp_global_trees[CPTI_BASE_CTOR_IDENTIFIER]
/* The name of a destructor that takes an in-charge parameter to
   decide whether or not to destroy virtual base classes.  */
#define dtor_identifier			cp_global_trees[CPTI_DTOR_IDENTIFIER]
/* The name of a destructor that destroys virtual base classes.  */
#define complete_dtor_identifier	cp_global_trees[CPTI_COMPLETE_DTOR_IDENTIFIER]
/* The name of a destructor that does not destroy virtual base
   classes.  */
#define base_dtor_identifier		cp_global_trees[CPTI_BASE_DTOR_IDENTIFIER]
/* The name of a destructor that destroys virtual base classes, and
   then deletes the entire object.  */
#define deleting_dtor_identifier	cp_global_trees[CPTI_DELETING_DTOR_IDENTIFIER]

/* The name used for conversion operators -- but note that actual
   conversion functions use special identifiers outside the identifier
   table.  */
#define conv_op_identifier		cp_global_trees[CPTI_CONV_OP_IDENTIFIER]

#define delta_identifier		cp_global_trees[CPTI_DELTA_IDENTIFIER]
#define in_charge_identifier		cp_global_trees[CPTI_IN_CHARGE_IDENTIFIER]
/* The name of the parameter that contains a pointer to the VTT to use
   for this subobject constructor or destructor.  */
#define vtt_parm_identifier		cp_global_trees[CPTI_VTT_PARM_IDENTIFIER]
#define as_base_identifier		cp_global_trees[CPTI_AS_BASE_IDENTIFIER]
#define this_identifier			cp_global_trees[CPTI_THIS_IDENTIFIER]
#define pfn_identifier			cp_global_trees[CPTI_PFN_IDENTIFIER]
#define vptr_identifier			cp_global_trees[CPTI_VPTR_IDENTIFIER]
/* The name of the ::, std & anon namespaces.  */
#define global_identifier		cp_global_trees[CPTI_GLOBAL_IDENTIFIER]
#define anon_identifier			cp_global_trees[CPTI_ANON_IDENTIFIER]
/* auto and declspec(auto) identifiers.  */
#define auto_identifier			cp_global_trees[CPTI_AUTO_IDENTIFIER]
#define decltype_auto_identifier	cp_global_trees[CPTI_DECLTYPE_AUTO_IDENTIFIER]
#define init_list_identifier		cp_global_trees[CPTI_INIT_LIST_IDENTIFIER]
#define for_range__identifier		cp_global_trees[CPTI_FOR_RANGE__IDENTIFIER]
#define for_begin__identifier		cp_global_trees[CPTI_FOR_BEGIN__IDENTIFIER]
#define for_end__identifier		cp_global_trees[CPTI_FOR_END__IDENTIFIER]
#define for_range_identifier		cp_global_trees[CPTI_FOR_RANGE_IDENTIFIER]
#define for_begin_identifier		cp_global_trees[CPTI_FOR_BEGIN_IDENTIFIER]
#define for_end_identifier		cp_global_trees[CPTI_FOR_END_IDENTIFIER]
#define abi_tag_identifier		cp_global_trees[CPTI_ABI_TAG_IDENTIFIER]
#define aligned_identifier		cp_global_trees[CPTI_ALIGNED_IDENTIFIER]
#define begin_identifier		cp_global_trees[CPTI_BEGIN_IDENTIFIER]
#define end_identifier			cp_global_trees[CPTI_END_IDENTIFIER]
#define get__identifier			cp_global_trees[CPTI_GET_IDENTIFIER]
#define gnu_identifier			cp_global_trees[CPTI_GNU_IDENTIFIER]
#define tuple_element_identifier	cp_global_trees[CPTI_TUPLE_ELEMENT_IDENTIFIER]
#define tuple_size_identifier		cp_global_trees[CPTI_TUPLE_SIZE_IDENTIFIER]
#define type_identifier			cp_global_trees[CPTI_TYPE_IDENTIFIER]
#define value_identifier		cp_global_trees[CPTI_VALUE_IDENTIFIER]
#define fun_identifier			cp_global_trees[CPTI_FUN_IDENTIFIER]
#define closure_identifier		cp_global_trees[CPTI_CLOSURE_IDENTIFIER]
#define heap_uninit_identifier		cp_global_trees[CPTI_HEAP_UNINIT_IDENTIFIER]
#define heap_identifier			cp_global_trees[CPTI_HEAP_IDENTIFIER]
#define heap_deleted_identifier		cp_global_trees[CPTI_HEAP_DELETED_IDENTIFIER]
#define heap_vec_uninit_identifier	cp_global_trees[CPTI_HEAP_VEC_UNINIT_IDENTIFIER]
#define heap_vec_identifier		cp_global_trees[CPTI_HEAP_VEC_IDENTIFIER]
#define omp_identifier			cp_global_trees[CPTI_OMP_IDENTIFIER]
#define internal_identifier		cp_global_trees[CPTI_INTERNAL_IDENTIFIER]
#define lang_name_c			cp_global_trees[CPTI_LANG_NAME_C]
#define lang_name_cplusplus		cp_global_trees[CPTI_LANG_NAME_CPLUSPLUS]

/* Exception specifiers used for throw(), noexcept(true),
   noexcept(false) and deferred noexcept.  We rely on these being
   uncloned.  */
#define empty_except_spec		cp_global_trees[CPTI_EMPTY_EXCEPT_SPEC]
#define noexcept_true_spec		cp_global_trees[CPTI_NOEXCEPT_TRUE_SPEC]
#define noexcept_false_spec		cp_global_trees[CPTI_NOEXCEPT_FALSE_SPEC]
#define noexcept_deferred_spec		cp_global_trees[CPTI_NOEXCEPT_DEFERRED_SPEC]

/* Exception handling function declarations.  */
#define terminate_fn			cp_global_trees[CPTI_TERMINATE_FN]
#define call_unexpected_fn		cp_global_trees[CPTI_CALL_UNEXPECTED_FN]
#define call_terminate_fn		cp_global_trees[CPTI_CALL_TERMINATE_FN]
#define get_exception_ptr_fn		cp_global_trees[CPTI_GET_EXCEPTION_PTR_FN]
#define begin_catch_fn			cp_global_trees[CPTI_BEGIN_CATCH_FN]
#define end_catch_fn			cp_global_trees[CPTI_END_CATCH_FN]
#define allocate_exception_fn		cp_global_trees[CPTI_ALLOCATE_EXCEPTION_FN]
#define free_exception_fn		cp_global_trees[CPTI_FREE_EXCEPTION_FN]
#define throw_fn			cp_global_trees[CPTI_THROW_FN]
#define rethrow_fn			cp_global_trees[CPTI_RETHROW_FN]

/* The type of the function-pointer argument to "std::atexit".  */
#define atexit_fn_ptr_type_node         cp_global_trees[CPTI_ATEXIT_FN_PTR_TYPE]

/* A pointer to `std::atexit'.  */
#define atexit_node			cp_global_trees[CPTI_ATEXIT]

/* A pointer to `__cxa_thread_atexit'.  */
#define thread_atexit_node		cp_global_trees[CPTI_THREAD_ATEXIT]

/* A pointer to `__dso_handle'.  */
#define dso_handle_node			cp_global_trees[CPTI_DSO_HANDLE]

/* The declaration of the dynamic_cast runtime.  */
#define dynamic_cast_node		cp_global_trees[CPTI_DCAST]

/* The type of a destructor, passed to __cxa_atexit, __cxa_thread_atexit
   or __cxa_throw.  */
#define cleanup_type			cp_global_trees[CPTI_CLEANUP_TYPE]

/* The type of the vtt parameter passed to subobject constructors and
   destructors.  */
#define vtt_parm_type			cp_global_trees[CPTI_VTT_PARM_TYPE]

/* A node which matches any template argument.  */
#define any_targ_node			cp_global_trees[CPTI_ANY_TARG]

/* Node to indicate default access. This must be distinct from the
   access nodes in tree.h.  */

#define access_default_node		null_node

#include "name-lookup.h"

/* Usage of TREE_LANG_FLAG_?:
   0: IDENTIFIER_KIND_BIT_0 (in IDENTIFIER_NODE)
      NEW_EXPR_USE_GLOBAL (in NEW_EXPR).
      COND_EXPR_IS_VEC_DELETE (in COND_EXPR).
      DELETE_EXPR_USE_GLOBAL (in DELETE_EXPR).
      CLEANUP_P (in TRY_BLOCK)
      AGGR_INIT_VIA_CTOR_P (in AGGR_INIT_EXPR)
      PTRMEM_OK_P (in ADDR_EXPR, OFFSET_REF, SCOPE_REF)
      PAREN_STRING_LITERAL_P (in STRING_CST)
      CP_DECL_THREAD_LOCAL_P (in VAR_DECL)
      KOENIG_LOOKUP_P (in CALL_EXPR)
      STATEMENT_LIST_NO_SCOPE (in STATEMENT_LIST).
      EXPR_STMT_STMT_EXPR_RESULT (in EXPR_STMT)
      STMT_EXPR_NO_SCOPE (in STMT_EXPR)
      BIND_EXPR_TRY_BLOCK (in BIND_EXPR)
      TYPENAME_IS_ENUM_P (in TYPENAME_TYPE)
      OMP_FOR_GIMPLIFYING_P (in OMP_FOR, OMP_SIMD, OMP_DISTRIBUTE,
			     and OMP_TASKLOOP)
      BASELINK_QUALIFIED_P (in BASELINK)
      TARGET_EXPR_IMPLICIT_P (in TARGET_EXPR)
      TEMPLATE_PARM_PARAMETER_PACK (in TEMPLATE_PARM_INDEX)
      ATTR_IS_DEPENDENT (in the TREE_LIST for an attribute)
      ABI_TAG_IMPLICIT (in the TREE_LIST for the argument of abi_tag)
      LAMBDA_CAPTURE_EXPLICIT_P (in a TREE_LIST in LAMBDA_EXPR_CAPTURE_LIST)
      PARENTHESIZED_LIST_P (in the TREE_LIST for a parameter-declaration-list)
      CONSTRUCTOR_IS_DIRECT_INIT (in CONSTRUCTOR)
      DECLTYPE_FOR_LAMBDA_CAPTURE (in DECLTYPE_TYPE)
      VEC_INIT_EXPR_IS_CONSTEXPR (in VEC_INIT_EXPR)
      DECL_OVERRIDE_P (in FUNCTION_DECL)
      IMPLICIT_CONV_EXPR_DIRECT_INIT (in IMPLICIT_CONV_EXPR)
      TRANSACTION_EXPR_IS_STMT (in TRANSACTION_EXPR)
      CONVERT_EXPR_VBASE_PATH (in CONVERT_EXPR)
      PACK_EXPANSION_LOCAL_P (in *_PACK_EXPANSION)
      TINFO_HAS_ACCESS_ERRORS (in TEMPLATE_INFO)
      SIZEOF_EXPR_TYPE_P (in SIZEOF_EXPR)
      COMPOUND_REQ_NOEXCEPT_P (in COMPOUND_REQ)
      BLOCK_OUTER_CURLY_BRACE_P (in BLOCK)
      FOLD_EXPR_MODIFY_P (*_FOLD_EXPR)
      IF_STMT_CONSTEXPR_P (IF_STMT)
      DECL_NAMESPACE_INLINE_P (in NAMESPACE_DECL)
      SWITCH_STMT_ALL_CASES_P (in SWITCH_STMT)
      REINTERPRET_CAST_P (in NOP_EXPR)
      ALIGNOF_EXPR_STD_P (in ALIGNOF_EXPR)
      OVL_DEDUP_P (in OVERLOAD)
      INIT_EXPR_NRV_P (in INIT_EXPR)
      ATOMIC_CONSTR_MAP_INSTANTIATED_P (in ATOMIC_CONSTR)
      contract_semantic (in ASSERTION_, PRECONDITION_, POSTCONDITION_STMT)
      RETURN_EXPR_LOCAL_ADDR_P (in RETURN_EXPR)
      PACK_INDEX_PARENTHESIZED_P (in PACK_INDEX_*)
      MUST_NOT_THROW_NOEXCEPT_P (in MUST_NOT_THROW_EXPR)
      CONSTEVAL_BLOCK_P (in STATIC_ASSERT)
      LAMBDA_EXPR_CONSTEVAL_BLOCK_P (in LAMBDA_EXPR)
   1: IDENTIFIER_KIND_BIT_1 (in IDENTIFIER_NODE)
      TI_PENDING_TEMPLATE_FLAG.
      TEMPLATE_PARMS_FOR_INLINE.
      DELETE_EXPR_USE_VEC (in DELETE_EXPR).
      ICS_ELLIPSIS_FLAG (in _CONV)
      DECL_INITIALIZED_P (in VAR_DECL)
      TYPENAME_IS_CLASS_P (in TYPENAME_TYPE)
      STMT_IS_FULL_EXPR_P (in _STMT)
      TARGET_EXPR_LIST_INIT_P (in TARGET_EXPR)
      DECL_FINAL_P (in FUNCTION_DECL)
      QUALIFIED_NAME_IS_TEMPLATE (in SCOPE_REF)
      CONSTRUCTOR_IS_DEPENDENT (in CONSTRUCTOR)
      TINFO_USED_TEMPLATE_ID (in TEMPLATE_INFO)
      PACK_EXPANSION_SIZEOF_P (in *_PACK_EXPANSION)
      OVL_USING_P (in OVERLOAD)
      IMPLICIT_CONV_EXPR_NONTYPE_ARG (in IMPLICIT_CONV_EXPR)
      BASELINK_FUNCTIONS_MAYBE_INCOMPLETE_P (in BASELINK)
      BIND_EXPR_VEC_DTOR (in BIND_EXPR)
      ATOMIC_CONSTR_EXPR_FROM_CONCEPT_P (in ATOMIC_CONSTR)
      STATIC_INIT_DECOMP_BASE_P (in the TREE_LIST for {static,tls}_aggregates)
      MUST_NOT_THROW_THROW_P (in MUST_NOT_THROW_EXPR)
   2: IDENTIFIER_KIND_BIT_2 (in IDENTIFIER_NODE)
      ICS_THIS_FLAG (in _CONV)
      DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (in VAR_DECL)
      STATEMENT_LIST_TRY_BLOCK (in STATEMENT_LIST)
      TYPENAME_IS_RESOLVING_P (in TYPENAME_TYPE)
      TARGET_EXPR_DIRECT_INIT_P (in TARGET_EXPR)
      FNDECL_USED_AUTO (in FUNCTION_DECL)
      DECLTYPE_FOR_LAMBDA_PROXY (in DECLTYPE_TYPE)
      REF_PARENTHESIZED_P (in COMPONENT_REF, INDIRECT_REF, SCOPE_REF,
			   VIEW_CONVERT_EXPR, PAREN_EXPR)
      AGGR_INIT_ZERO_FIRST (in AGGR_INIT_EXPR)
      CONSTRUCTOR_MUTABLE_POISON (in CONSTRUCTOR)
      OVL_HIDDEN_P (in OVERLOAD)
      IF_STMT_CONSTEVAL_P (in IF_STMT)
      SWITCH_STMT_NO_BREAK_P (in SWITCH_STMT)
      LAMBDA_EXPR_CAPTURE_OPTIMIZED (in LAMBDA_EXPR)
      IMPLICIT_CONV_EXPR_BRACED_INIT (in IMPLICIT_CONV_EXPR)
      PACK_EXPANSION_AUTO_P (in *_PACK_EXPANSION)
      contract_semantic (in ASSERTION_, PRECONDITION_, POSTCONDITION_STMT)
      STATIC_INIT_DECOMP_NONBASE_P (in the TREE_LIST
				    for {static,tls}_aggregates)
      MUST_NOT_THROW_CATCH_P (in MUST_NOT_THROW_EXPR)
   3: IMPLICIT_RVALUE_P (in NON_LVALUE_EXPR or STATIC_CAST_EXPR)
      ICS_BAD_FLAG (in _CONV)
      FN_TRY_BLOCK_P (in TRY_BLOCK)
      BIND_EXPR_BODY_BLOCK (in BIND_EXPR)
      CALL_EXPR_ORDERED_ARGS (in CALL_EXPR, AGGR_INIT_EXPR)
      DECLTYPE_FOR_REF_CAPTURE (in DECLTYPE_TYPE)
      CONSTRUCTOR_C99_COMPOUND_LITERAL (in CONSTRUCTOR)
      OVL_NESTED_P (in OVERLOAD)
      DECL_MODULE_EXPORT_P (in _DECL)
      PACK_EXPANSION_FORCE_EXTRA_ARGS_P (in *_PACK_EXPANSION)
      LAMBDA_EXPR_STATIC_P (in LAMBDA_EXPR)
      TARGET_EXPR_ELIDING_P (in TARGET_EXPR)
      contract_semantic (in ASSERTION_, PRECONDITION_, POSTCONDITION_STMT)
      TYPENAME_IS_UNION_P (in TYPENAME_TYPE)
   4: IDENTIFIER_MARKED (IDENTIFIER_NODEs)
      TREE_HAS_CONSTRUCTOR (in INDIRECT_REF, SAVE_EXPR, CONSTRUCTOR,
	  CALL_EXPR, or FIELD_DECL).
      DECL_TINFO_P (in VAR_DECL, TYPE_DECL)
      FUNCTION_REF_QUALIFIED (in FUNCTION_TYPE, METHOD_TYPE)
      OVL_LOOKUP_P (in OVERLOAD)
      LOOKUP_FOUND_P (in RECORD_TYPE, UNION_TYPE, ENUMERAL_TYPE, NAMESPACE_DECL)
      FNDECL_MANIFESTLY_CONST_EVALUATED (in FUNCTION_DECL)
      TARGET_EXPR_INTERNAL_P (in TARGET_EXPR)
   5: IDENTIFIER_VIRTUAL_P (in IDENTIFIER_NODE)
      FUNCTION_RVALUE_QUALIFIED (in FUNCTION_TYPE, METHOD_TYPE)
      CALL_EXPR_REVERSE_ARGS (in CALL_EXPR, AGGR_INIT_EXPR)
      CONSTRUCTOR_PLACEHOLDER_BOUNDARY (in CONSTRUCTOR)
      OVL_EXPORT_P (in OVERLOAD)
      DECL_NTTP_OBJECT_P (in VAR_DECL)
   6: TYPE_MARKED_P (in _TYPE)
      DECL_NONTRIVIALLY_INITIALIZED_P (in VAR_DECL)
      RANGE_FOR_IVDEP (in RANGE_FOR_STMT)
      CALL_EXPR_OPERATOR_SYNTAX (in CALL_EXPR, AGGR_INIT_EXPR)
      CONSTRUCTOR_IS_DESIGNATED_INIT (in CONSTRUCTOR)
      OVL_NAME_INDEPENDENT_DECL_P (in OVERLOAD)

   Usage of TYPE_LANG_FLAG_?:
   0: TYPE_DEPENDENT_P
   1: TYPE_HAS_USER_CONSTRUCTOR.
   2: TYPE_HAS_LATE_RETURN_TYPE (in FUNCTION_TYPE, METHOD_TYPE)
      TYPE_PTRMEMFUNC_FLAG (in RECORD_TYPE)
   4: TYPE_HAS_NONTRIVIAL_DESTRUCTOR
   5: CLASS_TYPE_P (in RECORD_TYPE and UNION_TYPE)
      ENUM_FIXED_UNDERLYING_TYPE_P (in ENUMERAL_TYPE)
      AUTO_IS_DECLTYPE (in TEMPLATE_TYPE_PARM)
      TEMPLATE_TEMPLATE_PARM_SIMPLE_P (in TEMPLATE_TEMPLATE_PARM)
   6: TYPE_DEPENDENT_P_VALID

   Usage of DECL_LANG_FLAG_?:
   0: DECL_TEMPLATE_PARM_P (in PARM_DECL, CONST_DECL, TYPE_DECL, or TEMPLATE_DECL)
      DECL_LOCAL_DECL_P (in FUNCTION_DECL, VAR_DECL)
      DECL_MUTABLE_P (in FIELD_DECL)
      DECL_DEPENDENT_P (in USING_DECL)
      LABEL_DECL_BREAK (in LABEL_DECL)
   1: C_TYPEDEF_EXPLICITLY_SIGNED (in TYPE_DECL).
      DECL_TEMPLATE_INSTANTIATED (in a VAR_DECL or a FUNCTION_DECL)
      DECL_MEMBER_TEMPLATE_P (in TEMPLATE_DECL)
      USING_DECL_TYPENAME_P (in USING_DECL)
      DECL_VLA_CAPTURE_P (in FIELD_DECL)
      DECL_ARRAY_PARAMETER_P (in PARM_DECL)
      LABEL_DECL_CONTINUE (in LABEL_DECL)
   2: DECL_THIS_EXTERN (in VAR_DECL, FUNCTION_DECL or PARM_DECL)
      DECL_IMPLICIT_TYPEDEF_P (in a TYPE_DECL)
      DECL_CONSTRAINT_VAR_P (in a PARM_DECL)
      DECL_INSTANTIATING_NSDMI_P (in a FIELD_DECL)
      USING_DECL_UNRELATED_P (in USING_DECL)
   3: DECL_IN_AGGR_P.
   4: DECL_C_BIT_FIELD (in a FIELD_DECL)
      DECL_ANON_UNION_VAR_P (in a VAR_DECL)
      DECL_SELF_REFERENCE_P (in a TYPE_DECL)
      DECL_INVALID_OVERRIDER_P (in a FUNCTION_DECL)
      DECL_UNINSTANIATED_TEMPLATE_FRIEND_P (in TEMPLATE_DECL)
   5: DECL_INTERFACE_KNOWN.
   6: DECL_THIS_STATIC (in VAR_DECL, FUNCTION_DECL or PARM_DECL)
      DECL_FIELD_IS_BASE (in FIELD_DECL)
      TYPE_DECL_ALIAS_P (in TYPE_DECL)
   7: DECL_THUNK_P (in a member FUNCTION_DECL)
      DECL_NORMAL_CAPTURE_P (in FIELD_DECL)
      DECL_DECLARED_CONSTINIT_P (in VAR_DECL)
   8: DECL_DECLARED_CONSTEXPR_P (in VAR_DECL, FUNCTION_DECL)

   Usage of language-independent fields in a language-dependent manner:

   TYPE_ALIAS_SET
     This field is used by TYPENAME_TYPEs, TEMPLATE_TYPE_PARMs, and so
     forth as a substitute for the mark bits provided in `lang_type'.
     At present, only the six low-order bits are used.

   TYPE_LANG_SLOT_1
     For a FUNCTION_TYPE or METHOD_TYPE, this is TYPE_RAISES_EXCEPTIONS.
     For a POINTER_TYPE (to a METHOD_TYPE), this is TYPE_PTRMEMFUNC_TYPE.
     For an ENUMERAL_TYPE, BOUND_TEMPLATE_TEMPLATE_PARM_TYPE,
     RECORD_TYPE or UNION_TYPE this is TYPE_TEMPLATE_INFO,

  BINFO_VIRTUALS
     For a binfo, this is a TREE_LIST.  There is an entry for each
     virtual function declared either in BINFO or its direct and
     indirect primary bases.

     The BV_DELTA of each node gives the amount by which to adjust the
     `this' pointer when calling the function.  If the method is an
     overridden version of a base class method, then it is assumed
     that, prior to adjustment, the this pointer points to an object
     of the base class.

     The BV_VCALL_INDEX of each node, if non-NULL, gives the vtable
     index of the vcall offset for this entry.

     The BV_FN is the declaration for the virtual function itself.

     If BV_LOST_PRIMARY is set, it means that this entry is for a lost
     primary virtual base and can be left null in the vtable.

   BINFO_VTABLE
     This is an expression with POINTER_TYPE that gives the value
     to which the vptr should be initialized.  Use get_vtbl_decl_for_binfo
     to extract the VAR_DECL for the complete vtable.

   DECL_VINDEX
     This field is NULL for a non-virtual function.  For a virtual
     function, it is eventually set to an INTEGER_CST indicating the
     index in the vtable at which this function can be found.  When
     a virtual function is declared, but before it is known what
     function is overridden, this field is the error_mark_node.

     Temporarily, it may be set to a TREE_LIST whose TREE_VALUE is
     the virtual function this one overrides, and whose TREE_CHAIN is
     the old DECL_VINDEX.  */

/* Language-specific tree checkers.  */

#define VAR_OR_FUNCTION_DECL_CHECK(NODE) \
  TREE_CHECK2(NODE,VAR_DECL,FUNCTION_DECL)

#define TYPE_FUNCTION_OR_TEMPLATE_DECL_CHECK(NODE) \
  TREE_CHECK3(NODE,TYPE_DECL,TEMPLATE_DECL,FUNCTION_DECL)

#define TYPE_FUNCTION_OR_TEMPLATE_DECL_P(NODE) \
  (TREE_CODE (NODE) == TYPE_DECL || TREE_CODE (NODE) == TEMPLATE_DECL \
   || TREE_CODE (NODE) == FUNCTION_DECL)

#define VAR_FUNCTION_OR_PARM_DECL_CHECK(NODE) \
  TREE_CHECK3(NODE,VAR_DECL,FUNCTION_DECL,PARM_DECL)

#define VAR_TEMPL_TYPE_OR_FUNCTION_DECL_CHECK(NODE) \
  TREE_CHECK4(NODE,VAR_DECL,FUNCTION_DECL,TYPE_DECL,TEMPLATE_DECL)

#define VAR_TEMPL_TYPE_FIELD_OR_FUNCTION_DECL_CHECK(NODE) \
  TREE_CHECK5(NODE,VAR_DECL,FIELD_DECL,FUNCTION_DECL,TYPE_DECL,TEMPLATE_DECL)

#define BOUND_TEMPLATE_TEMPLATE_PARM_TYPE_CHECK(NODE) \
  TREE_CHECK(NODE,BOUND_TEMPLATE_TEMPLATE_PARM)

#if defined ENABLE_TREE_CHECKING && (GCC_VERSION >= 2007)

/* Returns t iff the node can have a TEMPLATE_INFO field.  */

inline tree
template_info_decl_check (const_tree t, const char* f, int l, const char* fn)
{
  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case FUNCTION_DECL:
    case FIELD_DECL:
    case TYPE_DECL:
    case CONCEPT_DECL:
    case TEMPLATE_DECL:
      return const_cast<tree>(t);
    default:
      break;
    }
  tree_check_failed (t, f, l, fn,
		     VAR_DECL, FUNCTION_DECL, FIELD_DECL, TYPE_DECL,
		     CONCEPT_DECL, TEMPLATE_DECL, 0);
  gcc_unreachable ();
}

#define TEMPLATE_INFO_DECL_CHECK(NODE) \
  template_info_decl_check ((NODE), __FILE__, __LINE__, __FUNCTION__)

#define THUNK_FUNCTION_CHECK(NODE) __extension__			\
({  __typeof (NODE) const __t = (NODE);					\
    if (TREE_CODE (__t) != FUNCTION_DECL || !__t->decl_common.lang_specific \
	|| !__t->decl_common.lang_specific->u.fn.thunk_p)		\
      tree_check_failed (__t, __FILE__, __LINE__, __FUNCTION__, 0);	\
     __t; })

#define DECL_TEMPLATE_PARM_CHECK(NODE) \
  decl_template_parm_check ((NODE), __FILE__, __LINE__, __FUNCTION__)

#else /* ENABLE_TREE_CHECKING */

#define TEMPLATE_INFO_DECL_CHECK(NODE) (NODE)
#define THUNK_FUNCTION_CHECK(NODE) (NODE)
#define DECL_TEMPLATE_PARM_CHECK(NODE) (NODE)

#endif /* ENABLE_TREE_CHECKING */

/* Language-dependent contents of an identifier.  */

struct GTY(()) lang_identifier {
  struct c_common_identifier c_common;
  cxx_binding *bindings;
};

/* Return a typed pointer version of T if it designates a
   C++ front-end identifier.  */
inline lang_identifier*
identifier_p (tree t)
{
  if (TREE_CODE (t) == IDENTIFIER_NODE)
    return (lang_identifier*) t;
  return NULL;
}

#define LANG_IDENTIFIER_CAST(NODE) \
	((struct lang_identifier*)IDENTIFIER_NODE_CHECK (NODE))

struct GTY(()) template_parm_index {
  struct tree_common common;
  int index;
  int level;
  int orig_level;
  tree decl;
};

struct GTY(()) ptrmem_cst {
  struct tree_typed typed;
  tree member;
  location_t locus;
};
typedef struct ptrmem_cst * ptrmem_cst_t;

#define CLEANUP_P(NODE)		TREE_LANG_FLAG_0 (TRY_BLOCK_CHECK (NODE))

#define BIND_EXPR_TRY_BLOCK(NODE) \
  TREE_LANG_FLAG_0 (BIND_EXPR_CHECK (NODE))

/* This BIND_EXPR is from build_vec_delete_1.  */
#define BIND_EXPR_VEC_DTOR(NODE) \
  TREE_LANG_FLAG_1 (BIND_EXPR_CHECK (NODE))

/* Used to mark the block around the member initializers and cleanups.  */
#define BIND_EXPR_BODY_BLOCK(NODE) \
  TREE_LANG_FLAG_3 (BIND_EXPR_CHECK (NODE))
#define FUNCTION_NEEDS_BODY_BLOCK(NODE) \
  (DECL_CONSTRUCTOR_P (NODE) || DECL_DESTRUCTOR_P (NODE) \
   || LAMBDA_FUNCTION_P (NODE))

#define STATEMENT_LIST_NO_SCOPE(NODE) \
  TREE_LANG_FLAG_0 (STATEMENT_LIST_CHECK (NODE))
#define STATEMENT_LIST_TRY_BLOCK(NODE) \
  TREE_LANG_FLAG_2 (STATEMENT_LIST_CHECK (NODE))

/* Mark the outer curly brace BLOCK.  */
#define BLOCK_OUTER_CURLY_BRACE_P(NODE)	TREE_LANG_FLAG_0 (BLOCK_CHECK (NODE))

/* Nonzero if this statement should be considered a full-expression,
   i.e., if temporaries created during this statement should have
   their destructors run at the end of this statement.  */
#define STMT_IS_FULL_EXPR_P(NODE) TREE_LANG_FLAG_1 ((NODE))

/* Marks the result of a statement expression.  */
#define EXPR_STMT_STMT_EXPR_RESULT(NODE) \
  TREE_LANG_FLAG_0 (EXPR_STMT_CHECK (NODE))

/* Nonzero if this statement-expression does not have an associated scope.  */
#define STMT_EXPR_NO_SCOPE(NODE) \
   TREE_LANG_FLAG_0 (STMT_EXPR_CHECK (NODE))

#define COND_EXPR_IS_VEC_DELETE(NODE) \
  TREE_LANG_FLAG_0 (COND_EXPR_CHECK (NODE))

/* Nonzero if this NOP_EXPR is a reinterpret_cast.  Such conversions
   are not constexprs.  Other NOP_EXPRs are.  */
#define REINTERPRET_CAST_P(NODE)		\
  TREE_LANG_FLAG_0 (NOP_EXPR_CHECK (NODE))

/* Returns nonzero iff TYPE1 and TYPE2 are the same type, in the usual
   sense of `same'.  */
#define same_type_p(TYPE1, TYPE2) \
  comptypes ((TYPE1), (TYPE2), COMPARE_STRICT)

/* Returns nonzero iff NODE is a declaration for the global function
   `main'.  */
#define DECL_MAIN_ANY_P(NODE)				\
   (DECL_EXTERN_C_FUNCTION_P (NODE)			\
    && DECL_NAME (NODE) != NULL_TREE			\
    && MAIN_NAME_P (DECL_NAME (NODE)))

/* Nonzero iff NODE is a declaration for `int main', or we are hosted.  */
#define DECL_MAIN_FREESTANDING_P(NODE)				\
  (DECL_MAIN_ANY_P(NODE)					\
   && (flag_hosted						\
       || TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (NODE)))	\
	  == integer_type_node))

/* Nonzero iff NODE is a declaration for `main', and we are hosted.  */
#define DECL_MAIN_P(NODE) (DECL_MAIN_ANY_P(NODE) && flag_hosted)

/* Lookup walker marking.  */
#define LOOKUP_SEEN_P(NODE) TREE_VISITED (NODE)
#define LOOKUP_FOUND_P(NODE) \
  TREE_LANG_FLAG_4 (TREE_CHECK4 (NODE,RECORD_TYPE,UNION_TYPE,ENUMERAL_TYPE,\
				 NAMESPACE_DECL))

/* These two accessors should only be used by OVL manipulators.
   Other users should use iterators and convenience functions.  */
#define OVL_FUNCTION(NODE) \
  (((struct tree_overload*)OVERLOAD_CHECK (NODE))->function)
#define OVL_CHAIN(NODE) \
  (((struct tree_overload*)OVERLOAD_CHECK (NODE))->common.chain)

/* If set, this or a subsequent overload contains decls that need deduping.  */
#define OVL_DEDUP_P(NODE)	TREE_LANG_FLAG_0 (OVERLOAD_CHECK (NODE))
/* If set, this was imported in a using declaration.   */
#define OVL_USING_P(NODE)	TREE_LANG_FLAG_1 (OVERLOAD_CHECK (NODE))
/* If set, this overload is a hidden decl.  */
#define OVL_HIDDEN_P(NODE)	TREE_LANG_FLAG_2 (OVERLOAD_CHECK (NODE))
/* If set, this overload contains a nested overload.  */
#define OVL_NESTED_P(NODE)	TREE_LANG_FLAG_3 (OVERLOAD_CHECK (NODE))
/* If set, this overload was constructed during lookup.  */
#define OVL_LOOKUP_P(NODE)	TREE_LANG_FLAG_4 (OVERLOAD_CHECK (NODE))
/* If set, this OVL_USING_P overload is exported.  */
#define OVL_EXPORT_P(NODE)	TREE_LANG_FLAG_5 (OVERLOAD_CHECK (NODE))
/* If set, this OVL_USING_P overload is in the module purview.  */
#define OVL_PURVIEW_P(NODE)	(OVERLOAD_CHECK (NODE)->base.public_flag)
/* If set, this overload includes name-independent declarations.  */
#define OVL_NAME_INDEPENDENT_DECL_P(NODE) \
  TREE_LANG_FLAG_6 (OVERLOAD_CHECK (NODE))

/* The first decl of an overload.  */
#define OVL_FIRST(NODE)	ovl_first (NODE)
/* The name of the overload set.  */
#define OVL_NAME(NODE) DECL_NAME (OVL_FIRST (NODE))

/* Whether this is a set of overloaded functions.  TEMPLATE_DECLS are
   always wrapped in an OVERLOAD, so we don't need to check them
   here.  */
#define OVL_P(NODE) \
  (TREE_CODE (NODE) == FUNCTION_DECL || TREE_CODE (NODE) == OVERLOAD)
/* Whether this is a single member overload.  */
#define OVL_SINGLE_P(NODE) \
  (TREE_CODE (NODE) != OVERLOAD || !OVL_CHAIN (NODE))

/* OVL_HIDDEN_P nodes come before other nodes.  */

struct GTY(()) tree_overload {
  struct tree_common common;
  tree function;
};

/* Iterator for a 1 dimensional overload.  Permits iterating over the
   outer level of a 2-d overload when explicitly enabled.  */

class ovl_iterator {
  tree ovl;
  const bool allow_inner; /* Only used when checking.  */

 public:
  explicit ovl_iterator (tree o, bool allow = false)
    : ovl (o), allow_inner (allow)
  {
  }

 public:
  operator bool () const
  {
    return ovl;
  }
  ovl_iterator &operator++ ()
  {
    ovl = TREE_CODE (ovl) != OVERLOAD ? NULL_TREE : OVL_CHAIN (ovl);
    return *this;
  }
  tree operator* () const
  {
    tree fn = TREE_CODE (ovl) != OVERLOAD ? ovl : OVL_FUNCTION (ovl);

    /* Check this is not an unexpected 2-dimensional overload.  */
    gcc_checking_assert (allow_inner || TREE_CODE (fn) != OVERLOAD);

    return fn;
  }
  bool operator== (const ovl_iterator &o) const
  {
    return ovl == o.ovl;
  }
  tree get_using () const
  {
    gcc_checking_assert (using_p ());
    return ovl;
  }

 public:
  /* Whether this overload was introduced by a using decl.  */
  bool using_p () const
  {
    return (TREE_CODE (ovl) == USING_DECL
	    || (TREE_CODE (ovl) == OVERLOAD && OVL_USING_P (ovl)));
  }
  bool hidden_p () const
  {
    return TREE_CODE (ovl) == OVERLOAD && OVL_HIDDEN_P (ovl);
  }
  bool purview_p () const;
  bool exporting_p () const;

 public:
  tree remove_node (tree head)
  {
    return remove_node (head, ovl);
  }
  tree reveal_node (tree head)
  {
    return reveal_node (head, ovl);
  }

 protected:
  /* If we have a nested overload, point at the inner overload and
     return the next link on the outer one.  */
  tree maybe_push ()
  {
    tree r = NULL_TREE;

    if (ovl && TREE_CODE (ovl) == OVERLOAD && OVL_NESTED_P (ovl))
      {
	r = OVL_CHAIN (ovl);
	ovl = OVL_FUNCTION (ovl);
      }
    return r;
  }
  /* Restore an outer nested overload.  */
  void pop (tree outer)
  {
    gcc_checking_assert (!ovl);
    ovl = outer;
  }

 private:
  /* We make these static functions to avoid the address of the
     iterator escaping the local context.  */
  static tree remove_node (tree head, tree node);
  static tree reveal_node (tree ovl, tree node);
};

/* Treat a tree as a range of ovl_iterator, e.g.
   for (tree f : ovl_range (fns)) { ... }  */

class ovl_range
{
  tree t;
  bool allow;
public:
  explicit ovl_range (tree t, bool allow = false): t(t), allow(allow) { }
  ovl_iterator begin() { return ovl_iterator (t, allow); }
  ovl_iterator end() { return ovl_iterator (NULL_TREE, allow); }
};

/* Iterator over a (potentially) 2 dimensional overload, which is
   produced by name lookup.  */

class lkp_iterator : public ovl_iterator {
  typedef ovl_iterator parent;

  tree outer;

 public:
  explicit lkp_iterator (tree o)
    : parent (o, true), outer (maybe_push ())
  {
  }

 public:
  lkp_iterator &operator++ ()
  {
    bool repush = !outer;

    if (!parent::operator++ () && !repush)
      {
	pop (outer);
	repush = true;
      }

    if (repush)
      outer = maybe_push ();

    return *this;
  }
};

/* Treat a tree as a range of lkp_iterator, e.g.
   for (tree f : lkp_range (fns)) { ... }  */

class lkp_range
{
  tree t;
public:
  lkp_range (tree t): t(t) { }
  lkp_iterator begin() { return lkp_iterator (t); }
  lkp_iterator end() { return lkp_iterator (NULL_TREE); }
};

/* Iterator for a RAW_DATA_CST.  */

class raw_data_iterator {
  tree t;
  unsigned int n;

 public:
  explicit raw_data_iterator (tree t, unsigned int n)
    : t (t), n (n)
  {
  }

  operator bool () const
  {
    return n < (unsigned) RAW_DATA_LENGTH (t);
  }

  raw_data_iterator &operator++ ()
  {
    ++n;
    return *this;
  }

  tree operator* () const
  {
    return build_int_cst (TREE_TYPE (t), RAW_DATA_UCHAR_ELT (t, n));
  }

  bool operator== (const raw_data_iterator &o) const
  {
    return t == o.t && n == o.n;
  }
};

/* Treat a tree as a range of raw_data_iterator, e.g.
   for (tree f : raw_data_range (d)) { ... }  */

class raw_data_range
{
  tree t;
public:
  raw_data_range (tree t) : t (t) { }
  raw_data_iterator begin () { return raw_data_iterator (t, 0); }
  raw_data_iterator end ()
  { return raw_data_iterator (t, RAW_DATA_LENGTH (t)); }
};

/* hash traits for declarations.  Hashes potential overload sets via
   DECL_NAME.  */

struct named_decl_hash : ggc_remove <tree> {
  typedef tree value_type; /* A DECL or OVERLOAD  */
  typedef tree compare_type; /* An identifier.  */

  inline static hashval_t hash (const value_type decl);
  inline static bool equal (const value_type existing, compare_type candidate);

  static const bool empty_zero_p = true;
  static inline void mark_empty (value_type &p) {p = NULL_TREE;}
  static inline bool is_empty (value_type p) {return !p;}

  /* Nothing is deletable.  Everything is insertable.  */
  static bool is_deleted (value_type) { return false; }
  static void mark_deleted (value_type) { gcc_unreachable (); }
};

/* Simplified unique_ptr clone to release a tree vec on exit.  */

class releasing_vec
{
public:
  typedef vec<tree, va_gc> vec_t;

  releasing_vec (vec_t *v): v(v) { }
  releasing_vec (): v(make_tree_vector ()) { }

  /* Copy ops are deliberately declared but not defined,
     copies must always be elided.  */
  releasing_vec (const releasing_vec &);
  releasing_vec &operator= (const releasing_vec &);

  vec_t &operator* () const { return *v; }
  vec_t *operator-> () const { return v; }
  vec_t *get() const { return v; }
  operator vec_t *() const { return v; }
  vec_t ** operator& () { return &v; }

  /* Breaks pointer/value consistency for convenience.  This takes ptrdiff_t
     rather than unsigned to avoid ambiguity with the built-in operator[]
     (bootstrap/91828).  */
  tree& operator[] (ptrdiff_t i) const { return (*v)[i]; }

  tree *begin() { return ::begin (v); }
  tree *end() { return ::end (v); }

  void release () { release_tree_vector (v); v = NULL; }

  ~releasing_vec () { release_tree_vector (v); }
private:
  vec_t *v;
};
/* Forwarding functions for vec_safe_* that might reallocate.  */
inline tree* vec_safe_push (releasing_vec& r, const tree &t CXX_MEM_STAT_INFO)
{ return vec_safe_push (*&r, t PASS_MEM_STAT); }
inline bool vec_safe_reserve (releasing_vec& r, unsigned n, bool e = false CXX_MEM_STAT_INFO)
{ return vec_safe_reserve (*&r, n, e PASS_MEM_STAT); }
inline unsigned vec_safe_length (releasing_vec &r)
{ return r->length(); }
inline void vec_safe_splice (releasing_vec &r, vec<tree, va_gc> *p CXX_MEM_STAT_INFO)
{ vec_safe_splice (*&r, p PASS_MEM_STAT); }
void release_tree_vector (releasing_vec &); // cause link error

struct GTY(()) tree_template_decl {
  struct tree_decl_common common;
  tree arguments;
  tree result;
};

/* Returns true iff NODE is a BASELINK.  */
#define BASELINK_P(NODE) \
  (TREE_CODE (NODE) == BASELINK)
/* The BINFO indicating the base in which lookup found the
   BASELINK_FUNCTIONS.  */
#define BASELINK_BINFO(NODE) \
  (((struct tree_baselink*) BASELINK_CHECK (NODE))->binfo)
/* The functions referred to by the BASELINK; either a FUNCTION_DECL,
   a TEMPLATE_DECL, an OVERLOAD, or a TEMPLATE_ID_EXPR.  */
#define BASELINK_FUNCTIONS(NODE) \
  (((struct tree_baselink*) BASELINK_CHECK (NODE))->functions)
/* If T is a BASELINK, grab the functions, otherwise just T, which is
   expected to already be a (list of) functions.  */
#define MAYBE_BASELINK_FUNCTIONS(T) \
  (BASELINK_P (T) ? BASELINK_FUNCTIONS (T) : T)
/* The BINFO in which the search for the functions indicated by this baselink
   began.  This base is used to determine the accessibility of functions
   selected by overload resolution.  */
#define BASELINK_ACCESS_BINFO(NODE) \
  (((struct tree_baselink*) BASELINK_CHECK (NODE))->access_binfo)
/* For a type-conversion operator, the BASELINK_OPTYPE indicates the type
   to which the conversion should occur.  This value is important if
   the BASELINK_FUNCTIONS include a template conversion operator --
   the BASELINK_OPTYPE can be used to determine what type the user
   requested.  */
#define BASELINK_OPTYPE(NODE) \
  (TREE_CHAIN (BASELINK_CHECK (NODE)))
/* Nonzero if this baselink was from a qualified lookup.  */
#define BASELINK_QUALIFIED_P(NODE) \
  TREE_LANG_FLAG_0 (BASELINK_CHECK (NODE))
/* Nonzero if the overload set for this baselink might be incomplete due
   to the lookup being performed from an incomplete-class context.  */
#define BASELINK_FUNCTIONS_MAYBE_INCOMPLETE_P(NODE) \
  TREE_LANG_FLAG_1 (BASELINK_CHECK (NODE))

struct GTY(()) tree_baselink {
  struct tree_common common;
  tree binfo;
  tree functions;
  tree access_binfo;
};

/* The different kinds of ids that we encounter.  */

enum cp_id_kind
{
  /* Not an id at all.  */
  CP_ID_KIND_NONE,
  /* An unqualified-id that is not a template-id.  */
  CP_ID_KIND_UNQUALIFIED,
  /* An unqualified-id that is a dependent name.  */
  CP_ID_KIND_UNQUALIFIED_DEPENDENT,
  /* An unqualified template-id.  */
  CP_ID_KIND_TEMPLATE_ID,
  /* A qualified-id.  */
  CP_ID_KIND_QUALIFIED
};


/* The various kinds of C++0x warnings we encounter.  */

enum cpp0x_warn_str
{
  /* extended initializer lists */
  CPP0X_INITIALIZER_LISTS,
  /* explicit conversion operators */
  CPP0X_EXPLICIT_CONVERSION,
  /* variadic templates */
  CPP0X_VARIADIC_TEMPLATES,
  /* lambda expressions */
  CPP0X_LAMBDA_EXPR,
  /* C++0x auto */
  CPP0X_AUTO,
  /* scoped enums */
  CPP0X_SCOPED_ENUMS,
  /* defaulted and deleted functions */
  CPP0X_DEFAULTED_DELETED,
  /* inline namespaces */
  CPP0X_INLINE_NAMESPACES,
  /* override controls, override/final */
  CPP0X_OVERRIDE_CONTROLS,
  /* non-static data member initializers */
  CPP0X_NSDMI,
  /* user defined literals */
  CPP0X_USER_DEFINED_LITERALS,
  /* delegating constructors */
  CPP0X_DELEGATING_CTORS,
  /* inheriting constructors */
  CPP0X_INHERITING_CTORS,
  /* C++11 attributes */
  CPP0X_ATTRIBUTES,
  /* ref-qualified member functions */
  CPP0X_REF_QUALIFIER
};

/* The various kinds of operation used by composite_pointer_type.  */

enum composite_pointer_operation
{
  /* comparison */
  CPO_COMPARISON,
  /* conversion */
  CPO_CONVERSION,
  /* conditional expression */
  CPO_CONDITIONAL_EXPR
};

/* Possible cases of expression list used by
   build_x_compound_expr_from_list.  */
enum expr_list_kind {
  ELK_INIT,		/* initializer */
  ELK_MEM_INIT,		/* member initializer */
  ELK_FUNC_CAST		/* functional cast */
};

/* Possible cases of implicit bad rhs conversions.  */
enum impl_conv_rhs {
  ICR_DEFAULT_ARGUMENT, /* default argument */
  ICR_CONVERTING,       /* converting */
  ICR_INIT,             /* initialization */
  ICR_ARGPASS,          /* argument passing */
  ICR_RETURN,           /* return */
  ICR_ASSIGN            /* assignment */
};

/* Possible cases of implicit or explicit bad conversions to void.  */
enum impl_conv_void {
  ICV_CAST,            /* (explicit) conversion to void */
  ICV_SECOND_OF_COND,  /* second operand of conditional expression */
  ICV_THIRD_OF_COND,   /* third operand of conditional expression */
  ICV_RIGHT_OF_COMMA,  /* right operand of comma operator */
  ICV_LEFT_OF_COMMA,   /* left operand of comma operator */
  ICV_STATEMENT,       /* statement */
  ICV_THIRD_IN_FOR     /* for increment expression */
};

/* Possible invalid uses of an abstract class that might not have a
   specific associated declaration.  */
enum GTY(()) abstract_class_use {
  ACU_UNKNOWN,			/* unknown or decl provided */
  ACU_CAST,			/* cast to abstract class */
  ACU_NEW,			/* new-expression of abstract class */
  ACU_THROW,			/* throw-expression of abstract class */
  ACU_CATCH,			/* catch-parameter of abstract class */
  ACU_ARRAY,			/* array of abstract class */
  ACU_RETURN,			/* return type of abstract class */
  ACU_PARM			/* parameter type of abstract class */
};

/* Macros for access to language-specific slots in an identifier.  */

/* Identifiers map directly to block or class-scope bindings.
   Namespace-scope bindings are held in hash tables on the respective
   namespaces.  The identifier bindings are the innermost active
   binding, from whence you can get the decl and/or implicit-typedef
   of an elaborated type.   When not bound to a local entity the
   values are NULL.  */
#define IDENTIFIER_BINDING(NODE) \
  (LANG_IDENTIFIER_CAST (NODE)->bindings)
#define REAL_IDENTIFIER_TYPE_VALUE(NODE) TREE_TYPE (NODE)
#define SET_IDENTIFIER_TYPE_VALUE(NODE,TYPE) (TREE_TYPE (NODE) = (TYPE))

/* Kinds of identifiers.  Values are carefully chosen.  */
enum cp_identifier_kind {
  cik_normal = 0,	/* Not a special identifier.  */
  cik_keyword = 1,	/* A keyword.  */
  cik_ctor = 2,		/* Constructor (in-chg, complete or base).  */
  cik_dtor = 3,		/* Destructor (in-chg, deleting, complete or
			   base).  */
  cik_simple_op = 4,	/* Non-assignment operator name.  */
  cik_assign_op = 5,	/* An assignment operator name.  */
  cik_conv_op = 6,	/* Conversion operator name.  */
  cik_trait = 7,	/* Built-in trait name.  */
  cik_max
};

/* Kind bits.  */
#define IDENTIFIER_KIND_BIT_0(NODE) \
  TREE_LANG_FLAG_0 (IDENTIFIER_NODE_CHECK (NODE))
#define IDENTIFIER_KIND_BIT_1(NODE) \
  TREE_LANG_FLAG_1 (IDENTIFIER_NODE_CHECK (NODE))
#define IDENTIFIER_KIND_BIT_2(NODE) \
  TREE_LANG_FLAG_2 (IDENTIFIER_NODE_CHECK (NODE))

/* Used by various search routines.  */
#define IDENTIFIER_MARKED(NODE) \
  TREE_LANG_FLAG_4 (IDENTIFIER_NODE_CHECK (NODE))

/* Nonzero if this identifier is used as a virtual function name somewhere
   (optimizes searches).  */
#define IDENTIFIER_VIRTUAL_P(NODE) \
  TREE_LANG_FLAG_5 (IDENTIFIER_NODE_CHECK (NODE))

/* Return the cp_identifier_kind of the given IDENTIFIER node ID.  */

ATTRIBUTE_PURE inline cp_identifier_kind
get_identifier_kind (tree id)
{
  unsigned bit0 = IDENTIFIER_KIND_BIT_0 (id);
  unsigned bit1 = IDENTIFIER_KIND_BIT_1 (id);
  unsigned bit2 = IDENTIFIER_KIND_BIT_2 (id);
  return cp_identifier_kind ((bit2 << 2) | (bit1 << 1) | bit0);
}

/* True if this identifier is a reserved word.  C_RID_CODE (node) is
   then the RID_* value of the keyword.  Value 1.  */
#define IDENTIFIER_KEYWORD_P(NODE)		\
  (get_identifier_kind (NODE) == cik_keyword)

/* True if this identifier is the name of a constructor or
   destructor.  Value 2 or 3.  */
#define IDENTIFIER_CDTOR_P(NODE)		\
  (IDENTIFIER_CTOR_P (NODE) || IDENTIFIER_DTOR_P (NODE))

/* True if this identifier is the name of a constructor.  Value 2.  */
#define IDENTIFIER_CTOR_P(NODE)			\
  (get_identifier_kind (NODE) == cik_ctor)

/* True if this identifier is the name of a destructor.  Value 3.  */
#define IDENTIFIER_DTOR_P(NODE)			\
  (get_identifier_kind (NODE) == cik_dtor)

/* True if this identifier is for any operator name (including
   conversions).  Value 4, 5, or 6.  */
#define IDENTIFIER_ANY_OP_P(NODE)		\
  (IDENTIFIER_OVL_OP_P (NODE) || IDENTIFIER_CONV_OP_P (NODE))

/* True if this identifier is for an overloaded operator. Values 4, 5.  */
#define IDENTIFIER_OVL_OP_P(NODE)		\
  (get_identifier_kind (NODE) == cik_simple_op  \
   || get_identifier_kind (NODE) == cik_assign_op)

/* True if this identifier is for any assignment. Values 5.  */
#define IDENTIFIER_ASSIGN_OP_P(NODE)		\
  (get_identifier_kind (NODE) == cik_assign_op)

/* True if this identifier is the name of a type-conversion
   operator.  Value 6.  */
#define IDENTIFIER_CONV_OP_P(NODE)		\
  (get_identifier_kind (NODE) == cik_conv_op)

/* True if this identifier is the name of a built-in trait.  */
#define IDENTIFIER_TRAIT_P(NODE)		\
  (get_identifier_kind (NODE) == cik_trait)

/* True if this identifier is a new or delete operator.  */
#define IDENTIFIER_NEWDEL_OP_P(NODE)		\
  (IDENTIFIER_OVL_OP_P (NODE)			\
   && IDENTIFIER_OVL_OP_FLAGS (NODE) & OVL_OP_FLAG_ALLOC)

/* True if this identifier is a new operator.  */
#define IDENTIFIER_NEW_OP_P(NODE)					\
  (IDENTIFIER_OVL_OP_P (NODE)						\
   && (IDENTIFIER_OVL_OP_FLAGS (NODE)					\
       & (OVL_OP_FLAG_ALLOC | OVL_OP_FLAG_DELETE)) == OVL_OP_FLAG_ALLOC)

/* Access a C++-specific index for identifier NODE.
   Used to optimize operator mappings etc.  */
#define IDENTIFIER_CP_INDEX(NODE)		\
  (IDENTIFIER_NODE_CHECK(NODE)->base.u.bits.address_space)

/* In a RECORD_TYPE or UNION_TYPE, nonzero if any component is read-only.  */
#define C_TYPE_FIELDS_READONLY(TYPE) \
  (LANG_TYPE_CLASS_CHECK (TYPE)->fields_readonly)

/* The tokens stored in the unparsed operand.  */

#define DEFPARSE_TOKENS(NODE) \
  (((struct tree_deferred_parse *)DEFERRED_PARSE_CHECK (NODE))->tokens)
#define DEFPARSE_INSTANTIATIONS(NODE) \
  (((struct tree_deferred_parse *)DEFERRED_PARSE_CHECK (NODE))->instantiations)

struct GTY (()) tree_deferred_parse {
  struct tree_base base;
  struct cp_token_cache *tokens;
  vec<tree, va_gc> *instantiations;
};


#define DEFERRED_NOEXCEPT_PATTERN(NODE) \
  (((struct tree_deferred_noexcept *)DEFERRED_NOEXCEPT_CHECK (NODE))->pattern)
#define DEFERRED_NOEXCEPT_ARGS(NODE) \
  (((struct tree_deferred_noexcept *)DEFERRED_NOEXCEPT_CHECK (NODE))->args)
#define DEFERRED_NOEXCEPT_SPEC_P(NODE)				\
  ((NODE) && (TREE_PURPOSE (NODE))				\
   && (TREE_CODE (TREE_PURPOSE (NODE)) == DEFERRED_NOEXCEPT))
#define UNEVALUATED_NOEXCEPT_SPEC_P(NODE)				\
  (DEFERRED_NOEXCEPT_SPEC_P (NODE)					\
   && DEFERRED_NOEXCEPT_PATTERN (TREE_PURPOSE (NODE)) == NULL_TREE)
#define UNPARSED_NOEXCEPT_SPEC_P(NODE) \
  ((NODE) && (TREE_PURPOSE (NODE)) \
   && (TREE_CODE (TREE_PURPOSE (NODE)) == DEFERRED_PARSE))

struct GTY (()) tree_deferred_noexcept {
  struct tree_base base;
  tree pattern;
  tree args;
};


/* The condition associated with the static assertion.  This must be
   an integral constant expression.  */
#define STATIC_ASSERT_CONDITION(NODE) \
  (((struct tree_static_assert *)STATIC_ASSERT_CHECK (NODE))->condition)

/* The message associated with the static assertion.  This must be a
   string constant, which will be emitted as an error message when the
   static assert condition is false.  */
#define STATIC_ASSERT_MESSAGE(NODE) \
  (((struct tree_static_assert *)STATIC_ASSERT_CHECK (NODE))->message)

/* Source location information for a static assertion.  */
#define STATIC_ASSERT_SOURCE_LOCATION(NODE) \
  (((struct tree_static_assert *)STATIC_ASSERT_CHECK (NODE))->location)

/* True if this static assert represents a C++26 consteval block.  */
#define CONSTEVAL_BLOCK_P(NODE) \
  TREE_LANG_FLAG_0 (STATIC_ASSERT_CHECK (NODE))

struct GTY (()) tree_static_assert {
  struct tree_base base;
  tree condition;
  tree message;
  location_t location;
};

struct GTY (()) tree_argument_pack_select {
  struct tree_base base;
  tree argument_pack;
  int index;
};

/* The different kinds of traits that we encounter.  The size is limited to
   addr_space_t since a trait is looked up by IDENTIFIER_CP_INDEX.  */
enum cp_trait_kind : addr_space_t {
#define DEFTRAIT(TCC, CODE, NAME, ARITY) \
  CPTK_##CODE,
#include "cp-trait.def"
#undef DEFTRAIT
};

/* The trait type.  */
struct cp_trait {
  const char *name;
  cp_trait_kind kind;
  short arity;
  bool type;
};

/* The trait table indexed by cp_trait_kind.  */
extern const struct cp_trait cp_traits[];

/* The types that we are processing.  */
#define TRAIT_EXPR_TYPE1(NODE) \
  (((struct tree_trait_expr *)TRAIT_EXPR_CHECK (NODE))->type1)

#define TRAIT_EXPR_TYPE2(NODE) \
  (((struct tree_trait_expr *)TRAIT_EXPR_CHECK (NODE))->type2)

/* The specific trait that we are processing.  */
#define TRAIT_EXPR_KIND(NODE) \
  (((struct tree_trait_expr *)TRAIT_EXPR_CHECK (NODE))->kind)

#define TRAIT_EXPR_LOCATION(NODE) \
  (((struct tree_trait_expr *)TRAIT_EXPR_CHECK (NODE))->locus)

struct GTY (()) tree_trait_expr {
  struct tree_typed typed;
  tree type1;
  tree type2;
  location_t locus;
  enum cp_trait_kind kind;
};

/* An INTEGER_CST containing the kind of the trait type NODE.  */
#define TRAIT_TYPE_KIND_RAW(NODE) \
  TYPE_VALUES_RAW (TRAIT_TYPE_CHECK (NODE))

/* The kind of the trait type NODE.  */
#define TRAIT_TYPE_KIND(NODE) \
  ((enum cp_trait_kind) TREE_INT_CST_LOW (TRAIT_TYPE_KIND_RAW (NODE)))

/* The first argument of the trait type NODE.  */
#define TRAIT_TYPE_TYPE1(NODE) \
  TYPE_MIN_VALUE_RAW (TRAIT_TYPE_CHECK (NODE))

/* The rest of the arguments of the trait type NODE.  */
#define TRAIT_TYPE_TYPE2(NODE) \
  TYPE_MAX_VALUE_RAW (TRAIT_TYPE_CHECK (NODE))

/* Identifiers used for lambda types are almost anonymous.  Use this
   spare flag to distinguish them (they also have the anonymous flag).  */
#define IDENTIFIER_LAMBDA_P(NODE) \
  (IDENTIFIER_NODE_CHECK(NODE)->base.protected_flag)

/* Based off of TYPE_UNNAMED_P.  */
#define LAMBDA_TYPE_P(NODE)					\
  (TREE_CODE (NODE) == RECORD_TYPE				\
   && TYPE_LINKAGE_IDENTIFIER (NODE)				\
   && IDENTIFIER_LAMBDA_P (TYPE_LINKAGE_IDENTIFIER (NODE)))

/* Test if FUNCTION_DECL is a lambda function.  */
#define LAMBDA_FUNCTION_P(FNDECL)				\
  (DECL_DECLARES_FUNCTION_P (FNDECL)				\
   && DECL_OVERLOADED_OPERATOR_P (FNDECL)			\
   && DECL_OVERLOADED_OPERATOR_IS (FNDECL, CALL_EXPR)		\
   && LAMBDA_TYPE_P (CP_DECL_CONTEXT (FNDECL)))

enum cp_lambda_default_capture_mode_type {
  CPLD_NONE,
  CPLD_COPY,
  CPLD_REFERENCE
};

/* The method of default capture, if any.  */
#define LAMBDA_EXPR_DEFAULT_CAPTURE_MODE(NODE) \
  (((struct tree_lambda_expr *)LAMBDA_EXPR_CHECK (NODE))->default_capture_mode)

/* The capture-list, including `this'.  Each capture is stored as a FIELD_DECL
 * so that the name, type, and field are all together, whether or not it has
 * been added to the lambda's class type.
   TREE_LIST:
     TREE_PURPOSE: The FIELD_DECL for this capture.
     TREE_VALUE: The initializer. This is part of a GNU extension.  */
#define LAMBDA_EXPR_CAPTURE_LIST(NODE) \
  (((struct tree_lambda_expr *)LAMBDA_EXPR_CHECK (NODE))->capture_list)

/* During parsing of the lambda-introducer, the node in the capture-list
   that holds the 'this' capture.  During parsing of the body, the
   capture proxy for that node.  */
#define LAMBDA_EXPR_THIS_CAPTURE(NODE) \
  (((struct tree_lambda_expr *)LAMBDA_EXPR_CHECK (NODE))->this_capture)

/* True iff this lambda was created for a consteval block.  */
#define LAMBDA_EXPR_CONSTEVAL_BLOCK_P(NODE) \
  TREE_LANG_FLAG_0 (LAMBDA_EXPR_CHECK (NODE))

/* True iff uses of a const variable capture were optimized away.  */
#define LAMBDA_EXPR_CAPTURE_OPTIMIZED(NODE) \
  TREE_LANG_FLAG_2 (LAMBDA_EXPR_CHECK (NODE))

/* Predicate tracking whether the lambda was declared 'static'.  */
#define LAMBDA_EXPR_STATIC_P(NODE) \
  TREE_LANG_FLAG_3 (LAMBDA_EXPR_CHECK (NODE))

/* True if this TREE_LIST in LAMBDA_EXPR_CAPTURE_LIST is for an explicit
   capture.  */
#define LAMBDA_CAPTURE_EXPLICIT_P(NODE) \
  TREE_LANG_FLAG_0 (TREE_LIST_CHECK (NODE))

/* The source location of the lambda.  */
#define LAMBDA_EXPR_LOCATION(NODE) \
  (((struct tree_lambda_expr *)LAMBDA_EXPR_CHECK (NODE))->locus)

/* The mangling scope for the lambda: FUNCTION_DECL, PARM_DECL, VAR_DECL,
   FIELD_DECL, TYPE_DECL, or NULL_TREE.  If this is NULL_TREE, we have no
   linkage.  */
#define LAMBDA_EXPR_EXTRA_SCOPE(NODE) \
  (((struct tree_lambda_expr *)LAMBDA_EXPR_CHECK (NODE))->extra_scope)

/* Lambdas in the same extra scope might need a discriminating count.
   For ABI 17, we have single per-scope count, for ABI 18, we have
   per-scope, per-signature numbering.  */
#define LAMBDA_EXPR_SCOPE_ONLY_DISCRIMINATOR(NODE) \
  (((struct tree_lambda_expr *)LAMBDA_EXPR_CHECK (NODE))->discriminator_scope)
#define LAMBDA_EXPR_SCOPE_SIG_DISCRIMINATOR(NODE) \
  (((struct tree_lambda_expr *)LAMBDA_EXPR_CHECK (NODE))->discriminator_sig)

/* During parsing of the lambda, a vector of capture proxies which need
   to be pushed once we're done processing a nested lambda.  */
#define LAMBDA_EXPR_PENDING_PROXIES(NODE) \
  (((struct tree_lambda_expr *)LAMBDA_EXPR_CHECK (NODE))->pending_proxies)

/* If NODE was regenerated via tsubst_lambda_expr, this is a TEMPLATE_INFO
   whose TI_TEMPLATE is the immediate LAMBDA_EXPR from which NODE was
   regenerated, and TI_ARGS is the full set of template arguments used
   to regenerate NODE from the most general lambda.  */
#define LAMBDA_EXPR_REGEN_INFO(NODE) \
  (((struct tree_lambda_expr *)LAMBDA_EXPR_CHECK (NODE))->regen_info)

/* Like PACK_EXPANSION_EXTRA_ARGS, for lambda-expressions.  */
#define LAMBDA_EXPR_EXTRA_ARGS(NODE) \
  (((struct tree_lambda_expr *)LAMBDA_EXPR_CHECK (NODE))->extra_args)

/* The closure type of the lambda, which is also the type of the
   LAMBDA_EXPR.  */
#define LAMBDA_EXPR_CLOSURE(NODE) \
  (TREE_TYPE (LAMBDA_EXPR_CHECK (NODE)))

struct GTY (()) tree_lambda_expr
{
  struct tree_typed typed;
  tree capture_list;
  tree this_capture;
  tree extra_scope;
  tree regen_info;
  tree extra_args;
  vec<tree, va_gc> *pending_proxies;
  location_t locus;
  enum cp_lambda_default_capture_mode_type default_capture_mode : 2;
  unsigned discriminator_scope : 15; // Per-scope discriminator
  unsigned discriminator_sig : 15; // Per-scope, per-signature discriminator
};

/* Non-zero if this template specialization has access violations that
   should be rechecked when the function is instantiated outside argument
   deduction.  */
#define TINFO_HAS_ACCESS_ERRORS(NODE) \
  (TREE_LANG_FLAG_0 (TEMPLATE_INFO_CHECK (NODE)))
#define FNDECL_HAS_ACCESS_ERRORS(NODE) \
  (TINFO_HAS_ACCESS_ERRORS (DECL_TEMPLATE_INFO (NODE)))

/* Non-zero if this variable template specialization was specified using a
   template-id, so it's a partial or full specialization and not a definition
   of the member template of a particular class specialization.  */
#define TINFO_USED_TEMPLATE_ID(NODE) \
  (TREE_LANG_FLAG_1 (TEMPLATE_INFO_CHECK (NODE)))

/* The representation of a deferred access check.  */

struct GTY(()) deferred_access_check {
  /* The base class in which the declaration is referenced.  */
  tree binfo;
  /* The declaration whose access must be checked.  */
  tree decl;
  /* The declaration that should be used in the error message.  */
  tree diag_decl;
  /* The location of this access.  */
  location_t loc;
};

struct GTY(()) tree_template_info {
  struct tree_base base;
  tree tmpl;
  tree args;
  tree partial;
  vec<deferred_access_check, va_gc> *deferred_access_checks;
};

// Constraint information for a C++ declaration. Constraint information is
// comprised of:
//
// - a constraint expression introduced by the template header
// - a constraint expression introduced by a function declarator
// - the associated constraints, which are the conjunction of those,
//   and used for declaration matching
//
// The template and declarator requirements are kept to support pretty
// printing constrained declarations.
struct GTY(()) tree_constraint_info {
  struct tree_base base;
  tree template_reqs;
  tree declarator_reqs;
  tree associated_constr;
};

// Require that pointer P is non-null before returning.
template<typename T>
inline T*
check_nonnull (T* p)
{
  gcc_assert (p);
  return p;
}

/* Returns true iff T is non-null and represents constraint info.  */
inline tree_constraint_info *
check_constraint_info (tree t)
{
  if (t && TREE_CODE (t) == CONSTRAINT_INFO)
    return (tree_constraint_info *)t;
  return NULL;
}

/* Access the expression describing the template constraints. This may be
   null if no constraints were introduced in the template parameter list,
   a requirements clause after the template parameter list, or constraints
   through a constrained-type-specifier.  */
#define CI_TEMPLATE_REQS(NODE) \
  check_constraint_info (check_nonnull (NODE))->template_reqs

/* Access the expression describing the trailing constraints. This is non-null
   for any implicit instantiation of a constrained declaration. For a
   templated declaration it is non-null only when a trailing requires-clause
   was specified.  */
#define CI_DECLARATOR_REQS(NODE) \
  check_constraint_info (check_nonnull (NODE))->declarator_reqs

/* The computed associated constraint expression for a declaration.  */
#define CI_ASSOCIATED_CONSTRAINTS(NODE) \
  check_constraint_info (check_nonnull (NODE))->associated_constr

/* Access the constraint-expression introduced by the requires-clause
   associate the template parameter list NODE.  */
#define TEMPLATE_PARMS_CONSTRAINTS(NODE) \
  TREE_TYPE (TREE_LIST_CHECK (NODE))

/* Access the logical constraints on the template parameter declaration
   indicated by NODE.  */
#define TEMPLATE_PARM_CONSTRAINTS(NODE) \
  TREE_TYPE (TREE_LIST_CHECK (NODE))

/* Non-zero if the noexcept is present in a compound requirement.  */
#define COMPOUND_REQ_NOEXCEPT_P(NODE) \
  TREE_LANG_FLAG_0 (TREE_CHECK (NODE, COMPOUND_REQ))

/* A TREE_LIST whose TREE_VALUE is the constraints on the 'auto' placeholder
   type NODE, used in an argument deduction constraint.  The TREE_PURPOSE
   holds the set of template parameters that were in-scope when this 'auto'
   was formed.  */
#define PLACEHOLDER_TYPE_CONSTRAINTS_INFO(NODE) \
  DECL_SIZE_UNIT (TYPE_NAME (TEMPLATE_TYPE_PARM_CHECK (NODE)))

/* The constraints on the 'auto' placeholder type NODE.  */
#define PLACEHOLDER_TYPE_CONSTRAINTS(NODE)		   \
  (PLACEHOLDER_TYPE_CONSTRAINTS_INFO (NODE)		   \
   ? TREE_VALUE (PLACEHOLDER_TYPE_CONSTRAINTS_INFO (NODE)) \
   : NULL_TREE)

/* True if NODE is a constraint.  */
#define CONSTR_P(NODE)                  \
  (TREE_CODE (NODE) == ATOMIC_CONSTR    \
   || TREE_CODE (NODE) == CONJ_CONSTR   \
   || TREE_CODE (NODE) == DISJ_CONSTR)

/* Valid for any normalized constraint.  */
#define CONSTR_CHECK(NODE) \
  TREE_CHECK3 (NODE, ATOMIC_CONSTR, CONJ_CONSTR, DISJ_CONSTR)

/* The CONSTR_INFO stores normalization data for a constraint. It refers to
   the original expression and the expression or declaration
   from which the constraint was normalized.

   This is TREE_LIST whose TREE_PURPOSE is the original expression and whose
   TREE_VALUE is a list of contexts.  */
#define CONSTR_INFO(NODE) \
  TREE_TYPE (CONSTR_CHECK (NODE))

/* The expression evaluated by the constraint.  */
#define CONSTR_EXPR(NODE) \
  TREE_PURPOSE (CONSTR_INFO (NODE))

/* The expression or declaration from which this constraint was normalized.
   This is a TREE_LIST whose TREE_VALUE is either a template-id expression
   denoting a concept check or the declaration introducing the constraint.
   These are chained to other context objects.  */
#define CONSTR_CONTEXT(NODE) \
  TREE_VALUE (CONSTR_INFO (NODE))

/* The parameter mapping for an atomic constraint.  */
#define ATOMIC_CONSTR_MAP(NODE) \
  TREE_OPERAND (TREE_CHECK (NODE, ATOMIC_CONSTR), 0)

/* Whether the parameter mapping of this atomic constraint
   is already instantiated with concrete template arguments.
   Used only in satisfy_atom and in the satisfaction cache.  */
#define ATOMIC_CONSTR_MAP_INSTANTIATED_P(NODE) \
  TREE_LANG_FLAG_0 (ATOMIC_CONSTR_CHECK (NODE))

/* Whether the expression for this atomic constraint belongs to a
   concept definition.  */
#define ATOMIC_CONSTR_EXPR_FROM_CONCEPT_P(NODE) \
  TREE_LANG_FLAG_1 (ATOMIC_CONSTR_CHECK (NODE))

/* The expression of an atomic constraint.  */
#define ATOMIC_CONSTR_EXPR(NODE) \
  CONSTR_EXPR (ATOMIC_CONSTR_CHECK (NODE))

/* Whether a PARM_DECL represents a local parameter in a
   requires-expression.  */
#define CONSTRAINT_VAR_P(NODE) \
  DECL_LANG_FLAG_2 (TREE_CHECK (NODE, PARM_DECL))

/* The concept constraining this constrained template-parameter.  */
#define CONSTRAINED_PARM_CONCEPT(NODE) \
  DECL_SIZE_UNIT (TYPE_DECL_CHECK (NODE))
/* Any extra template arguments specified for a constrained
   template-parameter.  */
#define CONSTRAINED_PARM_EXTRA_ARGS(NODE) \
  DECL_SIZE (TYPE_DECL_CHECK (NODE))
/* The first template parameter of CONSTRAINED_PARM_CONCEPT to be used as a
   prototype for the constrained parameter in finish_shorthand_constraint,
   attached for convenience.  */
#define CONSTRAINED_PARM_PROTOTYPE(NODE) \
  DECL_INITIAL (TYPE_DECL_CHECK (NODE))

/* Module flags on FUNCTION,VAR,TYPE,CONCEPT or NAMESPACE
   A TEMPLATE_DECL holds them on the DECL_TEMPLATE_RESULT object --
   it's just not practical to keep them consistent.  */
#define DECL_MODULE_CHECK(NODE)						\
  TREE_NOT_CHECK (NODE, TEMPLATE_DECL)

/* In the purview of a named module (or in the purview of the
   header-unit being compiled).  */
#define DECL_MODULE_PURVIEW_P(N) \
  (DECL_LANG_SPECIFIC (DECL_MODULE_CHECK (N))->u.base.module_purview_p)

/* Attached to the named module it is in the purview of.  Decls
   attached to the global module will have this false.  */
#define DECL_MODULE_ATTACH_P(N) \
  (DECL_LANG_SPECIFIC (DECL_MODULE_CHECK (N))->u.base.module_attach_p)

/* True if the live version of the decl was imported.  */
#define DECL_MODULE_IMPORT_P(NODE) \
  (DECL_LANG_SPECIFIC (DECL_MODULE_CHECK (NODE))->u.base.module_import_p)

/* True if this decl is in the entity hash & array.  This means that
   some variant was imported, even if DECL_MODULE_IMPORT_P is false.  */
#define DECL_MODULE_ENTITY_P(NODE) \
  (DECL_LANG_SPECIFIC (DECL_MODULE_CHECK (NODE))->u.base.module_entity_p)

/* DECL that has attached decls for ODR-relatedness.  */
#define DECL_MODULE_KEYED_DECLS_P(NODE) \
  (DECL_LANG_SPECIFIC (DECL_MODULE_CHECK (NODE))->u.base.module_keyed_decls_p)

/* Whether this is an exported DECL.  Held on any decl that can appear
   at namespace scope (function, var, type, template, const or
   namespace).  templates copy from their template_result, consts have
   it for unscoped enums.  */
#define DECL_MODULE_EXPORT_P(NODE) TREE_LANG_FLAG_3 (NODE)

/* Represents a streamed-in translation-unit-local entity.  Any use of
   this node when instantiating a template should emit an error.  */
struct GTY(()) tree_tu_local_entity {
  struct tree_base base;
  tree name;
  location_t loc;
};

/* The name of a translation-unit-local entity.  */
#define TU_LOCAL_ENTITY_NAME(NODE) \
  (((struct tree_tu_local_entity *)TU_LOCAL_ENTITY_CHECK (NODE))->name)

/* The source location of the translation-unit-local entity.  */
#define TU_LOCAL_ENTITY_LOCATION(NODE) \
  (((struct tree_tu_local_entity *)TU_LOCAL_ENTITY_CHECK (NODE))->loc)


/* The list of local parameters introduced by this requires-expression,
   in the form of a chain of PARM_DECLs.  */
#define REQUIRES_EXPR_PARMS(NODE) \
  TREE_OPERAND (TREE_CHECK (NODE, REQUIRES_EXPR), 0)

/* A TREE_LIST of the requirements for this requires-expression.
   The requirements are stored in lexical order within the TREE_VALUE
   of each TREE_LIST node.  The TREE_PURPOSE of each node is unused.  */
#define REQUIRES_EXPR_REQS(NODE) \
  TREE_OPERAND (TREE_CHECK (NODE, REQUIRES_EXPR), 1)

/* Like PACK_EXPANSION_EXTRA_ARGS, for requires-expressions.  */
#define REQUIRES_EXPR_EXTRA_ARGS(NODE) \
  TREE_OPERAND (TREE_CHECK (NODE, REQUIRES_EXPR), 2)

enum cp_tree_node_structure_enum {
  TS_CP_GENERIC,
  TS_CP_IDENTIFIER,
  TS_CP_TPI,
  TS_CP_PTRMEM,
  TS_CP_OVERLOAD,
  TS_CP_BINDING_VECTOR,
  TS_CP_BASELINK,
  TS_CP_TEMPLATE_DECL,
  TS_CP_DEFERRED_PARSE,
  TS_CP_DEFERRED_NOEXCEPT,
  TS_CP_STATIC_ASSERT,
  TS_CP_ARGUMENT_PACK_SELECT,
  TS_CP_TRAIT_EXPR,
  TS_CP_LAMBDA_EXPR,
  TS_CP_TEMPLATE_INFO,
  TS_CP_CONSTRAINT_INFO,
  TS_CP_USERDEF_LITERAL,
  TS_CP_TU_LOCAL_ENTITY
};

/* The resulting tree type.  */
union GTY((desc ("cp_tree_node_structure (&%h)"),
       chain_next ("(union lang_tree_node *) c_tree_chain_next (&%h.generic)"))) lang_tree_node {
  union tree_node GTY ((tag ("TS_CP_GENERIC"),
			desc ("tree_node_structure (&%h)"))) generic;
  struct template_parm_index GTY ((tag ("TS_CP_TPI"))) tpi;
  struct ptrmem_cst GTY ((tag ("TS_CP_PTRMEM"))) ptrmem;
  struct tree_overload GTY ((tag ("TS_CP_OVERLOAD"))) overload;
  struct tree_binding_vec GTY ((tag ("TS_CP_BINDING_VECTOR"))) binding_vec;
  struct tree_baselink GTY ((tag ("TS_CP_BASELINK"))) baselink;
  struct tree_template_decl GTY ((tag ("TS_CP_TEMPLATE_DECL"))) template_decl;
  struct tree_deferred_parse GTY ((tag ("TS_CP_DEFERRED_PARSE"))) deferred_parse;
  struct tree_deferred_noexcept GTY ((tag ("TS_CP_DEFERRED_NOEXCEPT"))) deferred_noexcept;
  struct lang_identifier GTY ((tag ("TS_CP_IDENTIFIER"))) identifier;
  struct tree_static_assert GTY ((tag ("TS_CP_STATIC_ASSERT")))
    static_assertion;
  struct tree_argument_pack_select GTY ((tag ("TS_CP_ARGUMENT_PACK_SELECT")))
    argument_pack_select;
  struct tree_trait_expr GTY ((tag ("TS_CP_TRAIT_EXPR")))
    trait_expression;
  struct tree_lambda_expr GTY ((tag ("TS_CP_LAMBDA_EXPR")))
    lambda_expression;
  struct tree_template_info GTY ((tag ("TS_CP_TEMPLATE_INFO")))
    template_info;
  struct tree_constraint_info GTY ((tag ("TS_CP_CONSTRAINT_INFO")))
    constraint_info;
  struct tree_userdef_literal GTY ((tag ("TS_CP_USERDEF_LITERAL")))
    userdef_literal;
  struct tree_tu_local_entity GTY ((tag ("TS_CP_TU_LOCAL_ENTITY")))
    tu_local_entity;
};


struct GTY(()) cp_omp_declare_target_attr {
  bool attr_syntax;
  int device_type;
  bool indirect;
};

struct GTY(()) cp_omp_begin_assumes_data {
  bool attr_syntax;
};

/* Global state.  */

struct GTY(()) saved_scope {
  vec<cxx_saved_binding, va_gc> *old_bindings;
  tree old_namespace;
  vec<tree, va_gc> *decl_ns_list;
  tree class_name;
  tree class_type;
  tree access_specifier;
  tree function_decl;
  vec<tree, va_gc> *lang_base;
  tree lang_name;
  tree template_parms;
  cp_binding_level *x_previous_class_level;
  tree x_saved_tree;

  /* Only used for uses of this in trailing return type.  */
  tree x_current_class_ptr;
  tree x_current_class_ref;

  int x_processing_template_decl;
  int x_processing_specialization;
  int x_processing_constraint;
  int x_processing_contract_condition;
  int suppress_location_wrappers;
  BOOL_BITFIELD x_processing_explicit_instantiation : 1;
  BOOL_BITFIELD need_pop_function_context : 1;
  BOOL_BITFIELD x_processing_omp_trait_property_expr : 1;

  /* Nonzero if we are parsing the discarded statement of a constexpr
     if-statement.  */
  BOOL_BITFIELD discarded_stmt : 1;
  /* Nonzero if we are parsing or instantiating the compound-statement
     of consteval if statement.  Also set while processing an immediate
     invocation.  */
  BOOL_BITFIELD consteval_if_p : 1;

  int unevaluated_operand;
  int inhibit_evaluation_warnings;
  int noexcept_operand;
  int ref_temp_count;

  struct stmt_tree_s x_stmt_tree;

  cp_binding_level *class_bindings;
  cp_binding_level *bindings;

  hash_map<tree, tree> *GTY((skip)) x_local_specializations;
  vec<cp_omp_declare_target_attr, va_gc> *omp_declare_target_attribute;
  vec<cp_omp_begin_assumes_data, va_gc> *omp_begin_assumes;

  struct saved_scope *prev;
};

extern GTY(()) struct saved_scope *scope_chain;

/* The current open namespace.  */

#define current_namespace scope_chain->old_namespace

/* The stack for namespaces of current declarations.  */

#define decl_namespace_list scope_chain->decl_ns_list

/* IDENTIFIER_NODE: name of current class */

#define current_class_name scope_chain->class_name

/* _TYPE: the type of the current class */

#define current_class_type scope_chain->class_type

/* When parsing a class definition, the access specifier most recently
   given by the user, or, if no access specifier was given, the
   default value appropriate for the kind of class (i.e., struct,
   class, or union).  */

#define current_access_specifier scope_chain->access_specifier

/* Pointer to the top of the language name stack.  */

#define current_lang_base scope_chain->lang_base
#define current_lang_name scope_chain->lang_name

/* When parsing a template declaration, a TREE_LIST represents the
   active template parameters.  Each node in the list represents one
   level of template parameters.  The innermost level is first in the
   list.  The depth of each level is stored as an INTEGER_CST in the
   TREE_PURPOSE of each node.  The parameters for that level are
   stored in the TREE_VALUE.  */

#define current_template_parms scope_chain->template_parms
#define current_template_depth \
  (current_template_parms ? TMPL_PARMS_DEPTH (current_template_parms) : 0)
#define in_template_context (current_template_parms != NULL_TREE)

#define processing_template_decl scope_chain->x_processing_template_decl
#define processing_specialization scope_chain->x_processing_specialization
#define processing_explicit_instantiation scope_chain->x_processing_explicit_instantiation
#define processing_omp_trait_property_expr scope_chain->x_processing_omp_trait_property_expr

/* Nonzero if we are parsing the conditional expression of a contract
   condition. These expressions appear outside the parameter list (like a
   trailing return type), but are potentially evaluated.  */

#define processing_contract_condition scope_chain->x_processing_contract_condition

#define in_discarded_stmt scope_chain->discarded_stmt
#define in_consteval_if_p scope_chain->consteval_if_p

#define current_ref_temp_count scope_chain->ref_temp_count

/* RAII sentinel to handle clearing processing_template_decl and restoring
   it when done.  */

class processing_template_decl_sentinel
{
public:
  int saved;
  processing_template_decl_sentinel (bool reset = true)
    : saved (processing_template_decl)
  {
    if (reset)
      processing_template_decl = 0;
  }
  ~processing_template_decl_sentinel()
  {
    processing_template_decl = saved;
  }
};

/* RAII sentinel to disable certain warnings during template substitution
   and elsewhere.  */

class warning_sentinel
{
public:
  int &flag;
  int val;
  warning_sentinel(int& flag, bool suppress=true)
    : flag(flag), val(flag) { if (suppress) flag = 0; }
  ~warning_sentinel() { flag = val; }
};

/* RAII sentinel to temporarily override input_location.  This will not set
   input_location to UNKNOWN_LOCATION or BUILTINS_LOCATION.  */

class iloc_sentinel
{
  location_t saved_loc;
public:
  iloc_sentinel (location_t loc): saved_loc (input_location)
  {
    if (loc >= RESERVED_LOCATION_COUNT)
      input_location = loc;
  }
  ~iloc_sentinel ()
  {
    input_location = saved_loc;
  }
};

/* RAII sentinel that saves the value of a variable, optionally
   overrides it right away, and restores its value when the sentinel
   id destructed.  */

template <typename T>
class temp_override
{
  T& overridden_variable;
  T saved_value;
public:
  temp_override(T& var) : overridden_variable (var), saved_value (var) {}
  temp_override(T& var, T overrider)
    : overridden_variable (var), saved_value (var)
  {
    overridden_variable = overrider;
  }
  ~temp_override() { overridden_variable = saved_value; }
};

/* Wrapping a template parameter in type_identity_t hides it from template
   argument deduction.  */
#if __cpp_lib_type_identity
using std::type_identity_t;
#else
template <typename T>
struct type_identity { typedef T type; };
template <typename T>
using type_identity_t = typename type_identity<T>::type;
#endif

/* Object generator function for temp_override, so you don't need to write the
   type of the object as a template argument.

   Use as auto x = make_temp_override (flag); */

template <typename T>
inline temp_override<T>
make_temp_override (T& var)
{
  return { var };
}

/* Likewise, but use as auto x = make_temp_override (flag, value); */

template <typename T>
inline temp_override<T>
make_temp_override (T& var, type_identity_t<T> overrider)
{
  return { var, overrider };
}

/* temp_override for in_consteval_if_p, which can't use make_temp_override
   because it is a bitfield.  */

struct in_consteval_if_p_temp_override {
  bool save_in_consteval_if_p;
  in_consteval_if_p_temp_override ()
    : save_in_consteval_if_p (in_consteval_if_p) {}
  void reset () { in_consteval_if_p = save_in_consteval_if_p; }
  ~in_consteval_if_p_temp_override ()
  { reset (); }
};

/* The cached class binding level, from the most recently exited
   class, or NULL if none.  */

#define previous_class_level scope_chain->x_previous_class_level

/* A map from local variable declarations in the body of the template
   presently being instantiated to the corresponding instantiated
   local variables.  */

#define local_specializations scope_chain->x_local_specializations

/* Nonzero if we are parsing the operand of a noexcept operator.  */

#define cp_noexcept_operand scope_chain->noexcept_operand

struct named_label_entry; /* Defined in decl.cc.  */

struct named_label_hash : ggc_remove <named_label_entry *>
{
  typedef named_label_entry *value_type;
  typedef tree compare_type; /* An identifier.  */

  inline static hashval_t hash (value_type);
  inline static bool equal (const value_type, compare_type);

  static const bool empty_zero_p = true;
  inline static void mark_empty (value_type &p) {p = NULL;}
  inline static bool is_empty (value_type p) {return !p;}

  /* Nothing is deletable.  Everything is insertable.  */
  inline static bool is_deleted (value_type) { return false; }
  inline static void mark_deleted (value_type) { gcc_unreachable (); }
};

/* Global state pertinent to the current function.  */

struct GTY(()) language_function {
  struct c_language_function base;

  tree x_current_class_ptr;
  tree x_current_class_ref;
  tree x_eh_spec_block;
  tree x_in_charge_parm;
  tree x_vtt_parm;
  tree x_return_value;

  BOOL_BITFIELD returns_value : 1;
  BOOL_BITFIELD returns_null : 1;
  BOOL_BITFIELD returns_abnormally : 1;
  BOOL_BITFIELD infinite_loop: 1;
  BOOL_BITFIELD x_in_function_try_handler : 1;
  BOOL_BITFIELD x_in_base_initializer : 1;

  /* True if this function can throw an exception.  */
  BOOL_BITFIELD can_throw : 1;

  BOOL_BITFIELD invalid_constexpr : 1;
  BOOL_BITFIELD throwing_cleanup : 1;
  /* True if we gave any errors in this function.  */
  BOOL_BITFIELD erroneous : 1;

  hash_table<named_label_hash> *x_named_labels;

  cp_binding_level *bindings;

  /* Tracking possibly infinite loops.  This is a vec<tree> only because
     vec<bool> doesn't work with gtype.  */
  vec<tree, va_gc> *infinite_loops;
};

/* The current C++-specific per-function global variables.  */

#define cp_function_chain (cfun->language)

/* When we're processing a member function, current_class_ptr is the
   PARM_DECL for the `this' pointer.  The current_class_ref is an
   expression for `*this'.  */

#define current_class_ptr			\
  (*(cfun && cp_function_chain			\
     ? &cp_function_chain->x_current_class_ptr	\
     : &scope_chain->x_current_class_ptr))
#define current_class_ref			\
  (*(cfun && cp_function_chain			\
     ? &cp_function_chain->x_current_class_ref	\
     : &scope_chain->x_current_class_ref))

/* The EH_SPEC_BLOCK for the exception-specifiers for the current
   function, if any.  */

#define current_eh_spec_block cp_function_chain->x_eh_spec_block

/* The `__in_chrg' parameter for the current function.  Only used for
   constructors and destructors.  */

#define current_in_charge_parm cp_function_chain->x_in_charge_parm

/* The `__vtt_parm' parameter for the current function.  Only used for
   constructors and destructors.  */

#define current_vtt_parm cp_function_chain->x_vtt_parm

/* A boolean flag to control whether we need to clean up the return value if a
   local destructor throws.  Only used in functions that return by value a
   class with a destructor.  Which 'tors don't, so we can use the same
   field as current_vtt_parm.  */

#define current_retval_sentinel current_vtt_parm

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement that specifies a return value is seen.  */

#define current_function_returns_value cp_function_chain->returns_value

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement with no argument is seen.  */

#define current_function_returns_null cp_function_chain->returns_null

/* Set to 0 at beginning of a function definition, set to 1 if
   a call to a noreturn function is seen.  */

#define current_function_returns_abnormally \
  cp_function_chain->returns_abnormally

/* Set to 0 at beginning of a function definition, set to 1 if we see an
   obvious infinite loop.  This can have false positives and false
   negatives, so it should only be used as a heuristic.  */

#define current_function_infinite_loop cp_function_chain->infinite_loop

/* Nonzero if we are processing a base initializer.  Zero elsewhere.  */
#define in_base_initializer cp_function_chain->x_in_base_initializer

#define in_function_try_handler cp_function_chain->x_in_function_try_handler

/* Expression always returned from function, or error_mark_node
   otherwise, for use by the automatic named return value optimization.  */

#define current_function_return_value \
  (cp_function_chain->x_return_value)

/* In parser.cc.  */
extern tree cp_literal_operator_id (const char *);

#define NON_ERROR(NODE) ((NODE) == error_mark_node ? NULL_TREE : (NODE))

/* TRUE if a tree code represents a statement.  */
extern bool statement_code_p[MAX_TREE_CODES];

#define STATEMENT_CODE_P(CODE) statement_code_p[(int) (CODE)]

enum languages { lang_c, lang_cplusplus };

/* Macros to make error reporting functions' lives easier.  */
#define TYPE_LINKAGE_IDENTIFIER(NODE) \
  (TYPE_IDENTIFIER (TYPE_MAIN_VARIANT (NODE)))
#define TYPE_NAME_STRING(NODE) (IDENTIFIER_POINTER (TYPE_IDENTIFIER (NODE)))
#define TYPE_NAME_LENGTH(NODE) (IDENTIFIER_LENGTH (TYPE_IDENTIFIER (NODE)))

/* Any kind of anonymous type.  */
#define TYPE_ANON_P(NODE)					\
  (TYPE_LINKAGE_IDENTIFIER (NODE)				\
   && IDENTIFIER_ANON_P (TYPE_LINKAGE_IDENTIFIER (NODE)))

/* Nonzero if NODE, a TYPE, has no name for linkage purposes.  */
#define TYPE_UNNAMED_P(NODE)					\
  (TYPE_ANON_P (NODE)						\
   && !IDENTIFIER_LAMBDA_P (TYPE_LINKAGE_IDENTIFIER (NODE)))

/* The _DECL for this _TYPE.  */
#define TYPE_MAIN_DECL(NODE) (TYPE_STUB_DECL (TYPE_MAIN_VARIANT (NODE)))

/* Nonzero if T is a type that could resolve to any kind of concrete type
   at instantiation time.  */
#define WILDCARD_TYPE_P(T)				\
  (TREE_CODE (T) == TEMPLATE_TYPE_PARM			\
   || TREE_CODE (T) == TYPENAME_TYPE			\
   || TREE_CODE (T) == TYPEOF_TYPE			\
   || TREE_CODE (T) == BOUND_TEMPLATE_TEMPLATE_PARM	\
   || TREE_CODE (T) == UNBOUND_CLASS_TEMPLATE		\
   || TREE_CODE (T) == DECLTYPE_TYPE			\
   || TREE_CODE (T) == TRAIT_TYPE			\
   || TREE_CODE (T) == DEPENDENT_OPERATOR_TYPE		\
   || TREE_CODE (T) == PACK_INDEX_TYPE)

/* Nonzero if T is a class (or struct or union) type.  Also nonzero
   for template type parameters, typename types, and instantiated
   template template parameters.  Keep these checks in ascending code
   order.  */
#define MAYBE_CLASS_TYPE_P(T) (WILDCARD_TYPE_P (T) || CLASS_TYPE_P (T))

/* Set CLASS_TYPE_P for T to VAL.  T must be a class, struct, or
   union type.  */
#define SET_CLASS_TYPE_P(T, VAL) \
  (TYPE_LANG_FLAG_5 (RECORD_OR_UNION_CHECK (T)) = (VAL))

/* Nonzero if T is a class type.  Zero for template type parameters,
   typename types, and so forth.  */
#define CLASS_TYPE_P(T) \
  (RECORD_OR_UNION_CODE_P (TREE_CODE (T)) && TYPE_LANG_FLAG_5 (T))

/* Nonzero if T is a class type but not a union.  */
#define NON_UNION_CLASS_TYPE_P(T) \
  (TREE_CODE (T) == RECORD_TYPE && TYPE_LANG_FLAG_5 (T))

/* Nonzero if T is a class type and is a union.  */
#define UNION_TYPE_P(T) \
  (TREE_CODE (T) == UNION_TYPE && TYPE_LANG_FLAG_5 (T))

/* Keep these checks in ascending code order.  */
#define RECORD_OR_UNION_CODE_P(T)	\
  ((T) == RECORD_TYPE || (T) == UNION_TYPE)
#define OVERLOAD_TYPE_P(T) \
  (CLASS_TYPE_P (T) || TREE_CODE (T) == ENUMERAL_TYPE)

/* True if this type is dependent.  This predicate is only valid if
   TYPE_DEPENDENT_P_VALID is true.  */
#define TYPE_DEPENDENT_P(NODE) TYPE_LANG_FLAG_0 (NODE)

/* True if dependent_type_p has been called for this type, with the
   result that TYPE_DEPENDENT_P is valid.  */
#define TYPE_DEPENDENT_P_VALID(NODE) TYPE_LANG_FLAG_6(NODE)

/* Nonzero if this type is const-qualified.  */
#define CP_TYPE_CONST_P(NODE)				\
  ((cp_type_quals (NODE) & TYPE_QUAL_CONST) != 0)

/* Nonzero if this type is volatile-qualified.  */
#define CP_TYPE_VOLATILE_P(NODE)			\
  ((cp_type_quals (NODE) & TYPE_QUAL_VOLATILE) != 0)

/* Nonzero if this type is restrict-qualified.  */
#define CP_TYPE_RESTRICT_P(NODE)			\
  ((cp_type_quals (NODE) & TYPE_QUAL_RESTRICT) != 0)

/* Nonzero if this type is const-qualified, but not
   volatile-qualified.  Other qualifiers are ignored.  This macro is
   used to test whether or not it is OK to bind an rvalue to a
   reference.  */
#define CP_TYPE_CONST_NON_VOLATILE_P(NODE)				\
  ((cp_type_quals (NODE) & (TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE))	\
   == TYPE_QUAL_CONST)

#define FUNCTION_ARG_CHAIN(NODE) \
  TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (NODE)))

/* Given a FUNCTION_DECL, returns the first TREE_LIST out of TYPE_ARG_TYPES
   which refers to a user-written parameter.  */
#define FUNCTION_FIRST_USER_PARMTYPE(NODE) \
  skip_artificial_parms_for ((NODE), TYPE_ARG_TYPES (TREE_TYPE (NODE)))

/* Similarly, but for DECL_ARGUMENTS.  */
#define FUNCTION_FIRST_USER_PARM(NODE) \
  skip_artificial_parms_for ((NODE), DECL_ARGUMENTS (NODE))

/* Nonzero iff TYPE is derived from PARENT. Ignores accessibility and
   ambiguity issues.  */
#define DERIVED_FROM_P(PARENT, TYPE) \
  (lookup_base ((TYPE), (PARENT), ba_any, NULL, tf_none) != NULL_TREE)

/* Gives the visibility specification for a class type.  */
#define CLASSTYPE_VISIBILITY(TYPE)		\
	DECL_VISIBILITY (TYPE_MAIN_DECL (TYPE))
#define CLASSTYPE_VISIBILITY_SPECIFIED(TYPE)	\
	DECL_VISIBILITY_SPECIFIED (TYPE_MAIN_DECL (TYPE))

struct GTY (()) tree_pair_s {
  tree purpose;
  tree value;
};
typedef tree_pair_s *tree_pair_p;

/* This structure provides additional information above and beyond
   what is provide in the ordinary tree_type.  In the past, we used it
   for the types of class types, template parameters types, typename
   types, and so forth.  However, there can be many (tens to hundreds
   of thousands) of template parameter types in a compilation, and
   there's no need for this additional information in that case.
   Therefore, we now use this data structure only for class types.

   In the past, it was thought that there would be relatively few
   class types.  However, in the presence of heavy use of templates,
   many (i.e., thousands) of classes can easily be generated.
   Therefore, we should endeavor to keep the size of this structure to
   a minimum.  */
struct GTY(()) lang_type {
  unsigned char align;

  unsigned has_type_conversion : 1;
  unsigned has_copy_ctor : 1;
  unsigned has_default_ctor : 1;
  unsigned const_needs_init : 1;
  unsigned ref_needs_init : 1;
  unsigned has_const_copy_assign : 1;
  unsigned use_template : 2;

  unsigned has_mutable : 1;
  unsigned com_interface : 1;
  unsigned non_pod_class : 1;
  unsigned nearly_empty_p : 1;
  unsigned user_align : 1;
  unsigned has_copy_assign : 1;
  unsigned has_new : 1;
  unsigned has_array_new : 1;

  unsigned gets_delete : 2;
  unsigned interface_only : 1;
  unsigned interface_unknown : 1;
  unsigned contains_empty_class_p : 1;
  unsigned anon_aggr : 1;
  unsigned non_zero_init : 1;
  unsigned empty_p : 1;
  /* 32 bits allocated.  */

  unsigned vec_new_uses_cookie : 1;
  unsigned declared_class : 1;
  unsigned diamond_shaped : 1;
  unsigned repeated_base : 1;
  unsigned being_defined : 1;
  unsigned debug_requested : 1;
  unsigned fields_readonly : 1;
  unsigned ptrmemfunc_flag : 1;

  unsigned lazy_default_ctor : 1;
  unsigned lazy_copy_ctor : 1;
  unsigned lazy_copy_assign : 1;
  unsigned lazy_destructor : 1;
  unsigned has_const_copy_ctor : 1;
  unsigned has_complex_copy_ctor : 1;
  unsigned has_complex_copy_assign : 1;
  unsigned non_aggregate : 1;

  unsigned has_complex_dflt : 1;
  unsigned has_list_ctor : 1;
  unsigned non_std_layout : 1;
  unsigned is_literal : 1;
  unsigned lazy_move_ctor : 1;
  unsigned lazy_move_assign : 1;
  unsigned has_complex_move_ctor : 1;
  unsigned has_complex_move_assign : 1;

  unsigned has_constexpr_ctor : 1;
  unsigned unique_obj_representations : 1;
  unsigned unique_obj_representations_set : 1;
  bool erroneous : 1;
  bool non_pod_aggregate : 1;
  bool non_aggregate_pod : 1;
  bool trivially_relocatable : 1;
  bool trivially_relocatable_computed : 1;

  bool replaceable : 1;
  bool replaceable_computed : 1;

  /* When adding a flag here, consider whether or not it ought to
     apply to a template instance if it applies to the template.  If
     so, make sure to copy it in instantiate_class_template!

     Also make sure new flags here are streamed in module.cc.  */

  /* There are some bits left to fill out a 32-bit word.  Keep track
     of this by updating the size of this bitfield whenever you add or
     remove a flag.  */
  unsigned dummy : 30;

  tree primary_base;
  vec<tree_pair_s, va_gc> *vcall_indices;
  tree vtables;
  tree typeinfo_var;
  vec<tree, va_gc> *vbases;
  tree as_base;
  vec<tree, va_gc> *pure_virtuals;
  tree friend_classes;
  vec<tree, va_gc> * GTY((reorder ("resort_type_member_vec"))) members;
  /* CLASSTYPE_KEY_METHOD for TYPE_POLYMORPHIC_P types, CLASSTYPE_LAMBDA_EXPR
     otherwise.  */
  tree key_method;
  tree decl_list;
  tree befriending_classes;
  union maybe_objc_info {
    /* If not c_dialect_objc, this part is not even allocated.  */
    char GTY((tag ("0"))) non_objc;
    /* In a RECORD_TYPE, information specific to Objective-C, such
       as a list of adopted protocols or a pointer to a corresponding
       @interface.  See objc/objc-act.h for details.  */
    tree GTY((tag ("1"))) objc_info;
  } GTY ((desc ("c_dialect_objc ()"))) info;
};

/* We used to have a variant type for lang_type.  Keep the name of the
   checking accessor for the sole survivor.  */
#define LANG_TYPE_CLASS_CHECK(NODE) (TYPE_LANG_SPECIFIC (NODE))

/* Nonzero for _CLASSTYPE means that operator delete is defined.  */
#define TYPE_GETS_DELETE(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->gets_delete)
#define TYPE_GETS_REG_DELETE(NODE) (TYPE_GETS_DELETE (NODE) & 1)
#define TYPE_GETS_VEC_DELETE(NODE) (TYPE_GETS_DELETE (NODE) & 2)

/* Nonzero if `new NODE[x]' should cause the allocation of extra
   storage to indicate how many array elements are in use.  */
#define TYPE_VEC_NEW_USES_COOKIE(NODE)			\
  (CLASS_TYPE_P (NODE)					\
   && LANG_TYPE_CLASS_CHECK (NODE)->vec_new_uses_cookie)

/* Nonzero means that this _CLASSTYPE node defines ways of converting
   itself to other types.  */
#define TYPE_HAS_CONVERSION(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->has_type_conversion)

/* Nonzero means that NODE (a class type) has a default constructor --
   but that it has not yet been declared.  */
#define CLASSTYPE_LAZY_DEFAULT_CTOR(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->lazy_default_ctor)

/* Nonzero means that NODE (a class type) has a copy constructor --
   but that it has not yet been declared.  */
#define CLASSTYPE_LAZY_COPY_CTOR(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->lazy_copy_ctor)

/* Nonzero means that NODE (a class type) has a move constructor --
   but that it has not yet been declared.  */
#define CLASSTYPE_LAZY_MOVE_CTOR(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->lazy_move_ctor)

/* Nonzero means that NODE (a class type) has an assignment operator
   -- but that it has not yet been declared.  */
#define CLASSTYPE_LAZY_COPY_ASSIGN(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->lazy_copy_assign)

/* Nonzero means that NODE (a class type) has an assignment operator
   -- but that it has not yet been declared.  */
#define CLASSTYPE_LAZY_MOVE_ASSIGN(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->lazy_move_assign)

/* Nonzero means that NODE (a class type) has a destructor -- but that
   it has not yet been declared.  */
#define CLASSTYPE_LAZY_DESTRUCTOR(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->lazy_destructor)

/* Nonzero means that NODE (a class type) is final */
#define CLASSTYPE_FINAL(NODE) \
  TYPE_FINAL_P (NODE)


/* Nonzero means that this _CLASSTYPE node overloads operator=(X&).  */
#define TYPE_HAS_COPY_ASSIGN(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->has_copy_assign)

/* True iff the class type NODE has an "operator =" whose parameter
   has a parameter of type "const X&".  */
#define TYPE_HAS_CONST_COPY_ASSIGN(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->has_const_copy_assign)

/* Nonzero means that this _CLASSTYPE node has an X(X&) constructor.  */
#define TYPE_HAS_COPY_CTOR(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->has_copy_ctor)
#define TYPE_HAS_CONST_COPY_CTOR(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->has_const_copy_ctor)

/* Nonzero if this class has an X(initializer_list<T>) constructor.  */
#define TYPE_HAS_LIST_CTOR(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->has_list_ctor)

/* Nonzero if this class has a constexpr constructor other than a copy/move
   constructor.  Note that a class can have constexpr constructors for
   static initialization even if it isn't a literal class.  */
#define TYPE_HAS_CONSTEXPR_CTOR(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->has_constexpr_ctor)

/* Nonzero if this class defines an overloaded operator new.  (An
   operator new [] doesn't count.)  */
#define TYPE_HAS_NEW_OPERATOR(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->has_new)

/* Nonzero if this class defines an overloaded operator new[].  */
#define TYPE_HAS_ARRAY_NEW_OPERATOR(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->has_array_new)

/* Nonzero means that this type is being defined.  I.e., the left brace
   starting the definition of this type has been seen.  */
#define TYPE_BEING_DEFINED(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->being_defined)

/* Nonzero means that this type is either complete or being defined, so we
   can do lookup in it.  */
#define COMPLETE_OR_OPEN_TYPE_P(NODE) \
  (COMPLETE_TYPE_P (NODE) || (CLASS_TYPE_P (NODE) && TYPE_BEING_DEFINED (NODE)))

/* Mark bits for repeated base checks.  */
#define TYPE_MARKED_P(NODE) TREE_LANG_FLAG_6 (TYPE_CHECK (NODE))

/* Nonzero if the class NODE has multiple paths to the same (virtual)
   base object.  */
#define CLASSTYPE_DIAMOND_SHAPED_P(NODE) \
  (LANG_TYPE_CLASS_CHECK(NODE)->diamond_shaped)

/* Nonzero if the class NODE has multiple instances of the same base
   type.  */
#define CLASSTYPE_REPEATED_BASE_P(NODE) \
  (LANG_TYPE_CLASS_CHECK(NODE)->repeated_base)

/* The member function with which the vtable will be emitted:
   the first noninline non-pure-virtual member function.  NULL_TREE
   if there is no key function or if this is a class template */
#define CLASSTYPE_KEY_METHOD(NODE) \
  (TYPE_POLYMORPHIC_P (NODE)					\
   ? LANG_TYPE_CLASS_CHECK (NODE)->key_method			\
   : NULL_TREE)
#define SET_CLASSTYPE_KEY_METHOD(NODE, VALUE) \
  (gcc_checking_assert (TYPE_POLYMORPHIC_P (NODE)),		\
   LANG_TYPE_CLASS_CHECK (NODE)->key_method = (VALUE))

/* Vector of members.  During definition, it is unordered and only
   member functions are present.  After completion it is sorted and
   contains both member functions and non-functions.  STAT_HACK is
   involved to preserve oneslot per name invariant.  */
#define CLASSTYPE_MEMBER_VEC(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->members)

/* For class templates, this is a TREE_LIST of all member data,
   functions, types, and friends in the order of declaration.
   The TREE_PURPOSE of each TREE_LIST is NULL_TREE for a friend,
   and the RECORD_TYPE for the class template otherwise.  */
#define CLASSTYPE_DECL_LIST(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->decl_list)

/* A FUNCTION_DECL or OVERLOAD for the constructors for NODE.  These
   are the constructors that take an in-charge parameter.  */
#define CLASSTYPE_CONSTRUCTORS(NODE) \
  (get_class_binding_direct (NODE, ctor_identifier))

/* A FUNCTION_DECL for the destructor for NODE.  This is the
   destructors that take an in-charge parameter.  If
   CLASSTYPE_LAZY_DESTRUCTOR is true, then this entry will be NULL
   until the destructor is created with lazily_declare_fn.  */
#define CLASSTYPE_DESTRUCTOR(NODE) \
  (get_class_binding_direct (NODE, dtor_identifier))

/* Nonzero if NODE has a primary base class, i.e., a base class with
   which it shares the virtual function table pointer.  */
#define CLASSTYPE_HAS_PRIMARY_BASE_P(NODE) \
  (CLASSTYPE_PRIMARY_BINFO (NODE) != NULL_TREE)

/* If non-NULL, this is the binfo for the primary base class, i.e.,
   the base class which contains the virtual function table pointer
   for this class.  */
#define CLASSTYPE_PRIMARY_BINFO(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->primary_base)

/* A vector of BINFOs for the direct and indirect virtual base classes
   that this type uses in a post-order depth-first left-to-right
   order.  (In other words, these bases appear in the order that they
   should be initialized.)  */
#define CLASSTYPE_VBASECLASSES(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->vbases)

/* The type corresponding to NODE when NODE is used as a base class,
   i.e., NODE without virtual base classes or tail padding.  */
#define CLASSTYPE_AS_BASE(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->as_base)

/* True iff NODE is the CLASSTYPE_AS_BASE version of some type.  */
#define IS_FAKE_BASE_TYPE(NODE)					\
  (RECORD_OR_UNION_TYPE_P (NODE)				\
   && TYPE_CONTEXT (NODE) && CLASS_TYPE_P (TYPE_CONTEXT (NODE))	\
   && CLASSTYPE_AS_BASE (TYPE_CONTEXT (NODE)) == (NODE))

/* These are the size and alignment of the type without its virtual
   base classes, for when we use this type as a base itself.  */
#define CLASSTYPE_SIZE(NODE) TYPE_SIZE (CLASSTYPE_AS_BASE (NODE))
#define CLASSTYPE_SIZE_UNIT(NODE) TYPE_SIZE_UNIT (CLASSTYPE_AS_BASE (NODE))
#define CLASSTYPE_ALIGN(NODE) TYPE_ALIGN (CLASSTYPE_AS_BASE (NODE))
#define CLASSTYPE_USER_ALIGN(NODE) TYPE_USER_ALIGN (CLASSTYPE_AS_BASE (NODE))

/* The alignment of NODE, without its virtual bases, in bytes.  */
#define CLASSTYPE_ALIGN_UNIT(NODE) \
  (CLASSTYPE_ALIGN (NODE) / BITS_PER_UNIT)

/* A vec<tree> of virtual functions which cannot be inherited by
   derived classes.  When deriving from this type, the derived
   class must provide its own definition for each of these functions.  */
#define CLASSTYPE_PURE_VIRTUALS(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->pure_virtuals)

/* Nonzero means that this type is an abstract class type.  */
#define ABSTRACT_CLASS_TYPE_P(NODE) \
  (CLASS_TYPE_P (NODE) && CLASSTYPE_PURE_VIRTUALS(NODE))

/* Nonzero means that this type has an X() constructor.  */
#define TYPE_HAS_DEFAULT_CONSTRUCTOR(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->has_default_ctor)

/* Nonzero means that this type contains a mutable member.  */
#define CLASSTYPE_HAS_MUTABLE(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->has_mutable)
#define TYPE_HAS_MUTABLE_P(NODE) (cp_has_mutable_p (NODE))

/* Nonzero means that this class type is not POD for the purpose of layout
   (as defined in the ABI).  This is different from the language's POD.  */
#define CLASSTYPE_NON_LAYOUT_POD_P(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->non_pod_class)

/* Nonzero means that this class type is a non-standard-layout class.  */
#define CLASSTYPE_NON_STD_LAYOUT(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->non_std_layout)

/* Nonzero means that this class type does have unique object
   representations.  */
#define CLASSTYPE_UNIQUE_OBJ_REPRESENTATIONS(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->unique_obj_representations)

/* Nonzero means that this class type has
   CLASSTYPE_UNIQUE_OBJ_REPRESENTATIONS computed.  */
#define CLASSTYPE_UNIQUE_OBJ_REPRESENTATIONS_SET(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->unique_obj_representations_set)

/* Nonzero means that this class contains pod types whose default
   initialization is not a zero initialization (namely, pointers to
   data members).  */
#define CLASSTYPE_NON_ZERO_INIT_P(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->non_zero_init)

/* Nonzero if this class is "empty" in the sense of the C++ ABI.  */
#define CLASSTYPE_EMPTY_P(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->empty_p)

/* Nonzero if this class is "nearly empty", i.e., contains only a
   virtual function table pointer.  */
#define CLASSTYPE_NEARLY_EMPTY_P(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->nearly_empty_p)

/* Nonzero if this class contains an empty subobject.  */
#define CLASSTYPE_CONTAINS_EMPTY_CLASS_P(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->contains_empty_class_p)

/* A list of class types of which this type is a friend.  The
   TREE_VALUE is normally a TYPE, but will be a TEMPLATE_DECL in the
   case of a template friend.  */
#define CLASSTYPE_FRIEND_CLASSES(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->friend_classes)

/* A list of the classes which grant friendship to this class.  */
#define CLASSTYPE_BEFRIENDING_CLASSES(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->befriending_classes)

/* The associated LAMBDA_EXPR that made this class.  */
#define CLASSTYPE_LAMBDA_EXPR(NODE) \
  (TYPE_POLYMORPHIC_P (NODE)				\
   ? NULL_TREE						\
   : LANG_TYPE_CLASS_CHECK (NODE)->key_method)
#define SET_CLASSTYPE_LAMBDA_EXPR(NODE, VALUE) \
  (gcc_checking_assert (!TYPE_POLYMORPHIC_P (NODE)),	\
   LANG_TYPE_CLASS_CHECK (NODE)->key_method = (VALUE))
/* The extra mangling scope for this closure type.  */
#define LAMBDA_TYPE_EXTRA_SCOPE(NODE) \
  (LAMBDA_EXPR_EXTRA_SCOPE (CLASSTYPE_LAMBDA_EXPR (NODE)))

/* Say whether this node was declared as a "class" or a "struct".  */
#define CLASSTYPE_DECLARED_CLASS(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->declared_class)

/* Nonzero if this class has const members
   which have no specified initialization.  */
#define CLASSTYPE_READONLY_FIELDS_NEED_INIT(NODE)	\
  (TYPE_LANG_SPECIFIC (NODE)				\
   ? LANG_TYPE_CLASS_CHECK (NODE)->const_needs_init : 0)
#define SET_CLASSTYPE_READONLY_FIELDS_NEED_INIT(NODE, VALUE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->const_needs_init = (VALUE))

/* Nonzero if this class has ref members
   which have no specified initialization.  */
#define CLASSTYPE_REF_FIELDS_NEED_INIT(NODE)		\
  (TYPE_LANG_SPECIFIC (NODE)				\
   ? LANG_TYPE_CLASS_CHECK (NODE)->ref_needs_init : 0)
#define SET_CLASSTYPE_REF_FIELDS_NEED_INIT(NODE, VALUE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->ref_needs_init = (VALUE))

/* Nonzero if this class is included from a header file which employs
   `#pragma interface', and it is not included in its implementation file.  */
#define CLASSTYPE_INTERFACE_ONLY(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->interface_only)

/* True if we have already determined whether or not vtables, VTTs,
   typeinfo, and other similar per-class data should be emitted in
   this translation unit.  This flag does not indicate whether or not
   these items should be emitted; it only indicates that we know one
   way or the other.  */
#define CLASSTYPE_INTERFACE_KNOWN(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->interface_unknown == 0)
/* The opposite of CLASSTYPE_INTERFACE_KNOWN.  */
#define CLASSTYPE_INTERFACE_UNKNOWN(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->interface_unknown)

#define SET_CLASSTYPE_INTERFACE_UNKNOWN_X(NODE,X) \
  (LANG_TYPE_CLASS_CHECK (NODE)->interface_unknown = !!(X))
#define SET_CLASSTYPE_INTERFACE_UNKNOWN(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->interface_unknown = 1)
#define SET_CLASSTYPE_INTERFACE_KNOWN(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->interface_unknown = 0)

/* Nonzero if a _DECL node requires us to output debug info for this class.  */
#define CLASSTYPE_DEBUG_REQUESTED(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->debug_requested)

/* True if we saw errors while instantiating this class.  */
#define CLASSTYPE_ERRONEOUS(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->erroneous)

/* True if this class is non-layout-POD only because it was not an aggregate
   before C++14.  If we run out of bits in lang_type, this could be replaced
   with a hash_set only filled in when abi_version_crosses (17).  */
#define CLASSTYPE_NON_POD_AGGREGATE(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->non_pod_aggregate)

/* True if this class is layout-POD though it's not an aggregate in C++20 and
   above (c++/120012).  This could also be a hash_set.  */
#define CLASSTYPE_NON_AGGREGATE_POD(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->non_aggregate_pod)

/* If CLASSTYPE_TRIVIALLY_RELOCATABLE_COMPUTED, true if this class is
   trivially relocatable.
   If !CLASSTYPE_TRIVIALLY_RELOCATABLE_COMPUTED, true if this class
   is marked with trivially_relocatable_if_eligible conditional keyword.  */
#define CLASSTYPE_TRIVIALLY_RELOCATABLE_BIT(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->trivially_relocatable)

/* True if whether this class is trivially relocatable or not
   has been computed already.  */
#define CLASSTYPE_TRIVIALLY_RELOCATABLE_COMPUTED(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->trivially_relocatable_computed)

/* If CLASSTYPE_REPLACEABLE_COMPUTED, true if this class is replaceable.
   If !CLASSTYPE_REPLACEABLE_COMPUTED, true if this class is marked with
   replaceable_if_eligible conditional keyword.  */
#define CLASSTYPE_REPLACEABLE_BIT(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->replaceable)

/* True if whether this class is replaceable or not has been computed
   already.  */
#define CLASSTYPE_REPLACEABLE_COMPUTED(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->replaceable_computed)

/* Additional macros for inheritance information.  */

/* Nonzero means that this class is on a path leading to a new vtable.  */
#define BINFO_VTABLE_PATH_MARKED(NODE) BINFO_FLAG_1 (NODE)

/* Nonzero means B (a BINFO) has its own vtable.  Any copies will not
   have this flag set.  */
#define BINFO_NEW_VTABLE_MARKED(B) (BINFO_FLAG_2 (B))

/* Compare a BINFO_TYPE with another type for equality.  For a binfo,
   this is functionally equivalent to using same_type_p, but
   measurably faster.  At least one of the arguments must be a
   BINFO_TYPE.  The other can be a BINFO_TYPE or a regular type.  If
   BINFO_TYPE(T) ever stops being the main variant of the class the
   binfo is for, this macro must change.  */
#define SAME_BINFO_TYPE_P(A, B) ((A) == (B))

/* Any subobject that needs a new vtable must have a vptr and must not
   be a non-virtual primary base (since it would then use the vtable from a
   derived class and never become non-primary.)  */
#define SET_BINFO_NEW_VTABLE_MARKED(B)					 \
  (BINFO_NEW_VTABLE_MARKED (B) = 1,					 \
   gcc_assert (!BINFO_PRIMARY_P (B) || BINFO_VIRTUAL_P (B)),		 \
   gcc_assert (TYPE_VFIELD (BINFO_TYPE (B))))

/* Nonzero if this binfo is for a dependent base - one that should not
   be searched.  */
#define BINFO_DEPENDENT_BASE_P(NODE) BINFO_FLAG_3 (NODE)

/* Nonzero if this binfo has lost its primary base binfo (because that
   is a nearly-empty virtual base that has been taken by some other
   base in the complete hierarchy.  */
#define BINFO_LOST_PRIMARY_P(NODE) BINFO_FLAG_4 (NODE)

/* Nonzero if this BINFO is a primary base class.  */
#define BINFO_PRIMARY_P(NODE) BINFO_FLAG_5(NODE)

/* A vec<tree_pair_s> of the vcall indices associated with the class
   NODE.  The PURPOSE of each element is a FUNCTION_DECL for a virtual
   function.  The VALUE is the index into the virtual table where the
   vcall offset for that function is stored, when NODE is a virtual
   base.  */
#define CLASSTYPE_VCALL_INDICES(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->vcall_indices)

/* The various vtables for the class NODE.  The primary vtable will be
   first, followed by the construction vtables and VTT, if any.  */
#define CLASSTYPE_VTABLES(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->vtables)

/* The std::type_info variable representing this class, or NULL if no
   such variable has been created.  This field is only set for the
   TYPE_MAIN_VARIANT of the class.  */
#define CLASSTYPE_TYPEINFO_VAR(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->typeinfo_var)

/* Accessor macros for the BINFO_VIRTUALS list.  */

/* The number of bytes by which to adjust the `this' pointer when
   calling this virtual function.  Subtract this value from the this
   pointer. Always non-NULL, might be constant zero though.  */
#define BV_DELTA(NODE) (TREE_PURPOSE (NODE))

/* If non-NULL, the vtable index at which to find the vcall offset
   when calling this virtual function.  Add the value at that vtable
   index to the this pointer.  */
#define BV_VCALL_INDEX(NODE) (TREE_TYPE (NODE))

/* The function to call.  */
#define BV_FN(NODE) (TREE_VALUE (NODE))

/* Whether or not this entry is for a lost primary virtual base.  */
#define BV_LOST_PRIMARY(NODE) (TREE_LANG_FLAG_0 (NODE))

/* For FUNCTION_TYPE or METHOD_TYPE, a list of the exceptions that
   this type can raise.  Each TREE_VALUE is a _TYPE.  The TREE_VALUE
   will be NULL_TREE to indicate a throw specification of `()', or
   no exceptions allowed.  For a noexcept specification, TREE_VALUE
   is NULL_TREE and TREE_PURPOSE is the constant-expression.  For
   a deferred noexcept-specification, TREE_PURPOSE is a DEFERRED_NOEXCEPT
   (for templates) or an OVERLOAD list of functions (for implicitly
   declared functions).  */
#define TYPE_RAISES_EXCEPTIONS(NODE) \
  TYPE_LANG_SLOT_1 (FUNC_OR_METHOD_CHECK (NODE))

/* For FUNCTION_TYPE or METHOD_TYPE, return 1 iff it is declared `throw()'
   or noexcept(true).  */
#define TYPE_NOTHROW_P(NODE) nothrow_spec_p (TYPE_RAISES_EXCEPTIONS (NODE))

/* For FUNCTION_TYPE or METHOD_TYPE, true if NODE is noexcept.  This is the
   case for things declared noexcept(true) and, with -fnothrow-opt, for
   throw() functions.  */
#define TYPE_NOEXCEPT_P(NODE) type_noexcept_p (NODE)

/* The binding level associated with the namespace.  */
#define NAMESPACE_LEVEL(NODE) \
  (LANG_DECL_NS_CHECK (NODE)->level)

/* Discriminator values for lang_decl.  */

enum lang_decl_selector
{
  lds_min,
  lds_fn,
  lds_ns,
  lds_parm,
  lds_decomp
};

/* Flags shared by all forms of DECL_LANG_SPECIFIC.

   Some of the flags live here only to make lang_decl_min/fn smaller.  Do
   not make this struct larger than 32 bits.  */

struct GTY(()) lang_decl_base {
  ENUM_BITFIELD(lang_decl_selector) selector : 3;
  ENUM_BITFIELD(languages) language : 1;
  unsigned use_template : 2;
  unsigned not_really_extern : 1;	   /* var or fn */
  unsigned initialized_in_class : 1;	   /* var or fn */

  unsigned threadprivate_or_deleted_p : 1; /* var or fn */
  /* anticipated_p is no longer used for anticipated_decls (fn, type
     or template).  It is used as DECL_OMP_PRIVATIZED_MEMBER in
     var.  */
  unsigned anticipated_p : 1;
  unsigned friend_or_tls : 1;		   /* var, fn, type or template */
  unsigned unknown_bound_p : 1;		   /* var */
  unsigned odr_used : 1;		   /* var or fn */
  unsigned concept_p : 1;		   /* applies to vars and functions */
  unsigned var_declared_inline_p : 1;	   /* var */
  unsigned dependent_init_p : 1;	   /* var */

  /* The following four apply to VAR, FUNCTION, TYPE, CONCEPT, & NAMESPACE
     decls.  */
  unsigned module_purview_p : 1;	   /* in named-module purview */
  unsigned module_attach_p : 1;		   /* attached to named module */
  unsigned module_import_p : 1;		   /* from an import */
  unsigned module_entity_p : 1;		   /* is in the entitity ary & hash */

  unsigned module_keyed_decls_p : 1;	   /* has keys, applies to all decls */

  /* VAR_DECL being used to represent an OpenMP declared mapper.  */
  unsigned omp_declare_mapper_p : 1;

  /* 10 spare bits.  */
};

/* True for DECL codes which have template info and access.  */
#define LANG_DECL_HAS_MIN(NODE)			\
  (VAR_OR_FUNCTION_DECL_P (NODE)		\
   || TREE_CODE (NODE) == FIELD_DECL		\
   || TREE_CODE (NODE) == CONST_DECL		\
   || TREE_CODE (NODE) == TYPE_DECL		\
   || TREE_CODE (NODE) == TEMPLATE_DECL		\
   || TREE_CODE (NODE) == USING_DECL            \
   || TREE_CODE (NODE) == CONCEPT_DECL)

/* DECL_LANG_SPECIFIC for the above codes.  */

struct GTY(()) lang_decl_min {
  struct lang_decl_base base; /* 32-bits.  */

  /* In a FUNCTION_DECL for which DECL_THUNK_P holds, this is
     THUNK_ALIAS.
     In a FUNCTION_DECL for which DECL_THUNK_P does not hold,
     VAR_DECL, TYPE_DECL, or TEMPLATE_DECL, this is
     DECL_TEMPLATE_INFO.  */
  tree template_info;

  /* In a DECL_THUNK_P FUNCTION_DECL, this is THUNK_VIRTUAL_OFFSET.
     In a lambda-capture proxy VAR_DECL, this is DECL_CAPTURED_VARIABLE.
     In a function-scope TREE_STATIC VAR_DECL or IMPLICIT_TYPEDEF_P TYPE_DECL,
     this is DECL_DISCRIMINATOR.
     In constexpr exception artificial VAR_DECL, this is
     DECL_EXCEPTION_REFCOUNT.
     In a DECL_LOCAL_DECL_P decl, this is the namespace decl it aliases.
     Otherwise, in a class-scope DECL, this is DECL_ACCESS.   */
  tree access;
};

/* Additional DECL_LANG_SPECIFIC information for functions.  */

struct GTY(()) lang_decl_fn {
  struct lang_decl_min min;

  /* In a overloaded operator, this is the compressed operator code.  */
  unsigned ovl_op_code : 6;
  unsigned global_ctor_p : 1;
  unsigned global_dtor_p : 1;

  unsigned static_function : 1;
  unsigned pure_virtual : 1;
  unsigned defaulted_p : 1;
  unsigned has_in_charge_parm_p : 1;
  unsigned has_vtt_parm_p : 1;
  unsigned pending_inline_p : 1;
  unsigned nonconverting : 1;
  unsigned thunk_p : 1;

  unsigned this_thunk_p : 1;
  unsigned omp_declare_reduction_p : 1;
  unsigned has_dependent_explicit_spec_p : 1;
  unsigned immediate_fn_p : 1;
  unsigned maybe_deleted : 1;
  unsigned coroutine_p : 1;
  unsigned implicit_constexpr : 1;
  unsigned escalated_p : 1;
  unsigned xobj_func : 1;

  unsigned spare : 7;

  /* 32-bits padding on 64-bit host.  */

  /* For a non-thunk function decl, this is a tree list of
     friendly classes. For a thunk function decl, it is the
     thunked to function decl.  */
  tree befriending_classes;

  /* For a virtual FUNCTION_DECL for which
     DECL_THIS_THUNK_P does not hold, this is DECL_THUNKS. Both
     this pointer and result pointer adjusting thunks are
     chained here.  This pointer thunks to return pointer thunks
     will be chained on the return pointer thunk.
     For a DECL_CONSTRUCTOR_P or deduction_guide_p FUNCTION_DECL,
     this is the base from whence we inherit.
     Otherwise, it is the class in which a (namespace-scope) friend
     is defined (if any).  */
  tree context;

  union lang_decl_u5
  {
    /* In a non-thunk FUNCTION_DECL, this is DECL_CLONED_FUNCTION.  */
    tree GTY ((tag ("0"))) cloned_function;

    /* In a FUNCTION_DECL for which THUNK_P holds this is the
       THUNK_FIXED_OFFSET.  */
    HOST_WIDE_INT GTY ((tag ("1"))) fixed_offset;
  } GTY ((desc ("%1.thunk_p"))) u5;

  union lang_decl_u3
  {
    struct cp_token_cache * GTY ((tag ("1"))) pending_inline_info;
    tree GTY ((tag ("0"))) saved_auto_return_type;
  } GTY ((desc ("%1.pending_inline_p"))) u;

};

/* DECL_LANG_SPECIFIC for namespaces.  */

struct GTY(()) lang_decl_ns {
  struct lang_decl_base base; /* 32 bits.  */
  cp_binding_level *level;

  /* Inline children.  Needs to be va_gc, because of PCH.  */
  vec<tree, va_gc> *inlinees;

  /* Hash table of bound decls. It'd be nice to have this inline, but
     as the hash_map has a dtor, we can't then put this struct into a
     union (until moving to c++11).  */
  hash_table<named_decl_hash> *bindings;
};

/* DECL_LANG_SPECIFIC for parameters.  */

struct GTY(()) lang_decl_parm {
  struct lang_decl_base base; /* 32 bits.  */
  int level;
  int index;
};

/* Additional DECL_LANG_SPECIFIC information for structured bindings.  */

struct GTY(()) lang_decl_decomp {
  struct lang_decl_min min;
  /* The artificial underlying "e" variable of the structured binding
     variable.  */
  tree base;
};

/* DECL_LANG_SPECIFIC for all types.  It would be nice to just make this a
   union rather than a struct containing a union as its only field, but
   tree.h declares it as a struct.  */

struct GTY(()) lang_decl {
  union GTY((desc ("%h.base.selector"))) lang_decl_u {
     /* Nothing of only the base type exists.  */
    struct lang_decl_base GTY ((default)) base;
    struct lang_decl_min GTY((tag ("lds_min"))) min;
    struct lang_decl_fn GTY ((tag ("lds_fn"))) fn;
    struct lang_decl_ns GTY((tag ("lds_ns"))) ns;
    struct lang_decl_parm GTY((tag ("lds_parm"))) parm;
    struct lang_decl_decomp GTY((tag ("lds_decomp"))) decomp;
  } u;
};

/* Looks through a template (if present) to find what it declares.  */
#define STRIP_TEMPLATE(NODE) \
  (TREE_CODE (NODE) == TEMPLATE_DECL ? DECL_TEMPLATE_RESULT (NODE) : NODE)

#if defined ENABLE_TREE_CHECKING && (GCC_VERSION >= 2007)

#define LANG_DECL_MIN_CHECK(NODE) __extension__			\
({ struct lang_decl *lt = DECL_LANG_SPECIFIC (NODE);		\
   if (!LANG_DECL_HAS_MIN (NODE))				\
     lang_check_failed (__FILE__, __LINE__, __FUNCTION__);	\
   &lt->u.min; })

/* We want to be able to check DECL_CONSTRUCTOR_P and such on a function
   template, not just on a FUNCTION_DECL.  So when looking for things in
   lang_decl_fn, look down through a TEMPLATE_DECL into its result.  */
#define LANG_DECL_FN_CHECK(NODE) __extension__				\
({ struct lang_decl *lt = DECL_LANG_SPECIFIC (STRIP_TEMPLATE (NODE));	\
   if (!DECL_DECLARES_FUNCTION_P (NODE)					\
       || lt->u.base.selector != lds_fn)				\
     lang_check_failed (__FILE__, __LINE__, __FUNCTION__);		\
   &lt->u.fn; })

#define LANG_DECL_NS_CHECK(NODE) __extension__				\
({ struct lang_decl *lt = DECL_LANG_SPECIFIC (NODE);			\
   if (TREE_CODE (NODE) != NAMESPACE_DECL				\
       || lt->u.base.selector != lds_ns)				\
     lang_check_failed (__FILE__, __LINE__, __FUNCTION__);		\
   &lt->u.ns; })

#define LANG_DECL_PARM_CHECK(NODE) __extension__		\
({ struct lang_decl *lt = DECL_LANG_SPECIFIC (NODE);		\
  if (TREE_CODE (NODE) != PARM_DECL				\
      || lt->u.base.selector != lds_parm)			\
    lang_check_failed (__FILE__, __LINE__, __FUNCTION__);	\
  &lt->u.parm; })

#define LANG_DECL_DECOMP_CHECK(NODE) __extension__		\
({ struct lang_decl *lt = DECL_LANG_SPECIFIC (NODE);		\
  if (!VAR_P (NODE)						\
      || lt->u.base.selector != lds_decomp)			\
    lang_check_failed (__FILE__, __LINE__, __FUNCTION__);	\
  &lt->u.decomp; })

#else

#define LANG_DECL_MIN_CHECK(NODE) \
  (&DECL_LANG_SPECIFIC (NODE)->u.min)

#define LANG_DECL_FN_CHECK(NODE) \
  (&DECL_LANG_SPECIFIC (STRIP_TEMPLATE (NODE))->u.fn)

#define LANG_DECL_NS_CHECK(NODE) \
  (&DECL_LANG_SPECIFIC (NODE)->u.ns)

#define LANG_DECL_PARM_CHECK(NODE) \
  (&DECL_LANG_SPECIFIC (NODE)->u.parm)

#define LANG_DECL_DECOMP_CHECK(NODE) \
  (&DECL_LANG_SPECIFIC (NODE)->u.decomp)

#endif /* ENABLE_TREE_CHECKING */

/* For a FUNCTION_DECL or a VAR_DECL, the language linkage for the
   declaration.  Some entities (like a member function in a local
   class, or a local variable) do not have linkage at all, and this
   macro should not be used in those cases.

   Implementation note: A FUNCTION_DECL without DECL_LANG_SPECIFIC was
   created by language-independent code, and has C linkage.  Most
   VAR_DECLs have C++ linkage, and do not have DECL_LANG_SPECIFIC, but
   we do create DECL_LANG_SPECIFIC for variables with non-C++ linkage.  */
#define DECL_LANGUAGE(NODE)				\
  (DECL_LANG_SPECIFIC (NODE)				\
   ? DECL_LANG_SPECIFIC (NODE)->u.base.language		\
   : (TREE_CODE (NODE) == FUNCTION_DECL			\
      ? lang_c : lang_cplusplus))

/* Set the language linkage for NODE to LANGUAGE.  */
#define SET_DECL_LANGUAGE(NODE, LANGUAGE) \
  (DECL_LANG_SPECIFIC (NODE)->u.base.language = (LANGUAGE))

/* For FUNCTION_DECLs and TEMPLATE_DECLs: nonzero means that this function
   is a constructor.  */
#define DECL_CONSTRUCTOR_P(NODE) \
  DECL_CXX_CONSTRUCTOR_P (STRIP_TEMPLATE (NODE))

/* Nonzero if NODE (a FUNCTION_DECL) is a constructor for a complete
   object.  */
#define DECL_COMPLETE_CONSTRUCTOR_P(NODE)		\
  (DECL_NAME (NODE) == complete_ctor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a constructor for a base
   object.  */
#define DECL_BASE_CONSTRUCTOR_P(NODE)		\
  (DECL_NAME (NODE) == base_ctor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a constructor, but not either the
   specialized in-charge constructor or the specialized not-in-charge
   constructor.  */
#define DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P(NODE)		\
  (DECL_NAME (NODE) == ctor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a copy constructor.  */
#define DECL_COPY_CONSTRUCTOR_P(NODE) \
  (DECL_CONSTRUCTOR_P (NODE) && copy_fn_p (NODE) > 0)

/* Nonzero if NODE (a FUNCTION_DECL) is a move constructor.  */
#define DECL_MOVE_CONSTRUCTOR_P(NODE) \
  (DECL_CONSTRUCTOR_P (NODE) && move_fn_p (NODE))

/* Nonzero if NODE (a FUNCTION_DECL or TEMPLATE_DECL)
   is a destructor.  */
#define DECL_DESTRUCTOR_P(NODE)				\
  DECL_CXX_DESTRUCTOR_P (STRIP_TEMPLATE (NODE))

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor, but not the
   specialized in-charge constructor, in-charge deleting constructor,
   or the base destructor.  */
#define DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P(NODE)			\
  (DECL_NAME (NODE) == dtor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor for a complete
   object.  */
#define DECL_COMPLETE_DESTRUCTOR_P(NODE)		\
  (DECL_NAME (NODE) == complete_dtor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor for a base
   object.  */
#define DECL_BASE_DESTRUCTOR_P(NODE)		\
  (DECL_NAME (NODE) == base_dtor_identifier)

/* Nonzero if NODE (a FUNCTION_DECL) is a destructor for a complete
   object that deletes the object after it has been destroyed.  */
#define DECL_DELETING_DESTRUCTOR_P(NODE)		\
  (DECL_NAME (NODE) == deleting_dtor_identifier)

/* Nonzero if either DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P or
   DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P is true of NODE.  */
#define DECL_MAYBE_IN_CHARGE_CDTOR_P(NODE)              \
  (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (NODE)            \
   || DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (NODE))

/* Nonzero if NODE (a _DECL) is a cloned constructor or
   destructor.  */
#define DECL_CLONED_FUNCTION_P(NODE)		\
  (DECL_NAME (NODE)				\
   && IDENTIFIER_CDTOR_P (DECL_NAME (NODE))	\
   && !DECL_MAYBE_IN_CHARGE_CDTOR_P (NODE))

/* If DECL_CLONED_FUNCTION_P holds, this is the function that was
   cloned.  */
#define DECL_CLONED_FUNCTION(NODE)		\
  (DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (NODE))->u.fn.u5.cloned_function)

/* Perform an action for each clone of FN, if FN is a function with
   clones.  This macro should be used like:

      FOR_EACH_CLONE (clone, fn)
	{ ... }

  */
#define FOR_EACH_CLONE(CLONE, FN)			\
  if (!(TREE_CODE (FN) == FUNCTION_DECL			\
	&& DECL_MAYBE_IN_CHARGE_CDTOR_P (FN)))          \
    ;							\
  else							\
    for (CLONE = DECL_CHAIN (FN);			\
	 CLONE && DECL_CLONED_FUNCTION_P (CLONE);	\
	 CLONE = DECL_CHAIN (CLONE))

/* Nonzero if NODE has DECL_DISCRIMINATOR and not DECL_ACCESS.  */
#define DECL_DISCRIMINATOR_P(NODE)				\
  (((VAR_P (NODE) && TREE_STATIC (NODE))	\
    || DECL_IMPLICIT_TYPEDEF_P (NODE))				\
   && DECL_FUNCTION_SCOPE_P (NODE))

/* Discriminator for name mangling.  */
#define DECL_DISCRIMINATOR(NODE) (LANG_DECL_MIN_CHECK (NODE)->access)

/* The index of a user-declared parameter in its function, starting at 1.
   All artificial parameters will have index 0.  */
#define DECL_PARM_INDEX(NODE) \
  (LANG_DECL_PARM_CHECK (NODE)->index)

/* The level of a user-declared parameter in its function, starting at 1.
   A parameter of the function will have level 1; a parameter of the first
   nested function declarator (i.e. t in void f (void (*p)(T t))) will have
   level 2.  */
#define DECL_PARM_LEVEL(NODE) \
  (LANG_DECL_PARM_CHECK (NODE)->level)

/* Nonzero if the VTT parm has been added to NODE.  */
#define DECL_HAS_VTT_PARM_P(NODE) \
  (LANG_DECL_FN_CHECK (NODE)->has_vtt_parm_p)

/* Nonzero if NODE is a user-defined conversion operator.  */
#define DECL_CONV_FN_P(NODE) IDENTIFIER_CONV_OP_P (DECL_NAME (NODE))

/* The type to which conversion operator FN converts to.   */
#define DECL_CONV_FN_TYPE(FN) \
  TREE_TYPE ((gcc_checking_assert (DECL_CONV_FN_P (FN)), DECL_NAME (FN)))

/* Nonzero if NODE, a templated variable, was declared as an
   array of unknown bound.  */
#define VAR_HAD_UNKNOWN_BOUND(NODE)			\
  (DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE))		\
   ? DECL_LANG_SPECIFIC (NODE)->u.base.unknown_bound_p	\
   : false)
#define SET_VAR_HAD_UNKNOWN_BOUND(NODE) \
  (DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE))->u.base.unknown_bound_p = true)

/* True iff decl NODE is for an overloaded operator.  */
#define DECL_OVERLOADED_OPERATOR_P(NODE)		\
  IDENTIFIER_ANY_OP_P (DECL_NAME (NODE))

/* Nonzero if NODE is an assignment operator (including += and such).  */
#define DECL_ASSIGNMENT_OPERATOR_P(NODE)		 \
  IDENTIFIER_ASSIGN_OP_P (DECL_NAME (NODE))

/* NODE is a function_decl for an overloaded operator.  Return its
   compressed (raw) operator code.  Note that this is not a TREE_CODE.  */
#define DECL_OVERLOADED_OPERATOR_CODE_RAW(NODE)		\
  (LANG_DECL_FN_CHECK (NODE)->ovl_op_code)

/* DECL is an overloaded operator.  Test whether it is for TREE_CODE
   (a literal constant).  */
#define DECL_OVERLOADED_OPERATOR_IS(DECL, CODE)			\
  (DECL_OVERLOADED_OPERATOR_CODE_RAW (DECL) == OVL_OP_##CODE)

/* For FUNCTION_DECLs: nonzero means that this function is a
   constructor or a destructor with an extra in-charge parameter to
   control whether or not virtual bases are constructed.  */
#define DECL_HAS_IN_CHARGE_PARM_P(NODE) \
  (LANG_DECL_FN_CHECK (NODE)->has_in_charge_parm_p)

/* Nonzero if DECL is a declaration of __builtin_constant_p.  */
#define DECL_IS_BUILTIN_CONSTANT_P(NODE)		\
 (TREE_CODE (NODE) == FUNCTION_DECL			\
  && DECL_BUILT_IN_CLASS (NODE) == BUILT_IN_NORMAL	\
  && DECL_FUNCTION_CODE (NODE) == BUILT_IN_CONSTANT_P)

/* Nonzero for _DECL means that this decl appears in (or will appear
   in) as a member in a RECORD_TYPE or UNION_TYPE node.  It is also for
   detecting circularity in case members are multiply defined.  In the
   case of a VAR_DECL, it means that no definition has been seen, even
   if an initializer has been.  */
#define DECL_IN_AGGR_P(NODE) (DECL_LANG_FLAG_3 (NODE))

/* Nonzero for a VAR_DECL means that the variable's initialization (if
   any) has been processed.  (In general, DECL_INITIALIZED_P is
   !DECL_EXTERNAL, but static data members may be initialized even if
   not defined.)  */
#define DECL_INITIALIZED_P(NODE) \
   (TREE_LANG_FLAG_1 (VAR_DECL_CHECK (NODE)))

/* Nonzero for a VAR_DECL iff an explicit initializer was provided
   or a non-trivial constructor is called.  */
#define DECL_NONTRIVIALLY_INITIALIZED_P(NODE)	\
   (TREE_LANG_FLAG_6 (VAR_DECL_CHECK (NODE)))

/* Nonzero for a VAR_DECL that was initialized with a
   constant-expression.  */
#define DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P(NODE) \
  (TREE_LANG_FLAG_2 (VAR_DECL_CHECK (NODE)))

/* Nonzero if the DECL was initialized in the class definition itself,
   rather than outside the class.  This is used for both static member
   VAR_DECLS, and FUNCTION_DECLS that are defined in the class.  */
#define DECL_INITIALIZED_IN_CLASS_P(DECL) \
  (DECL_LANG_SPECIFIC (VAR_OR_FUNCTION_DECL_CHECK (DECL)) \
   ->u.base.initialized_in_class)

/* Nonzero if the DECL is used in the sense of 3.2 [basic.def.odr].
   Only available for decls with DECL_LANG_SPECIFIC.  */
#define DECL_ODR_USED(DECL) \
  (DECL_LANG_SPECIFIC (VAR_OR_FUNCTION_DECL_CHECK (DECL)) \
   ->u.base.odr_used)

/* Nonzero for FUNCTION_DECL means that this is a friend that is
   either not pushed into a namespace/looked up in a class (because it
   is a dependent type, in an uninstantiated template), or it has
   /only/ been subject to hidden friend injection from one or more
   befriending classes.  Once another decl matches, the flag is
   cleared.  There are requirements on its default parms.  */
#define DECL_UNIQUE_FRIEND_P(NODE) \
  (DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (NODE)) \
   ->u.base.friend_or_tls)

/* True of a TEMPLATE_DECL that is a template class friend.  Such
   decls are not pushed until instantiated (as they may depend on
   parameters of the befriending class).  DECL_CHAIN is the
   befriending class.  */
#define DECL_UNINSTANTIATED_TEMPLATE_FRIEND_P(NODE) \
  (DECL_LANG_FLAG_4 (TEMPLATE_DECL_CHECK (NODE)))

/* Nonzero if the thread-local variable was declared with __thread as
   opposed to thread_local.  */
#define DECL_GNU_TLS_P(NODE)				\
  (DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE))		\
   && DECL_LANG_SPECIFIC (NODE)->u.base.friend_or_tls)
#define SET_DECL_GNU_TLS_P(NODE)				\
  (retrofit_lang_decl (VAR_DECL_CHECK (NODE)),			\
   DECL_LANG_SPECIFIC (NODE)->u.base.friend_or_tls = true)

/* A TREE_LIST of the types which have befriended this FUNCTION_DECL.  */
#define DECL_BEFRIENDING_CLASSES(NODE) \
  (LANG_DECL_FN_CHECK (NODE)->befriending_classes)

/* Nonzero for FUNCTION_DECL means that this decl is a static
   member function.  */
#define DECL_STATIC_FUNCTION_P(NODE) \
  (LANG_DECL_FN_CHECK (NODE)->static_function)

/* Nonzero for FUNCTION_DECL means that this decl is a non-static member
   function.  C++23 explicit object member functions are also considered
   non-static, but most former uses of this macro meant implicit object member
   function.  Instead of this macro, use DECL_IOBJ_MEMBER_FUNCTION_P or
   DECL_OBJECT_MEMBER_FUNCTION_P.  */
#define DECL_NONSTATIC_MEMBER_FUNCTION_P(NODE) did_you_mean_object_or_iobj

/* Nonzero for FUNCTION_DECL means that this decl is an implicit object
   member function.  */
#define DECL_IOBJ_MEMBER_FUNCTION_P(NODE) \
  (TREE_CODE (TREE_TYPE (NODE)) == METHOD_TYPE)

/* Simple member access, only valid for FUNCTION_DECL nodes.  */
#define DECL_FUNCTION_XOBJ_FLAG(NODE)	\
  (LANG_DECL_FN_CHECK (NODE)->xobj_func)

/* Nonzero if NODE is an xobj member function,
   safely evaluates to false for all non FUNCTION_DECL nodes.  */
#define DECL_XOBJ_MEMBER_FUNCTION_P(NODE)		\
  (TREE_CODE (STRIP_TEMPLATE (NODE)) == FUNCTION_DECL	\
   && DECL_FUNCTION_XOBJ_FLAG (NODE) == 1)

/* Nonzero if NODE is a member function with an object argument,
   in other words, a non-static member function.  */
#define DECL_OBJECT_MEMBER_FUNCTION_P(NODE) \
  (DECL_IOBJ_MEMBER_FUNCTION_P (NODE) || DECL_XOBJ_MEMBER_FUNCTION_P (NODE))

/* Nonzero for FUNCTION_DECL means that this decl is a member function
   (static or non-static).  */
#define DECL_FUNCTION_MEMBER_P(NODE) \
  (DECL_OBJECT_MEMBER_FUNCTION_P (NODE) || DECL_STATIC_FUNCTION_P (NODE)) \

/* Nonzero for FUNCTION_DECL means that this member function
   has `this' as const X *const.  */
#define DECL_CONST_MEMFUNC_P(NODE)					 \
  (DECL_IOBJ_MEMBER_FUNCTION_P (NODE)				 \
   && CP_TYPE_CONST_P (TREE_TYPE (TREE_VALUE				 \
				  (TYPE_ARG_TYPES (TREE_TYPE (NODE))))))

/* Nonzero for FUNCTION_DECL means that this member function
   has `this' as volatile X *const.  */
#define DECL_VOLATILE_MEMFUNC_P(NODE)					 \
  (DECL_IOBJ_MEMBER_FUNCTION_P (NODE)				 \
   && CP_TYPE_VOLATILE_P (TREE_TYPE (TREE_VALUE				 \
				  (TYPE_ARG_TYPES (TREE_TYPE (NODE))))))

/* Nonzero for a DECL means that this member is a non-static member.  */
#define DECL_NONSTATIC_MEMBER_P(NODE)		\
  (DECL_OBJECT_MEMBER_FUNCTION_P (NODE)	\
   || TREE_CODE (NODE) == FIELD_DECL)

/* Nonzero for a FIELD_DECL means that this member object type
   is mutable.  */
#define DECL_MUTABLE_P(NODE) (DECL_LANG_FLAG_0 (FIELD_DECL_CHECK (NODE)))

/* Nonzero for _DECL means that this constructor or conversion function is
   non-converting.  */
#define DECL_NONCONVERTING_P(NODE) \
  (LANG_DECL_FN_CHECK (NODE)->nonconverting)

/* Nonzero for FUNCTION_DECL means that this member function is a pure
   virtual function.  */
#define DECL_PURE_VIRTUAL_P(NODE) \
  (LANG_DECL_FN_CHECK (NODE)->pure_virtual)

/* Nonzero for FUNCTION_DECL means that this member function (either
   a constructor or a conversion function) has an explicit specifier
   with a value-dependent expression.  */
#define DECL_HAS_DEPENDENT_EXPLICIT_SPEC_P(NODE) \
  (LANG_DECL_FN_CHECK (NODE)->has_dependent_explicit_spec_p)

/* Nonzero for a defaulted FUNCTION_DECL for which we haven't decided yet if
   it's deleted; we will decide in synthesize_method.  */
#define DECL_MAYBE_DELETED(NODE) \
  (LANG_DECL_FN_CHECK (NODE)->maybe_deleted)

/* Nonzero for FUNCTION_DECL means that this function's body has been
   checked for immediate-escalating expressions and maybe promoted.  It
   does *not* mean the function is consteval.  It must not be set in
   a function that was marked consteval by the user, so that we can
   distinguish between explicitly consteval functions and promoted consteval
   functions.  */
#define DECL_ESCALATION_CHECKED_P(NODE) (LANG_DECL_FN_CHECK (NODE)->escalated_p)

/* True (in a FUNCTION_DECL) if NODE is a virtual function that is an
   invalid overrider for a function from a base class.  Once we have
   complained about an invalid overrider we avoid complaining about it
   again.  */
#define DECL_INVALID_OVERRIDER_P(NODE) \
  (DECL_LANG_FLAG_4 (NODE))

/* True (in a FUNCTION_DECL) if NODE is a function declared with
   an override virt-specifier */
#define DECL_OVERRIDE_P(NODE) (TREE_LANG_FLAG_0 (NODE))

/* The thunks associated with NODE, a FUNCTION_DECL.  */
#define DECL_THUNKS(NODE) \
  (DECL_VIRTUAL_P (NODE) ? LANG_DECL_FN_CHECK (NODE)->context : NULL_TREE)

/* Set DECL_THUNKS.  */
#define SET_DECL_THUNKS(NODE,THUNKS) \
  (LANG_DECL_FN_CHECK (NODE)->context = (THUNKS))

/* If NODE, a FUNCTION_DECL, is a C++11 inheriting constructor, then this
   is the constructor it inherits from.  */
#define DECL_INHERITED_CTOR(NODE) \
  (DECL_DECLARES_FUNCTION_P (NODE) && DECL_CONSTRUCTOR_P (NODE) \
   ? LANG_DECL_FN_CHECK (NODE)->context : NULL_TREE)

/* And this is the base that constructor comes from.  */
#define DECL_INHERITED_CTOR_BASE(NODE)			\
  (DECL_INHERITED_CTOR (NODE)				\
   ? DECL_CONTEXT (flag_new_inheriting_ctors		\
		   ? strip_inheriting_ctors (NODE)	\
		   : DECL_INHERITED_CTOR (NODE))	\
   : NULL_TREE)

/* Set the inherited base.  */
#define SET_DECL_INHERITED_CTOR(NODE,INH) \
  (LANG_DECL_FN_CHECK (NODE)->context = (INH))

/* Nonzero if NODE is a thunk, rather than an ordinary function.  */
#define DECL_THUNK_P(NODE)			\
  (TREE_CODE (NODE) == FUNCTION_DECL		\
   && DECL_LANG_SPECIFIC (NODE)			\
   && LANG_DECL_FN_CHECK (NODE)->thunk_p)

/* Set DECL_THUNK_P for node.  */
#define SET_DECL_THUNK_P(NODE, THIS_ADJUSTING)			\
  (LANG_DECL_FN_CHECK (NODE)->thunk_p = 1,			\
   LANG_DECL_FN_CHECK (NODE)->this_thunk_p = (THIS_ADJUSTING))

/* Nonzero if NODE is a this pointer adjusting thunk.  */
#define DECL_THIS_THUNK_P(NODE)			\
  (DECL_THUNK_P (NODE) && LANG_DECL_FN_CHECK (NODE)->this_thunk_p)

/* Nonzero if NODE is a result pointer adjusting thunk.  */
#define DECL_RESULT_THUNK_P(NODE)			\
  (DECL_THUNK_P (NODE) && !LANG_DECL_FN_CHECK (NODE)->this_thunk_p)

/* Nonzero if NODE is a FUNCTION_DECL, but not a thunk.  */
#define DECL_NON_THUNK_FUNCTION_P(NODE)				\
  (TREE_CODE (NODE) == FUNCTION_DECL && !DECL_THUNK_P (NODE))

/* Nonzero if NODE is `extern "C"'.  */
#define DECL_EXTERN_C_P(NODE) \
  (DECL_LANGUAGE (NODE) == lang_c)

/* Nonzero if NODE is an `extern "C"' function.  */
#define DECL_EXTERN_C_FUNCTION_P(NODE) \
  (DECL_NON_THUNK_FUNCTION_P (NODE) && DECL_EXTERN_C_P (NODE))

/* Non-zero if this variable is declared `constinit' specifier.  */
#define DECL_DECLARED_CONSTINIT_P(NODE)		\
  (DECL_LANG_FLAG_7 (VAR_DECL_CHECK (NODE)))

/* True if DECL is declared 'constexpr'.  */
#define DECL_DECLARED_CONSTEXPR_P(DECL) \
  DECL_LANG_FLAG_8 (VAR_OR_FUNCTION_DECL_CHECK (STRIP_TEMPLATE (DECL)))

/* True if FNDECL is an immediate function.  */
#define DECL_IMMEDIATE_FUNCTION_P(NODE) \
  (DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (STRIP_TEMPLATE (NODE)))	\
   ? LANG_DECL_FN_CHECK (NODE)->immediate_fn_p				\
   : false)
#define SET_DECL_IMMEDIATE_FUNCTION_P(NODE) \
  (retrofit_lang_decl (FUNCTION_DECL_CHECK (NODE)),			\
   LANG_DECL_FN_CHECK (NODE)->immediate_fn_p = true)

/* Nonzero if this DECL is the __PRETTY_FUNCTION__ variable in a
   template function.  */
#define DECL_PRETTY_FUNCTION_P(NODE) \
  (DECL_NAME (NODE) \
   && id_equal (DECL_NAME (NODE), "__PRETTY_FUNCTION__"))

/* For a DECL, true if it is __func__ or similar.  */
#define DECL_FNAME_P(NODE)					\
  (VAR_P (NODE) && DECL_NAME (NODE) && DECL_ARTIFICIAL (NODE)	\
   && DECL_HAS_VALUE_EXPR_P (NODE)				\
   && (id_equal (DECL_NAME (NODE), "__PRETTY_FUNCTION__")	\
       || id_equal (DECL_NAME (NODE), "__FUNCTION__")		\
       || id_equal (DECL_NAME (NODE), "__func__")))

/* Nonzero if the variable was declared to be thread-local.
   We need a special C++ version of this test because the middle-end
   DECL_THREAD_LOCAL_P uses the symtab, so we can't use it for
   templates.  */
#define CP_DECL_THREAD_LOCAL_P(NODE) \
  (TREE_LANG_FLAG_0 (VAR_DECL_CHECK (NODE)))

/* The _TYPE context in which this _DECL appears.  This field holds the
   class where a virtual function instance is actually defined.  */
#define DECL_CLASS_CONTEXT(NODE) \
  (DECL_CLASS_SCOPE_P (NODE) ? DECL_CONTEXT (NODE) : NULL_TREE)

/* For a non-member friend function, the class (if any) in which this
   friend was defined.  For example, given:

     struct S { friend void f () { ... } };

   the DECL_FRIEND_CONTEXT for `f' will be `S'.  */
#define DECL_FRIEND_CONTEXT(NODE)				\
  ((DECL_DECLARES_FUNCTION_P (NODE) && !DECL_VIRTUAL_P (NODE)	\
    && !DECL_CONSTRUCTOR_P (NODE)				\
    && (cxx_dialect < cxx23 || !deduction_guide_p (NODE)))	\
   ? LANG_DECL_FN_CHECK (NODE)->context				\
   : NULL_TREE)

/* Set the DECL_FRIEND_CONTEXT for NODE to CONTEXT.  */
#define SET_DECL_FRIEND_CONTEXT(NODE, CONTEXT) \
  (LANG_DECL_FN_CHECK (NODE)->context = (CONTEXT))

#define CP_DECL_CONTEXT(NODE) \
  (!DECL_FILE_SCOPE_P (NODE) ? DECL_CONTEXT (NODE) : global_namespace)
#define CP_TYPE_CONTEXT(NODE) \
  (!TYPE_FILE_SCOPE_P (NODE) ? TYPE_CONTEXT (NODE) : global_namespace)
#define FROB_CONTEXT(NODE) \
  ((NODE) == global_namespace ? DECL_CONTEXT (NODE) : (NODE))

/* 1 iff NODE has namespace scope, including the global namespace.  */
#define DECL_NAMESPACE_SCOPE_P(NODE)				\
  (!DECL_TEMPLATE_PARM_P (NODE)					\
   && TREE_CODE (CP_DECL_CONTEXT (NODE)) == NAMESPACE_DECL)

#define TYPE_NAMESPACE_SCOPE_P(NODE) \
  (TREE_CODE (CP_TYPE_CONTEXT (NODE)) == NAMESPACE_DECL)

#define NAMESPACE_SCOPE_P(NODE) \
  ((DECL_P (NODE) && DECL_NAMESPACE_SCOPE_P (NODE)) \
   || (TYPE_P (NODE) && TYPE_NAMESPACE_SCOPE_P (NODE)))

/* 1 iff NODE is a class member.  */
#define DECL_CLASS_SCOPE_P(NODE) \
  (DECL_CONTEXT (NODE) && TYPE_P (DECL_CONTEXT (NODE)))

#define TYPE_CLASS_SCOPE_P(NODE) \
  (TYPE_CONTEXT (NODE) && TYPE_P (TYPE_CONTEXT (NODE)))

/* 1 iff NODE is function-local.  */
#define DECL_FUNCTION_SCOPE_P(NODE) \
  (DECL_CONTEXT (NODE) \
   && TREE_CODE (DECL_CONTEXT (NODE)) == FUNCTION_DECL)

#define TYPE_FUNCTION_SCOPE_P(NODE) \
  (TYPE_CONTEXT (NODE) && TREE_CODE (TYPE_CONTEXT (NODE)) == FUNCTION_DECL)

/* 1 iff VAR_DECL node NODE is a type-info decl.  This flag is set for
   both the primary typeinfo object and the associated NTBS name.  */
#define DECL_TINFO_P(NODE)			\
  TREE_LANG_FLAG_4 (TREE_CHECK2 (NODE,VAR_DECL,TYPE_DECL))

/* true iff VAR_DECL node NODE is a NTTP object decl.  */
#define DECL_NTTP_OBJECT_P(NODE)			\
  TREE_LANG_FLAG_5 (TREE_CHECK (NODE,VAR_DECL))

/* 1 iff VAR_DECL node NODE is virtual table or VTT.  We forward to
   DECL_VIRTUAL_P from the common code, as that has the semantics we
   need.  But we want a more descriptive name.  */
#define DECL_VTABLE_OR_VTT_P(NODE) DECL_VIRTUAL_P (VAR_DECL_CHECK (NODE))

/* 1 iff a _DECL for a template parameter came from
   synthesize_implicit_template_parm.  */
#define DECL_IMPLICIT_TEMPLATE_PARM_P(NODE) \
  DECL_VIRTUAL_P (DECL_TEMPLATE_PARM_CHECK (NODE))

/* 1 iff FUNCTION_TYPE or METHOD_TYPE has a ref-qualifier (either & or &&).  */
#define FUNCTION_REF_QUALIFIED(NODE) \
  TREE_LANG_FLAG_4 (FUNC_OR_METHOD_CHECK (NODE))

/* 1 iff FUNCTION_TYPE or METHOD_TYPE has &&-ref-qualifier.  */
#define FUNCTION_RVALUE_QUALIFIED(NODE) \
  TREE_LANG_FLAG_5 (FUNC_OR_METHOD_CHECK (NODE))

/* 1 iff NODE is function-local, but for types.  */
#define LOCAL_CLASS_P(NODE)				\
  (decl_function_context (TYPE_MAIN_DECL (NODE)) != NULL_TREE)

/* The nesting depth of namespace, class or function.  Makes is_ancestor much
   simpler.  Only 8 bits available.  */
#define SCOPE_DEPTH(NODE) \
  (NAMESPACE_DECL_CHECK (NODE)->base.u.bits.address_space)

/* Whether the namepace is an inline namespace.  */
#define DECL_NAMESPACE_INLINE_P(NODE) \
  TREE_LANG_FLAG_0 (NAMESPACE_DECL_CHECK (NODE))

/* In a NAMESPACE_DECL, a vector of inline namespaces.  */
#define DECL_NAMESPACE_INLINEES(NODE) \
   (LANG_DECL_NS_CHECK (NODE)->inlinees)

/* Pointer to hash_map from IDENTIFIERS to DECLS  */
#define DECL_NAMESPACE_BINDINGS(NODE) \
   (LANG_DECL_NS_CHECK (NODE)->bindings)

/* In a NAMESPACE_DECL, points to the original namespace if this is
   a namespace alias.  */
#define DECL_NAMESPACE_ALIAS(NODE) \
	DECL_ABSTRACT_ORIGIN (NAMESPACE_DECL_CHECK (NODE))
#define ORIGINAL_NAMESPACE(NODE)  \
  (DECL_NAMESPACE_ALIAS (NODE) ? DECL_NAMESPACE_ALIAS (NODE) : (NODE))

/* Nonzero if NODE is the std namespace.  */
#define DECL_NAMESPACE_STD_P(NODE)			\
  ((NODE) == std_node)

/* In a TREE_LIST in an attribute list, indicates that the attribute
   must be applied at instantiation time.  */
#define ATTR_IS_DEPENDENT(NODE) TREE_LANG_FLAG_0 (TREE_LIST_CHECK (NODE))

/* In a TREE_LIST in the argument of attribute abi_tag, indicates that the tag
   was inherited from a template parameter, not explicitly indicated.  */
#define ABI_TAG_IMPLICIT(NODE) TREE_LANG_FLAG_0 (TREE_LIST_CHECK (NODE))

/* In a TREE_LIST for a parameter-declaration-list, indicates that all the
   parameters in the list have declarators enclosed in ().  */
#define PARENTHESIZED_LIST_P(NODE) TREE_LANG_FLAG_0 (TREE_LIST_CHECK (NODE))

/* Non zero if this is a using decl for a dependent scope.  */
#define DECL_DEPENDENT_P(NODE) DECL_LANG_FLAG_0 (USING_DECL_CHECK (NODE))

/* The scope named in a using decl.  */
#define USING_DECL_SCOPE(NODE) DECL_RESULT_FLD (USING_DECL_CHECK (NODE))

/* The decls named by a using decl.  */
#define USING_DECL_DECLS(NODE) DECL_INITIAL (USING_DECL_CHECK (NODE))

/* Non zero if the using decl refers to a dependent type.  */
#define USING_DECL_TYPENAME_P(NODE) DECL_LANG_FLAG_1 (USING_DECL_CHECK (NODE))

/* True if member using decl NODE refers to a non-inherited NODE.  */
#define USING_DECL_UNRELATED_P(NODE) DECL_LANG_FLAG_2 (USING_DECL_CHECK (NODE))

/* True iff the CONST_DECL is a class-scope clone from C++20 using enum,
   created by handle_using_decl.  */
#define CONST_DECL_USING_P(NODE)			\
  (TREE_CODE (NODE) == CONST_DECL			\
   && TREE_TYPE (NODE)					\
   && TREE_CODE (TREE_TYPE (NODE)) == ENUMERAL_TYPE	\
   && DECL_CONTEXT (NODE) != TREE_TYPE (NODE))

/* In a FUNCTION_DECL, this is nonzero if this function was defined in
   the class definition.  We have saved away the text of the function,
   but have not yet processed it.  */
#define DECL_PENDING_INLINE_P(NODE) \
  (LANG_DECL_FN_CHECK (NODE)->pending_inline_p)

/* If DECL_PENDING_INLINE_P holds, this is the saved text of the
   function.  */
#define DECL_PENDING_INLINE_INFO(NODE) \
  (LANG_DECL_FN_CHECK (NODE)->u.pending_inline_info)

/* Nonzero for TYPE_DECL means that it was written 'using name = type'.  */
#define TYPE_DECL_ALIAS_P(NODE) \
  DECL_LANG_FLAG_6 (TYPE_DECL_CHECK (NODE))

/* Nonzero for a type which is an alias for another type; i.e, a type
   which declaration was written 'using name-of-type =
   another-type'.  */
#define TYPE_ALIAS_P(NODE)			\
  (TYPE_P (NODE)				\
   && TYPE_NAME (NODE)				\
   && TREE_CODE (TYPE_NAME (NODE)) == TYPE_DECL	\
   && TYPE_DECL_ALIAS_P (TYPE_NAME (NODE)))

/* If non-NULL for a VAR_DECL, FUNCTION_DECL, TYPE_DECL, TEMPLATE_DECL,
   or CONCEPT_DECL, the entity is either a template specialization (if
   DECL_USE_TEMPLATE is nonzero) or the abstract instance of the
   template itself.

   In either case, DECL_TEMPLATE_INFO is a TEMPLATE_INFO, whose
   TI_TEMPLATE is the TEMPLATE_DECL of which this entity is a
   specialization or abstract instance.  The TI_ARGS is the
   template arguments used to specialize the template.

   Consider:

      template <typename T> struct S { friend void f(T) {} };

   In this case, S<int>::f is, from the point of view of the compiler,
   an instantiation of a template -- but, from the point of view of
   the language, each instantiation of S results in a wholly unrelated
   global function f.  In this case, DECL_TEMPLATE_INFO for S<int>::f
   will be non-NULL, but DECL_USE_TEMPLATE will be zero.

   In a friend declaration, TI_TEMPLATE can be an overload set, or
   identifier.  */
#define DECL_TEMPLATE_INFO(NODE) \
  (DECL_LANG_SPECIFIC (TEMPLATE_INFO_DECL_CHECK (NODE)) \
   ->u.min.template_info)

/* For a lambda capture proxy, its captured variable.  */
#define DECL_CAPTURED_VARIABLE(NODE) \
  (LANG_DECL_MIN_CHECK (NODE)->access)

/* For a VAR_DECL, indicates that the variable is actually a
   non-static data member of anonymous union that has been promoted to
   variable status.  */
#define DECL_ANON_UNION_VAR_P(NODE) \
  (DECL_LANG_FLAG_4 (VAR_DECL_CHECK (NODE)))

/* Template information for a RECORD_TYPE or UNION_TYPE.  */
#define CLASSTYPE_TEMPLATE_INFO(NODE) \
  (TYPE_LANG_SLOT_1 (RECORD_OR_UNION_CHECK (NODE)))

/* Template information for a template template parameter.  */
#define TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO(NODE) \
  (TYPE_LANG_SLOT_1 (BOUND_TEMPLATE_TEMPLATE_PARM_TYPE_CHECK (NODE)))

/* Template information for an ENUMERAL_, RECORD_, UNION_TYPE, or
   BOUND_TEMPLATE_TEMPLATE_PARM type.  This ignores any alias
   templateness of NODE.  It'd be nice if this could unconditionally
   access the slot, rather than return NULL if given a
   non-templatable type.  */
#define TYPE_TEMPLATE_INFO(NODE)					\
  (TREE_CODE (NODE) == ENUMERAL_TYPE					\
   || TREE_CODE (NODE) == BOUND_TEMPLATE_TEMPLATE_PARM			\
   || RECORD_OR_UNION_TYPE_P (NODE)					\
   ? TYPE_LANG_SLOT_1 (NODE) : NULL_TREE)

/* Template information (if any) for an alias type.  */
#define TYPE_ALIAS_TEMPLATE_INFO(NODE)					\
  (DECL_LANG_SPECIFIC (TYPE_NAME (NODE))				\
   ? DECL_TEMPLATE_INFO (TYPE_NAME (NODE))				\
   : NULL_TREE)

/* If NODE is a type alias, this accessor returns the template info
   for the alias template (if any).  Otherwise behave as
   TYPE_TEMPLATE_INFO.  */
#define TYPE_TEMPLATE_INFO_MAYBE_ALIAS(NODE)				\
  (typedef_variant_p (NODE)						\
   ? TYPE_ALIAS_TEMPLATE_INFO (NODE)					\
   : TYPE_TEMPLATE_INFO (NODE))

/* Set the template information for a non-alias n ENUMERAL_, RECORD_,
   or UNION_TYPE to VAL.  ALIAS's are dealt with separately.  */
#define SET_TYPE_TEMPLATE_INFO(NODE, VAL)				\
  (TREE_CODE (NODE) == ENUMERAL_TYPE		\
   || (CLASS_TYPE_P (NODE) && !TYPE_ALIAS_P (NODE))			\
   ? (TYPE_LANG_SLOT_1 (NODE) = (VAL))					\
   : (DECL_TEMPLATE_INFO (TYPE_NAME (NODE)) = (VAL)))			\

#define TI_TEMPLATE(NODE) \
  ((struct tree_template_info*)TEMPLATE_INFO_CHECK (NODE))->tmpl
#define TI_ARGS(NODE) \
  ((struct tree_template_info*)TEMPLATE_INFO_CHECK (NODE))->args
#define TI_PENDING_TEMPLATE_FLAG(NODE) \
  TREE_LANG_FLAG_1 (TEMPLATE_INFO_CHECK (NODE))

/* For a class or variable template specialization, this contains the
   TEMPLATE_INFO result of most_specialized_partial_spec, i.e. the selected
   partial template specialization and arguments relative to it.  */
#define TI_PARTIAL_INFO(NODE) \
  (gcc_checking_assert (PRIMARY_TEMPLATE_P (TI_TEMPLATE (NODE))), \
   ((struct tree_template_info*)NODE)->partial)

/* For a given TREE_VEC containing a template argument list,
   this property contains the number of arguments that are not
   defaulted.  */
#define NON_DEFAULT_TEMPLATE_ARGS_COUNT(NODE) \
  TREE_CHAIN (TREE_VEC_CHECK (NODE))

/* Below are the setter and getter of the NON_DEFAULT_TEMPLATE_ARGS_COUNT
   property.  */
#define SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT(NODE, INT_VALUE) \
  NON_DEFAULT_TEMPLATE_ARGS_COUNT(NODE) = build_int_cst (NULL_TREE, INT_VALUE)
#if CHECKING_P
#define GET_NON_DEFAULT_TEMPLATE_ARGS_COUNT(NODE) \
    int_cst_value (NON_DEFAULT_TEMPLATE_ARGS_COUNT (NODE))
#else
#define GET_NON_DEFAULT_TEMPLATE_ARGS_COUNT(NODE) \
  NON_DEFAULT_TEMPLATE_ARGS_COUNT (NODE) \
  ? int_cst_value (NON_DEFAULT_TEMPLATE_ARGS_COUNT (NODE)) \
  : TREE_VEC_LENGTH (INNERMOST_TEMPLATE_ARGS (NODE))
#endif

/* True iff NODE represents the template args for a type-constraint,
   in which case the first one represents the constrained type.
   Currently only set during mangling.  */
#define TEMPLATE_ARGS_TYPE_CONSTRAINT_P(NODE) \
  TREE_PRIVATE (TREE_VEC_CHECK (NODE))

/* The list of access checks that were deferred during parsing
   which need to be performed at template instantiation time.

   FIXME this should be associated with the TEMPLATE_DECL, not the
   TEMPLATE_INFO.  */
#define TI_DEFERRED_ACCESS_CHECKS(NODE) \
  ((struct tree_template_info*)TEMPLATE_INFO_CHECK \
     (NODE))->deferred_access_checks

/* We use TREE_VECs to hold template arguments.  If there is only one
   level of template arguments, then the TREE_VEC contains the
   arguments directly.  If there is more than one level of template
   arguments, then each entry in the TREE_VEC is itself a TREE_VEC,
   containing the template arguments for a single level.  The first
   entry in the outer TREE_VEC is the outermost level of template
   parameters; the last is the innermost.

   It is incorrect to ever form a template argument vector containing
   only one level of arguments, but which is a TREE_VEC containing as
   its only entry the TREE_VEC for that level.

   For each TREE_VEC containing the template arguments for a single
   level, it's possible to get or set the number of non defaulted
   template arguments by using the accessor macros
   GET_NON_DEFAULT_TEMPLATE_ARGS_COUNT or
   SET_NON_DEFAULT_TEMPLATE_ARGS_COUNT.  */

/* Nonzero if the template arguments is actually a vector of vectors,
   rather than just a vector.  */
#define TMPL_ARGS_HAVE_MULTIPLE_LEVELS(NODE)		     \
  (NODE && TREE_VEC_LENGTH (NODE) && TREE_VEC_ELT (NODE, 0)  \
   && TREE_CODE (TREE_VEC_ELT (NODE, 0)) == TREE_VEC)

/* The depth of a template argument vector.  When called directly by
   the parser, we use a TREE_LIST rather than a TREE_VEC to represent
   template arguments.  In that case, there is only one level of template
   arguments.  We may even see NULL_TREE if there are 0 levels of
   template arguments, as in cp_parser_requires_expression.   */
#define TMPL_ARGS_DEPTH(NODE)						\
  ((NODE) == NULL_TREE ? 0						\
   : TMPL_ARGS_HAVE_MULTIPLE_LEVELS (NODE) ? TREE_VEC_LENGTH (NODE)	\
   : 1)

/* The LEVELth level of the template ARGS.  The outermost level of
   args is level 1, not level 0.  */
#define TMPL_ARGS_LEVEL(ARGS, LEVEL)		\
  (TMPL_ARGS_HAVE_MULTIPLE_LEVELS (ARGS)	\
   ? TREE_VEC_ELT (ARGS, (LEVEL) - 1)		\
   : (gcc_checking_assert ((LEVEL) == 1), (ARGS)))

/* Set the LEVELth level of the template ARGS to VAL.  This macro does
   not work with single-level argument vectors.  */
#define SET_TMPL_ARGS_LEVEL(ARGS, LEVEL, VAL)	\
  (TREE_VEC_ELT (ARGS, (LEVEL) - 1) = (VAL))

/* Accesses the IDXth parameter in the LEVELth level of the ARGS.  */
#define TMPL_ARG(ARGS, LEVEL, IDX)				\
  (TREE_VEC_ELT (TMPL_ARGS_LEVEL (ARGS, LEVEL), IDX))

/* Given a single level of template arguments in NODE, return the
   number of arguments.  */
#define NUM_TMPL_ARGS(NODE)				\
  (TREE_VEC_LENGTH (NODE))

/* Returns the innermost level of template arguments in ARGS.  */
#define INNERMOST_TEMPLATE_ARGS(NODE) \
  (get_innermost_template_args ((NODE), 1))

/* The number of levels of template parameters given by NODE.  */
#define TMPL_PARMS_DEPTH(NODE) \
  ((HOST_WIDE_INT) TREE_INT_CST_LOW (TREE_PURPOSE (NODE)))

/* The TEMPLATE_DECL instantiated or specialized by NODE.  This
   TEMPLATE_DECL will be the immediate parent, not the most general
   template.  For example, in:

      template <class T> struct S { template <class U> void f(U); }

   the FUNCTION_DECL for S<int>::f<double> will have, as its
   DECL_TI_TEMPLATE, `template <class U> S<int>::f<U>'.

   As a special case, for a member friend template of a template
   class, this value will not be a TEMPLATE_DECL, but rather an
   IDENTIFIER_NODE or OVERLOAD indicating the name of the template and
   any explicit template arguments provided.  For example, in:

     template <class T> struct S { friend void f<int>(int, double); }

   the DECL_TI_TEMPLATE will be an IDENTIFIER_NODE for `f' and the
   DECL_TI_ARGS will be {int}.

   For a FIELD_DECL with a non-static data member initializer, this value
   is the FIELD_DECL it was instantiated from.  */
#define DECL_TI_TEMPLATE(NODE)      TI_TEMPLATE (DECL_TEMPLATE_INFO (NODE))

/* The template arguments used to obtain this decl from the most
   general form of DECL_TI_TEMPLATE.  For the example given for
   DECL_TI_TEMPLATE, the DECL_TI_ARGS will be {int, double}.  These
   are always the full set of arguments required to instantiate this
   declaration from the most general template specialized here.  */
#define DECL_TI_ARGS(NODE)	    TI_ARGS (DECL_TEMPLATE_INFO (NODE))

/* The TEMPLATE_DECL associated with NODE, a class type.  Even if NODE
   will be generated from a partial specialization, the TEMPLATE_DECL
   referred to here will be the original template.  For example,
   given:

      template <typename T> struct S {};
      template <typename T> struct S<T*> {};

   the CLASSTYPE_TI_TEMPLATE for S<int*> will be S, not the S<T*>.

   For a member class template, CLASSTYPE_TI_TEMPLATE always refers to the
   partial instantiation rather than the primary template.  CLASSTYPE_TI_ARGS
   are for the primary template if the partial instantiation isn't
   specialized, or for the explicit specialization if it is, e.g.

      template <class T> class C { template <class U> class D; }
      template <> template <class U> class C<int>::D;  */
#define CLASSTYPE_TI_TEMPLATE(NODE) TI_TEMPLATE (CLASSTYPE_TEMPLATE_INFO (NODE))
#define CLASSTYPE_TI_ARGS(NODE)     TI_ARGS (CLASSTYPE_TEMPLATE_INFO (NODE))

/* For a template instantiation TYPE, returns the TYPE corresponding
   to the primary template.  Otherwise returns TYPE itself.  */
#define CLASSTYPE_PRIMARY_TEMPLATE_TYPE(TYPE)				\
  ((CLASSTYPE_USE_TEMPLATE ((TYPE))					\
    && !CLASSTYPE_TEMPLATE_SPECIALIZATION ((TYPE)))			\
   ? TREE_TYPE (DECL_TEMPLATE_RESULT (DECL_PRIMARY_TEMPLATE		\
				      (CLASSTYPE_TI_TEMPLATE ((TYPE))))) \
   : (TYPE))

/* Like CLASS_TI_TEMPLATE, but also works for ENUMERAL_TYPEs.  */
#define TYPE_TI_TEMPLATE(NODE)			\
  (TI_TEMPLATE (TYPE_TEMPLATE_INFO (NODE)))

/* Like DECL_TI_ARGS, but for an ENUMERAL_, RECORD_, or UNION_TYPE.  */
#define TYPE_TI_ARGS(NODE)			\
  (TI_ARGS (TYPE_TEMPLATE_INFO (NODE)))

#define INNERMOST_TEMPLATE_PARMS(NODE)  TREE_VALUE (NODE)

/* Nonzero if NODE (a TEMPLATE_DECL) is a member template, in the
   sense of [temp.mem].  */
#define DECL_MEMBER_TEMPLATE_P(NODE) \
  (DECL_LANG_FLAG_1 (TEMPLATE_DECL_CHECK (NODE)))

/* Nonzero if the NODE corresponds to the template parameters for a
   member template, whose inline definition is being processed after
   the class definition is complete.  */
#define TEMPLATE_PARMS_FOR_INLINE(NODE) TREE_LANG_FLAG_1 (NODE)

/* Determine if a declaration (PARM_DECL or FIELD_DECL) is a pack.  */
#define DECL_PACK_P(NODE) \
  (DECL_P (NODE) && PACK_EXPANSION_P (TREE_TYPE (NODE)))

/* Determines if NODE is an expansion of one or more parameter packs,
   e.g., a TYPE_PACK_EXPANSION or EXPR_PACK_EXPANSION.  */
#define PACK_EXPANSION_P(NODE)                 \
  (TREE_CODE (NODE) == TYPE_PACK_EXPANSION     \
   || TREE_CODE (NODE) == EXPR_PACK_EXPANSION)

#define PACK_EXPANSION_CHECK(NODE) \
  TREE_CHECK2 (NODE, TYPE_PACK_EXPANSION, EXPR_PACK_EXPANSION)

#define PACK_INDEX_CHECK(NODE) \
  TREE_CHECK2 (NODE, PACK_INDEX_TYPE, PACK_INDEX_EXPR)

/* Extracts the type or expression pattern from a TYPE_PACK_EXPANSION or
   EXPR_PACK_EXPANSION.  */
#define PACK_EXPANSION_PATTERN(NODE)                            \
  (TREE_CODE (PACK_EXPANSION_CHECK (NODE)) == TYPE_PACK_EXPANSION \
   ? TREE_TYPE (NODE) : TREE_OPERAND (NODE, 0))

/* The list of parameter packs used in the PACK_EXPANSION_* node. The
   TREE_VALUE of each TREE_LIST contains the parameter packs.  */
#define PACK_EXPANSION_PARAMETER_PACKS(NODE)		\
  *(TREE_CODE (PACK_EXPANSION_CHECK (NODE)) == EXPR_PACK_EXPANSION \
    ? &TREE_OPERAND (NODE, 1)				\
    : &TYPE_MIN_VALUE_RAW (TYPE_PACK_EXPANSION_CHECK (NODE)))

/* Any additional template args to be applied when substituting into
   the pattern, set by tsubst_pack_expansion for partial instantiations.
   If this is a TREE_LIST, the TREE_VALUE of the first element is the
   usual template argument TREE_VEC, and the TREE_PURPOSE of later elements
   are enclosing functions that provided function parameter packs we'll need
   to map appropriately.  */
#define PACK_EXPANSION_EXTRA_ARGS(NODE)		\
  *(TREE_CODE (PACK_EXPANSION_CHECK (NODE)) == TYPE_PACK_EXPANSION \
    ? &TYPE_MAX_VALUE_RAW (NODE)			\
    : &TREE_OPERAND ((NODE), 2))

/* True if NODE is a pack index.  */
#define PACK_INDEX_P(NODE)                 \
  (TREE_CODE (NODE) == PACK_INDEX_TYPE     \
   || TREE_CODE (NODE) == PACK_INDEX_EXPR)

/* For a pack index T...[N], the pack expansion 'T...'.  */
#define PACK_INDEX_PACK(NODE) \
  (TREE_CODE (PACK_INDEX_CHECK (NODE)) == PACK_INDEX_TYPE \
   ? TREE_TYPE (NODE) : TREE_OPERAND (NODE, 0))

/* For a pack index T...[N], the index N.  */
#define PACK_INDEX_INDEX(NODE) \
  *(TREE_CODE (PACK_INDEX_CHECK (NODE)) == PACK_INDEX_TYPE \
    ? &TYPE_MAX_VALUE_RAW (NODE)			\
    : &TREE_OPERAND ((NODE), 1))

/* True iff this pack expansion is within a function context.  */
#define PACK_EXPANSION_LOCAL_P(NODE) \
  TREE_LANG_FLAG_0 (PACK_EXPANSION_CHECK (NODE))

/* True iff this pack expansion is for sizeof....  */
#define PACK_EXPANSION_SIZEOF_P(NODE) \
  TREE_LANG_FLAG_1 (PACK_EXPANSION_CHECK (NODE))

/* True iff this pack expansion is for auto... in lambda init-capture.  */
#define PACK_EXPANSION_AUTO_P(NODE) \
  TREE_LANG_FLAG_2 (PACK_EXPANSION_CHECK (NODE))

/* True if we must use PACK_EXPANSION_EXTRA_ARGS and avoid partial
   instantiation of this pack expansion.  */
#define PACK_EXPANSION_FORCE_EXTRA_ARGS_P(NODE) \
  TREE_LANG_FLAG_3 (PACK_EXPANSION_CHECK (NODE))

/* Indicates whether a pack expansion has been parenthesized.  Used for
   a pack expansion in a decltype.  */
#define PACK_INDEX_PARENTHESIZED_P(NODE) \
  TREE_LANG_FLAG_1 (TREE_CHECK (NODE, PACK_INDEX_EXPR))

/* Determine if this is an argument pack.  */
#define ARGUMENT_PACK_P(NODE)                          \
  (TREE_CODE (NODE) == TYPE_ARGUMENT_PACK              \
   || TREE_CODE (NODE) == NONTYPE_ARGUMENT_PACK)

#define ARGUMENT_PACK_CHECK(NODE) \
  TREE_CHECK2 (NODE, TYPE_ARGUMENT_PACK, NONTYPE_ARGUMENT_PACK)

/* The arguments stored in an argument pack. Arguments are stored in a
   TREE_VEC, which may have length zero.  */
#define ARGUMENT_PACK_ARGS(NODE)                               \
  (TREE_CODE (ARGUMENT_PACK_CHECK (NODE)) == TYPE_ARGUMENT_PACK \
   ? TREE_TYPE (NODE) : TREE_OPERAND (NODE, 0))

/* Whether the argument pack is "incomplete", meaning that more
   arguments can still be deduced. Incomplete argument packs are only
   used when the user has provided an explicit template argument list
   for a variadic function template. Some of the explicit template
   arguments will be placed into the beginning of the argument pack,
   but additional arguments might still be deduced.  */
#define ARGUMENT_PACK_INCOMPLETE_P(NODE)        \
  TREE_ADDRESSABLE (ARGUMENT_PACK_ARGS (NODE))

/* When ARGUMENT_PACK_INCOMPLETE_P, stores the explicit template
   arguments used to fill this pack.  */
#define ARGUMENT_PACK_EXPLICIT_ARGS(NODE)       \
  TREE_TYPE (ARGUMENT_PACK_ARGS (NODE))

/* In an ARGUMENT_PACK_SELECT, the argument pack from which an
   argument will be selected.  */
#define ARGUMENT_PACK_SELECT_FROM_PACK(NODE)				\
  (((struct tree_argument_pack_select *)ARGUMENT_PACK_SELECT_CHECK (NODE))->argument_pack)

/* In an ARGUMENT_PACK_SELECT, the index of the argument we want to
   select.  */
#define ARGUMENT_PACK_SELECT_INDEX(NODE)				\
  (((struct tree_argument_pack_select *)ARGUMENT_PACK_SELECT_CHECK (NODE))->index)

#define FOLD_EXPR_CHECK(NODE)						\
  TREE_CHECK4 (NODE, UNARY_LEFT_FOLD_EXPR, UNARY_RIGHT_FOLD_EXPR,	\
	       BINARY_LEFT_FOLD_EXPR, BINARY_RIGHT_FOLD_EXPR)

#define BINARY_FOLD_EXPR_CHECK(NODE) \
  TREE_CHECK2 (NODE, BINARY_LEFT_FOLD_EXPR, BINARY_RIGHT_FOLD_EXPR)

/* True if NODE is UNARY_FOLD_EXPR or a BINARY_FOLD_EXPR */
#define FOLD_EXPR_P(NODE)				\
  (TREE_CODE (NODE) == UNARY_LEFT_FOLD_EXPR		\
   || TREE_CODE (NODE) == UNARY_RIGHT_FOLD_EXPR		\
   || TREE_CODE (NODE) == BINARY_LEFT_FOLD_EXPR		\
   || TREE_CODE (NODE) == BINARY_RIGHT_FOLD_EXPR)

/* True when NODE is a fold over a compound assignment operator.  */
#define FOLD_EXPR_MODIFY_P(NODE) \
  TREE_LANG_FLAG_0 (FOLD_EXPR_CHECK (NODE))

/* An INTEGER_CST containing the tree code of the folded operator.  */
#define FOLD_EXPR_OP_RAW(NODE) \
  TREE_OPERAND (FOLD_EXPR_CHECK (NODE), 0)

/* The tree code of the folded operator.  */
#define FOLD_EXPR_OP(NODE) \
  ((enum tree_code) TREE_INT_CST_LOW (FOLD_EXPR_OP_RAW (NODE)))

/* The expression containing an unexpanded parameter pack.  */
#define FOLD_EXPR_PACK(NODE) \
  TREE_OPERAND (FOLD_EXPR_CHECK (NODE), 1)

/* In a binary fold expression, the argument with no unexpanded
   parameter packs.  */
#define FOLD_EXPR_INIT(NODE) \
  TREE_OPERAND (BINARY_FOLD_EXPR_CHECK (NODE), 2)

/* In a FUNCTION_DECL, the saved auto-return pattern.  */
#define DECL_SAVED_AUTO_RETURN_TYPE(NODE)		\
  (LANG_DECL_FN_CHECK (FUNCTION_DECL_CHECK (NODE))	\
   ->u.saved_auto_return_type)

/* In a RETURN_EXPR, whether the expression refers to the address
   of a local variable.  */
#define RETURN_EXPR_LOCAL_ADDR_P(NODE) \
  TREE_LANG_FLAG_0 (RETURN_EXPR_CHECK (NODE))

/* True if NODE is an implicit INDIRECT_REF from convert_from_reference.  */
#define REFERENCE_REF_P(NODE)				\
  (INDIRECT_REF_P (NODE)				\
   && TREE_TYPE (TREE_OPERAND (NODE, 0))		\
   && TYPE_REF_P (TREE_TYPE (TREE_OPERAND ((NODE), 0))))

/* Look through an implicit INDIRECT_REF from convert_from_reference.  */
#define STRIP_REFERENCE_REF(NODE)			\
  (REFERENCE_REF_P (NODE) ? TREE_OPERAND (NODE, 0) : NODE)

/* True iff this represents an lvalue being treated as an rvalue during return
   or throw as per [class.copy.elision].  */
#define IMPLICIT_RVALUE_P(NODE) \
  TREE_LANG_FLAG_3 (TREE_CHECK2 ((NODE), NON_LVALUE_EXPR, STATIC_CAST_EXPR))

#define NEW_EXPR_USE_GLOBAL(NODE) \
  TREE_LANG_FLAG_0 (NEW_EXPR_CHECK (NODE))
#define DELETE_EXPR_USE_GLOBAL(NODE) \
  TREE_LANG_FLAG_0 (DELETE_EXPR_CHECK (NODE))
#define DELETE_EXPR_USE_VEC(NODE) \
  TREE_LANG_FLAG_1 (DELETE_EXPR_CHECK (NODE))

/* True iff this represents returning a potential named return value.  */
#define INIT_EXPR_NRV_P(NODE) \
  TREE_LANG_FLAG_0 (INIT_EXPR_CHECK (NODE))

#define CALL_OR_AGGR_INIT_CHECK(NODE) \
  TREE_CHECK2 ((NODE), CALL_EXPR, AGGR_INIT_EXPR)

/* In a CALL_EXPR appearing in a template, true if Koenig lookup
   should be performed at instantiation time.  */
#define KOENIG_LOOKUP_P(NODE) TREE_LANG_FLAG_0 (CALL_EXPR_CHECK (NODE))

/* True if the arguments to NODE should be evaluated in left-to-right
   order regardless of PUSH_ARGS_REVERSED.  */
#define CALL_EXPR_ORDERED_ARGS(NODE) \
  TREE_LANG_FLAG_3 (CALL_OR_AGGR_INIT_CHECK (NODE))

/* True if the arguments to NODE should be evaluated in right-to-left
   order regardless of PUSH_ARGS_REVERSED.  */
#define CALL_EXPR_REVERSE_ARGS(NODE) \
  TREE_LANG_FLAG_5 (CALL_OR_AGGR_INIT_CHECK (NODE))

/* True if CALL_EXPR was written as an operator expression, not a function
   call.  */
#define CALL_EXPR_OPERATOR_SYNTAX(NODE) \
  TREE_LANG_FLAG_6 (CALL_OR_AGGR_INIT_CHECK (NODE))

/* A TREE_LIST containing the result of phase 1 name lookup of the operator
   overloads that are pertinent to the dependent operator expression whose
   type is NODE.  Each TREE_PURPOSE is an IDENTIFIER_NODE and TREE_VALUE is
   the corresponding (possibly empty) lookup result.  The TREE_TYPE of the
   first TREE_LIST node points back to NODE.  */
#define DEPENDENT_OPERATOR_TYPE_SAVED_LOOKUPS(NODE) \
  TYPE_VALUES_RAW (DEPENDENT_OPERATOR_TYPE_CHECK (NODE))

/* Guarded helper for the above accessor macro that takes a (templated)
   operator expression instead of the type thereof.  */
inline tree
templated_operator_saved_lookups (tree t)
{
  tree type = TREE_TYPE (EXPR_CHECK (t));
  if (type && TREE_CODE (type) == DEPENDENT_OPERATOR_TYPE)
    return DEPENDENT_OPERATOR_TYPE_SAVED_LOOKUPS (type);
  else
    return NULL_TREE;
}

/* Indicates whether a string literal has been parenthesized. Such
   usages are disallowed in certain circumstances.  */

#define PAREN_STRING_LITERAL_P(NODE) \
  TREE_LANG_FLAG_0 (STRING_CST_CHECK (NODE))

/* Indicates whether a COMPONENT_REF or a SCOPE_REF has been parenthesized, an
   INDIRECT_REF comes from parenthesizing a _DECL, or a PAREN_EXPR identifies a
   parenthesized initializer relevant for decltype(auto).  Currently only set
   some of the time in C++14 mode.  */

#define REF_PARENTHESIZED_P(NODE) \
  TREE_LANG_FLAG_2 (TREE_CHECK5 ((NODE), COMPONENT_REF, INDIRECT_REF, SCOPE_REF, VIEW_CONVERT_EXPR, PAREN_EXPR))

/* Nonzero if this AGGR_INIT_EXPR provides for initialization via a
   constructor call, rather than an ordinary function call.  */
#define AGGR_INIT_VIA_CTOR_P(NODE) \
  TREE_LANG_FLAG_0 (AGGR_INIT_EXPR_CHECK (NODE))

/* Nonzero if expanding this AGGR_INIT_EXPR should first zero-initialize
   the object.  */
#define AGGR_INIT_ZERO_FIRST(NODE) \
  TREE_LANG_FLAG_2 (AGGR_INIT_EXPR_CHECK (NODE))

/* Nonzero means that the call is the jump from a thunk to the
   thunked-to function.  */
#define AGGR_INIT_FROM_THUNK_P(NODE) \
  (AGGR_INIT_EXPR_CHECK (NODE)->base.protected_flag)

/* Nonzero means that the call was marked musttail.  */
#define AGGR_INIT_EXPR_MUST_TAIL(NODE) \
  (AGGR_INIT_EXPR_CHECK (NODE)->base.static_flag)

/* AGGR_INIT_EXPR accessors.  These are equivalent to the CALL_EXPR
   accessors, except for AGGR_INIT_EXPR_SLOT (which takes the place of
   CALL_EXPR_STATIC_CHAIN).  */

#define AGGR_INIT_EXPR_FN(NODE) TREE_OPERAND (AGGR_INIT_EXPR_CHECK (NODE), 1)
#define AGGR_INIT_EXPR_SLOT(NODE) \
  TREE_OPERAND (AGGR_INIT_EXPR_CHECK (NODE), 2)
#define AGGR_INIT_EXPR_ARG(NODE, I) \
  TREE_OPERAND (AGGR_INIT_EXPR_CHECK (NODE), (I) + 3)
#define aggr_init_expr_nargs(NODE) (VL_EXP_OPERAND_LENGTH(NODE) - 3)

/* AGGR_INIT_EXPR_ARGP returns a pointer to the argument vector for NODE.
   We can't use &AGGR_INIT_EXPR_ARG (NODE, 0) because that will complain if
   the argument count is zero when checking is enabled.  Instead, do
   the pointer arithmetic to advance past the 3 fixed operands in a
   AGGR_INIT_EXPR.  That produces a valid pointer to just past the end of
   the operand array, even if it's not valid to dereference it.  */
#define AGGR_INIT_EXPR_ARGP(NODE) \
  (&(TREE_OPERAND (AGGR_INIT_EXPR_CHECK (NODE), 0)) + 3)

/* Abstract iterators for AGGR_INIT_EXPRs.  */

/* Structure containing iterator state.  */
struct aggr_init_expr_arg_iterator {
  tree t;	/* the aggr_init_expr */
  int n;	/* argument count */
  int i;	/* next argument index */
};

/* Initialize the abstract argument list iterator object ITER with the
   arguments from AGGR_INIT_EXPR node EXP.  */
inline void
init_aggr_init_expr_arg_iterator (tree exp,
				       aggr_init_expr_arg_iterator *iter)
{
  iter->t = exp;
  iter->n = aggr_init_expr_nargs (exp);
  iter->i = 0;
}

/* Return the next argument from abstract argument list iterator object ITER,
   and advance its state.  Return NULL_TREE if there are no more arguments.  */
inline tree
next_aggr_init_expr_arg (aggr_init_expr_arg_iterator *iter)
{
  tree result;
  if (iter->i >= iter->n)
    return NULL_TREE;
  result = AGGR_INIT_EXPR_ARG (iter->t, iter->i);
  iter->i++;
  return result;
}

/* Initialize the abstract argument list iterator object ITER, then advance
   past and return the first argument.  Useful in for expressions, e.g.
     for (arg = first_aggr_init_expr_arg (exp, &iter); arg;
          arg = next_aggr_init_expr_arg (&iter))   */
inline tree
first_aggr_init_expr_arg (tree exp, aggr_init_expr_arg_iterator *iter)
{
  init_aggr_init_expr_arg_iterator (exp, iter);
  return next_aggr_init_expr_arg (iter);
}

/* Test whether there are more arguments in abstract argument list iterator
   ITER, without changing its state.  */
inline bool
more_aggr_init_expr_args_p (const aggr_init_expr_arg_iterator *iter)
{
  return (iter->i < iter->n);
}

/* Iterate through each argument ARG of AGGR_INIT_EXPR CALL, using variable
   ITER (of type aggr_init_expr_arg_iterator) to hold the iteration state.  */
#define FOR_EACH_AGGR_INIT_EXPR_ARG(arg, iter, call)			\
  for ((arg) = first_aggr_init_expr_arg ((call), &(iter)); (arg);	\
       (arg) = next_aggr_init_expr_arg (&(iter)))

/* We have an expression tree T that represents a call, either CALL_EXPR
   or AGGR_INIT_EXPR.  Return a reference to the Nth argument.  */

inline tree&
get_nth_callarg (tree t, int n)
{
  switch (TREE_CODE (t))
    {
    case CALL_EXPR:
      return CALL_EXPR_ARG (t, n);

    case AGGR_INIT_EXPR:
      return AGGR_INIT_EXPR_ARG (t, n);

    default:
      gcc_unreachable ();
    }
}

/* VEC_INIT_EXPR accessors.  */
#define VEC_INIT_EXPR_SLOT(NODE) TREE_OPERAND (VEC_INIT_EXPR_CHECK (NODE), 0)
#define VEC_INIT_EXPR_INIT(NODE) TREE_OPERAND (VEC_INIT_EXPR_CHECK (NODE), 1)

/* Indicates that a VEC_INIT_EXPR is a potential constant expression.
   Only set when the current function is constexpr.  */
#define VEC_INIT_EXPR_IS_CONSTEXPR(NODE) \
  TREE_LANG_FLAG_0 (VEC_INIT_EXPR_CHECK (NODE))

/* Indicates that a VEC_INIT_EXPR is expressing value-initialization.  */
#define VEC_INIT_EXPR_VALUE_INIT(NODE) \
  TREE_LANG_FLAG_1 (VEC_INIT_EXPR_CHECK (NODE))

/* If T is a VEC_INIT_EXPR, return it, possibly stripping a TARGET_EXPR
   wrapper.  Otherwise, return null.  */
inline tree
get_vec_init_expr (tree t)
{
  if (t && TREE_CODE (t) == TARGET_EXPR)
    t = TARGET_EXPR_INITIAL (t);
  if (t && TREE_CODE (t) == VEC_INIT_EXPR)
    return t;
  return NULL_TREE;
}

/* The condition under which this MUST_NOT_THROW_EXPR actually blocks
   exceptions.  NULL_TREE means 'true'.  */
#define MUST_NOT_THROW_COND(NODE) \
  TREE_OPERAND (MUST_NOT_THROW_EXPR_CHECK (NODE), 1)

/* Reasons why MUST_NOT_THROW_EXPR has been created.  */

/* Indicates MUST_NOT_THROW_EXPR has been created to wrap body of
   a noexcept function.  */
#define MUST_NOT_THROW_NOEXCEPT_P(NODE) \
  TREE_LANG_FLAG_0 (MUST_NOT_THROW_EXPR_CHECK (NODE))

/* Indicates MUST_NOT_THROW_EXPR has been created to wrap construction of
   exception object during throw.  */
#define MUST_NOT_THROW_THROW_P(NODE) \
  TREE_LANG_FLAG_1 (MUST_NOT_THROW_EXPR_CHECK (NODE))

/* Indicates MUST_NOT_THROW_EXPR has been created to wrap construction of
   handler parameter during catch.  */
#define MUST_NOT_THROW_CATCH_P(NODE) \
  TREE_LANG_FLAG_2 (MUST_NOT_THROW_EXPR_CHECK (NODE))

/* The TYPE_MAIN_DECL for a class template type is a TYPE_DECL, not a
   TEMPLATE_DECL.  This macro determines whether or not a given class
   type is really a template type, as opposed to an instantiation or
   specialization of one.  */
#define CLASSTYPE_IS_TEMPLATE(NODE)  \
  (CLASSTYPE_TEMPLATE_INFO (NODE)    \
   && !CLASSTYPE_USE_TEMPLATE (NODE) \
   && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (NODE)))

/* The name used by the user to name the typename type.  Typically,
   this is an IDENTIFIER_NODE, and the same as the DECL_NAME on the
   corresponding TYPE_DECL.  However, this may also be a
   TEMPLATE_ID_EXPR if we had something like `typename X::Y<T>'.  */
#define TYPENAME_TYPE_FULLNAME(NODE) \
  (TYPE_VALUES_RAW (TYPENAME_TYPE_CHECK (NODE)))

/* True if a TYPENAME_TYPE was declared as an "enum".  */
#define TYPENAME_IS_ENUM_P(NODE) \
  (TREE_LANG_FLAG_0 (TYPENAME_TYPE_CHECK (NODE)))

/* True if a TYPENAME_TYPE was declared as a "class" or "struct".  */
#define TYPENAME_IS_CLASS_P(NODE) \
  (TREE_LANG_FLAG_1 (TYPENAME_TYPE_CHECK (NODE)))

/* True if a TYPENAME_TYPE was declared as a "union".  */
#define TYPENAME_IS_UNION_P(NODE) \
  (TREE_LANG_FLAG_3 (TYPENAME_TYPE_CHECK (NODE)))

/* True if a TYPENAME_TYPE is in the process of being resolved.  */
#define TYPENAME_IS_RESOLVING_P(NODE) \
  (TREE_LANG_FLAG_2 (TYPENAME_TYPE_CHECK (NODE)))

/* [class.virtual]

   A class that declares or inherits a virtual function is called a
   polymorphic class.  */
#define TYPE_POLYMORPHIC_P(NODE) (TREE_LANG_FLAG_2 (NODE))

/* Nonzero if this class has a virtual function table pointer.  */
#define TYPE_CONTAINS_VPTR_P(NODE)		\
  (TYPE_POLYMORPHIC_P (NODE) || CLASSTYPE_VBASECLASSES (NODE))

/* Nonzero if NODE is a FUNCTION_DECL or VAR_DECL (for a decl
   with namespace scope) declared in a local scope.  */
#define DECL_LOCAL_DECL_P(NODE) \
  DECL_LANG_FLAG_0 (VAR_OR_FUNCTION_DECL_CHECK (NODE))

/* The namespace-scope decl a DECL_LOCAL_DECL_P aliases.  */
#define DECL_LOCAL_DECL_ALIAS(NODE)			\
  DECL_ACCESS ((gcc_checking_assert (DECL_LOCAL_DECL_P (NODE)), NODE))

/* True if NODE was declared with auto in its return type, but it has
   started compilation and so the return type might have been changed by
   return type deduction; its declared return type should be found in
   DECL_SAVED_AUTO_RETURN_TYPE (NODE).   */
#define FNDECL_USED_AUTO(NODE) \
  TREE_LANG_FLAG_2 (FUNCTION_DECL_CHECK (NODE))

/* True if NODE is needed for a manifestly constant-evaluated expression.
   This doesn't especially need to be a flag, since currently it's only
   used for error recovery; if we run out of function flags it could move
   to an attribute.  */
#define FNDECL_MANIFESTLY_CONST_EVALUATED(NODE) \
  TREE_LANG_FLAG_4 (FUNCTION_DECL_CHECK (NODE))

/* True for artificial decls added for OpenMP privatized non-static
   data members.  */
#define DECL_OMP_PRIVATIZED_MEMBER(NODE) \
  (DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE))->u.base.anticipated_p)

/* Nonzero if NODE is an artificial FUNCTION_DECL for
   #pragma omp declare reduction.  */
#define DECL_OMP_DECLARE_REDUCTION_P(NODE) \
  (LANG_DECL_FN_CHECK (DECL_COMMON_CHECK (NODE))->omp_declare_reduction_p)

/* Nonzero if NODE is an artificial FUNCTION_DECL for
   #pragma omp declare mapper.  */
#define DECL_OMP_DECLARE_MAPPER_P(NODE) \
  (DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE))->u.base.omp_declare_mapper_p)

/* Nonzero if DECL has been declared threadprivate by
   #pragma omp threadprivate.  */
#define CP_DECL_THREADPRIVATE_P(DECL) \
  (DECL_LANG_SPECIFIC (VAR_DECL_CHECK (DECL))->u.base.threadprivate_or_deleted_p)

/* Nonzero if NODE is a VAR_DECL which has been declared inline.  */
#define DECL_VAR_DECLARED_INLINE_P(NODE) \
  (DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE))			\
   ? DECL_LANG_SPECIFIC (NODE)->u.base.var_declared_inline_p	\
   : false)
#define SET_DECL_VAR_DECLARED_INLINE_P(NODE) \
  (DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE))->u.base.var_declared_inline_p \
   = true)

/* True if NODE is a constant variable with a value-dependent initializer.  */
#define DECL_DEPENDENT_INIT_P(NODE)				\
  (DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE))			\
   && DECL_LANG_SPECIFIC (NODE)->u.base.dependent_init_p)
#define SET_DECL_DEPENDENT_INIT_P(NODE, X) \
  (DECL_LANG_SPECIFIC (VAR_DECL_CHECK (NODE))->u.base.dependent_init_p = (X))

/* Nonzero if NODE is an artificial VAR_DECL for a C++17 structured binding
   declaration or one of VAR_DECLs for the user identifiers in it.  */
#define DECL_DECOMPOSITION_P(NODE) \
  (VAR_P (NODE) && DECL_LANG_SPECIFIC (NODE)			\
   ? DECL_LANG_SPECIFIC (NODE)->u.base.selector == lds_decomp		\
   : false)

/* The underlying artificial VAR_DECL for structured binding.  On the
   artificial base VAR_DECL this can be NULL, or integer_{zero,one}_node
   for structured binding used in if/while/for resp. switch conditions,
   or a TARGET_EXPR with the condition value after cp_finish_decomp in
   those cases.  */
#define DECL_DECOMP_BASE(NODE) \
  (LANG_DECL_DECOMP_CHECK (NODE)->base)

/* True for the artificial VAR_DECL for structured binding.  */
#define DECL_DECOMP_IS_BASE(NODE) \
  (!DECL_DECOMP_BASE (NODE) || !VAR_P (DECL_DECOMP_BASE (NODE)))

/* Nonzero if NODE is an inline VAR_DECL.  In C++17, static data members
   declared with constexpr specifier are implicitly inline variables.  */
#define DECL_INLINE_VAR_P(NODE) \
  (DECL_VAR_DECLARED_INLINE_P (NODE)				\
   || (cxx_dialect >= cxx17					\
       && DECL_DECLARED_CONSTEXPR_P (NODE)			\
       && DECL_CLASS_SCOPE_P (NODE)))

/* Nonzero if DECL was declared with '= delete'.
   = delete("reason") is represented in addition to this flag by DECL_INITIAL
   being STRING_CST with the reason and TREE_TYPE of the STRING_CST the
   RID_DELETE IDENTIFIER_NODE.  */
#define DECL_DELETED_FN(DECL) \
  (LANG_DECL_FN_CHECK (DECL)->min.base.threadprivate_or_deleted_p)

/* Nonzero if DECL was declared with '= default' (maybe implicitly).  */
#define DECL_DEFAULTED_FN(DECL) \
  (LANG_DECL_FN_CHECK (DECL)->defaulted_p)

/* Nonzero if DECL is explicitly defaulted in the class body.  */
#define DECL_DEFAULTED_IN_CLASS_P(DECL)					\
  (DECL_DEFAULTED_FN (DECL) && DECL_INITIALIZED_IN_CLASS_P (DECL))
/* Nonzero if DECL was defaulted outside the class body.  */
#define DECL_DEFAULTED_OUTSIDE_CLASS_P(DECL)				\
  (DECL_DEFAULTED_FN (DECL)						\
   && !(DECL_ARTIFICIAL (DECL) || DECL_INITIALIZED_IN_CLASS_P (DECL)))

/* Record whether a typedef for type `int' was actually `signed int'.  */
#define C_TYPEDEF_EXPLICITLY_SIGNED(EXP) DECL_LANG_FLAG_1 (EXP)

/* Returns nonzero if DECL has external linkage, as specified by the
   language standard.  (This predicate may hold even when the
   corresponding entity is not actually given external linkage in the
   object file; see decl_linkage for details.)  */
#define DECL_EXTERNAL_LINKAGE_P(DECL) \
  (decl_linkage (DECL) == lk_external)

/* Keep these codes in ascending code order.  */

#define INTEGRAL_CODE_P(CODE)	\
  ((CODE) == ENUMERAL_TYPE	\
   || (CODE) == BOOLEAN_TYPE	\
   || (CODE) == INTEGER_TYPE)

/* [basic.fundamental]

   Types  bool, char, wchar_t, and the signed and unsigned integer types
   are collectively called integral types.

   Note that INTEGRAL_TYPE_P, as defined in tree.h, allows enumeration
   types as well, which is incorrect in C++.  Keep these checks in
   ascending code order.  */
#define CP_INTEGRAL_TYPE_P(TYPE)		\
  (TREE_CODE (TYPE) == BOOLEAN_TYPE		\
   || TREE_CODE (TYPE) == INTEGER_TYPE)

/* Returns true if TYPE is an integral or enumeration name.  Keep
   these checks in ascending code order.  */
#define INTEGRAL_OR_ENUMERATION_TYPE_P(TYPE) \
   (TREE_CODE (TYPE) == ENUMERAL_TYPE || CP_INTEGRAL_TYPE_P (TYPE))

/* Returns true if TYPE is an integral or unscoped enumeration type.  */
#define INTEGRAL_OR_UNSCOPED_ENUMERATION_TYPE_P(TYPE) \
   (UNSCOPED_ENUM_P (TYPE) || CP_INTEGRAL_TYPE_P (TYPE))

/* True if the class type TYPE is a literal type.  */
#define CLASSTYPE_LITERAL_P(TYPE)              \
   (LANG_TYPE_CLASS_CHECK (TYPE)->is_literal)

/* [basic.fundamental]

   Integral and floating types are collectively called arithmetic
   types.

   As a GNU extension, we also accept complex types.

   Keep these checks in ascending code order.  */
#define ARITHMETIC_TYPE_P(TYPE) \
  (CP_INTEGRAL_TYPE_P (TYPE) \
   || SCALAR_FLOAT_TYPE_P (TYPE) \
   || TREE_CODE (TYPE) == COMPLEX_TYPE)

/* [basic.types]

   Arithmetic types, enumeration types, pointer types,
   pointer-to-member types, and std::nullptr_t are collectively called
   scalar types.

   Keep these checks in ascending code order.  */
#define SCALAR_TYPE_P(TYPE)			\
  (TYPE_PTRDATAMEM_P (TYPE)			\
   || TREE_CODE (TYPE) == ENUMERAL_TYPE		\
   || ARITHMETIC_TYPE_P (TYPE)			\
   || TYPE_PTR_P (TYPE)				\
   || TYPE_PTRMEMFUNC_P (TYPE)                  \
   || NULLPTR_TYPE_P (TYPE))

/* Determines whether this type is a C++0x scoped enumeration
   type. Scoped enumerations types are introduced via "enum class" or
   "enum struct", e.g.,

     enum class Color {
       Red, Green, Blue
     };

   Scoped enumeration types are different from normal (unscoped)
   enumeration types in several ways:

     - The enumerators of a scoped enumeration type are only available
       within the scope of the enumeration type and not in the
       enclosing scope. For example, the Red color can be referred to
       with "Color::Red" but not "Red".

     - Scoped enumerators and enumerations do not implicitly convert
       to integers or 'bool'.

     - The underlying type of the enum is well-defined.  */
#define SCOPED_ENUM_P(TYPE)                                             \
  (TREE_CODE (TYPE) == ENUMERAL_TYPE && ENUM_IS_SCOPED (TYPE))

/* Determine whether this is an unscoped enumeration type.  */
#define UNSCOPED_ENUM_P(TYPE)                                           \
  (TREE_CODE (TYPE) == ENUMERAL_TYPE && !ENUM_IS_SCOPED (TYPE))

/* Set the flag indicating whether an ENUMERAL_TYPE is a C++0x scoped
   enumeration type (1) or a normal (unscoped) enumeration type
   (0).  */
#define SET_SCOPED_ENUM_P(TYPE, VAL)                    \
  (ENUM_IS_SCOPED (TYPE) = (VAL))

#define SET_OPAQUE_ENUM_P(TYPE, VAL)                    \
  (ENUM_IS_OPAQUE (TYPE) = (VAL))

#define OPAQUE_ENUM_P(TYPE)				\
  (TREE_CODE (TYPE) == ENUMERAL_TYPE && ENUM_IS_OPAQUE (TYPE))

/* [dcl.init.aggr]

   An aggregate is an array or a class with no user-provided
   constructors, no brace-or-equal-initializers for non-static data
   members, no private or protected non-static data members, no
   base classes, and no virtual functions.

   As an extension, we also treat vectors as aggregates.  Keep these
   checks in ascending code order.  */
#define CP_AGGREGATE_TYPE_P(TYPE)				\
  (gnu_vector_type_p (TYPE)					\
   || TREE_CODE (TYPE) == ARRAY_TYPE				\
   || (CLASS_TYPE_P (TYPE) && COMPLETE_TYPE_P (TYPE) && !CLASSTYPE_NON_AGGREGATE (TYPE)))

/* Nonzero for a class type means that the class type has a
   user-declared constructor.  */
#define TYPE_HAS_USER_CONSTRUCTOR(NODE) (TYPE_LANG_FLAG_1 (NODE))

/* Nonzero means that the FUNCTION_TYPE or METHOD_TYPE has a
   late-specified return type.  */
#define TYPE_HAS_LATE_RETURN_TYPE(NODE) \
  (TYPE_LANG_FLAG_2 (FUNC_OR_METHOD_CHECK (NODE)))

/* When appearing in an INDIRECT_REF, it means that the tree structure
   underneath is actually a call to a constructor.  This is needed
   when the constructor must initialize local storage (which can
   be automatically destroyed), rather than allowing it to allocate
   space from the heap.

   When appearing in a SAVE_EXPR, it means that underneath
   is a call to a constructor.

   When appearing in a CONSTRUCTOR, the expression is an unconverted
   compound literal.

   When appearing in a CALL_EXPR, it means that it is a call to
   a constructor.

   When appearing in a FIELD_DECL, it means that this field
   has been duly initialized in its constructor.  */
#define TREE_HAS_CONSTRUCTOR(NODE) (TREE_LANG_FLAG_4 (NODE))

/* True if NODE is a brace-enclosed initializer.  */
#define BRACE_ENCLOSED_INITIALIZER_P(NODE) \
  (TREE_CODE (NODE) == CONSTRUCTOR && TREE_TYPE (NODE) == init_list_type_node)

/* True if NODE is a compound-literal, i.e., a brace-enclosed
   initializer cast to a particular type.  This is mostly only set during
   template parsing; once the initializer has been digested into an actual
   value of the type, the expression is represented by a TARGET_EXPR.  */
#define COMPOUND_LITERAL_P(NODE) \
  (TREE_CODE (NODE) == CONSTRUCTOR && TREE_HAS_CONSTRUCTOR (NODE))

#define EMPTY_CONSTRUCTOR_P(NODE) (TREE_CODE (NODE) == CONSTRUCTOR \
				   && vec_safe_is_empty(CONSTRUCTOR_ELTS(NODE))\
				   && !TREE_HAS_CONSTRUCTOR (NODE))

/* True if NODE is a init-list used as a direct-initializer, i.e.
   B b{1,2}, not B b({1,2}) or B b = {1,2}.  */
#define CONSTRUCTOR_IS_DIRECT_INIT(NODE) (TREE_LANG_FLAG_0 (CONSTRUCTOR_CHECK (NODE)))

/* True if this CONSTRUCTOR is instantiation-dependent and needs to be
   substituted.  */
#define CONSTRUCTOR_IS_DEPENDENT(NODE) \
  (TREE_LANG_FLAG_1 (CONSTRUCTOR_CHECK (NODE)))

/* True if this CONSTRUCTOR should not be used as a variable initializer
   because it was loaded from a constexpr variable with mutable fields.  */
#define CONSTRUCTOR_MUTABLE_POISON(NODE) \
  (TREE_LANG_FLAG_2 (CONSTRUCTOR_CHECK (NODE)))

/* True if this typed CONSTRUCTOR represents C99 compound-literal syntax rather
   than C++11 functional cast syntax.  */
#define CONSTRUCTOR_C99_COMPOUND_LITERAL(NODE) \
  (TREE_LANG_FLAG_3 (CONSTRUCTOR_CHECK (NODE)))

/* True if this CONSTRUCTOR contains PLACEHOLDER_EXPRs referencing the
   CONSTRUCTOR's type not nested inside another CONSTRUCTOR marked with
   CONSTRUCTOR_PLACEHOLDER_BOUNDARY.  */
#define CONSTRUCTOR_PLACEHOLDER_BOUNDARY(NODE) \
  (TREE_LANG_FLAG_5 (CONSTRUCTOR_CHECK (NODE)))

#define DIRECT_LIST_INIT_P(NODE) \
   (BRACE_ENCLOSED_INITIALIZER_P (NODE) && CONSTRUCTOR_IS_DIRECT_INIT (NODE))

/* True if this is a designated initializer (when we allow initializer-clauses
   mixed with designated-initializer-clauses set whenever there is at least
   one designated-initializer-clause), or a C99 designator.  */
#define CONSTRUCTOR_IS_DESIGNATED_INIT(NODE) \
  (TREE_LANG_FLAG_6 (CONSTRUCTOR_CHECK (NODE)))

/* True if this CONSTRUCTOR comes from a parenthesized list of values, e.g.
   A(1, 2, 3).  */
#define CONSTRUCTOR_IS_PAREN_INIT(NODE) \
  (CONSTRUCTOR_CHECK(NODE)->base.private_flag)

/* True if reshape_init built this sub-CONSTRUCTOR to undo the brace elision
   of the original CONSTRUCTOR.  This flag is used during C++20 aggregate
   CTAD.  */
#define CONSTRUCTOR_BRACES_ELIDED_P(NODE) \
  (CONSTRUCTOR_CHECK (NODE)->base.protected_flag)

/* True if NODE represents a conversion for direct-initialization in a
   template.  Set by perform_implicit_conversion_flags.  */
#define IMPLICIT_CONV_EXPR_DIRECT_INIT(NODE) \
  (TREE_LANG_FLAG_0 (IMPLICIT_CONV_EXPR_CHECK (NODE)))

/* True if NODE represents a dependent conversion of a non-type template
   argument.  Set by maybe_convert_nontype_argument.  */
#define IMPLICIT_CONV_EXPR_NONTYPE_ARG(NODE) \
  (TREE_LANG_FLAG_1 (IMPLICIT_CONV_EXPR_CHECK (NODE)))

/* True if NODE represents a conversion for braced-init-list in a
   template.  Set by perform_implicit_conversion_flags.  */
#define IMPLICIT_CONV_EXPR_BRACED_INIT(NODE) \
  (TREE_LANG_FLAG_2 (IMPLICIT_CONV_EXPR_CHECK (NODE)))

/* True if NODE represents a conversion forced to be represented in
   maybe_convert_nontype_argument, i.e. for an alias template.  */
#define IMPLICIT_CONV_EXPR_FORCED(NODE) \
  (TREE_LANG_FLAG_3 (IMPLICIT_CONV_EXPR_CHECK (NODE)))

/* Nonzero means that an object of this type cannot be initialized using
   an initializer list.  */
#define CLASSTYPE_NON_AGGREGATE(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->non_aggregate)
#define TYPE_NON_AGGREGATE_CLASS(NODE) \
  (CLASS_TYPE_P (NODE) && CLASSTYPE_NON_AGGREGATE (NODE))

/* Nonzero if there is a non-trivial X::op=(cv X&) for this class.  */
#define TYPE_HAS_COMPLEX_COPY_ASSIGN(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->has_complex_copy_assign)

/* Nonzero if there is a non-trivial X::X(cv X&) for this class.  */
#define TYPE_HAS_COMPLEX_COPY_CTOR(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->has_complex_copy_ctor)

/* Nonzero if there is a non-trivial X::op=(X&&) for this class.  */
#define TYPE_HAS_COMPLEX_MOVE_ASSIGN(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->has_complex_move_assign)

/* Nonzero if there is a non-trivial X::X(X&&) for this class.  */
#define TYPE_HAS_COMPLEX_MOVE_CTOR(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->has_complex_move_ctor)

/* Nonzero if there is no trivial default constructor for this class.  */
#define TYPE_HAS_COMPLEX_DFLT(NODE) (LANG_TYPE_CLASS_CHECK (NODE)->has_complex_dflt)

/* Nonzero if TYPE has a trivial destructor.  From [class.dtor]:

     A destructor is trivial if it is an implicitly declared
     destructor and if:

       - all of the direct base classes of its class have trivial
	 destructors,

       - for all of the non-static data members of its class that are
	 of class type (or array thereof), each such class has a
	 trivial destructor.  */
#define TYPE_HAS_TRIVIAL_DESTRUCTOR(NODE) \
  (!TYPE_HAS_NONTRIVIAL_DESTRUCTOR (NODE))

/* Nonzero for _TYPE node means that this type does not have a trivial
   destructor.  Therefore, destroying an object of this type will
   involve a call to a destructor.  This can apply to objects of
   ARRAY_TYPE if the type of the elements needs a destructor.  */
#define TYPE_HAS_NONTRIVIAL_DESTRUCTOR(NODE) \
  (TYPE_LANG_FLAG_4 (NODE))

/* Nonzero for class type means that the default constructor is trivial.  */
#define TYPE_HAS_TRIVIAL_DFLT(NODE) \
  (TYPE_HAS_DEFAULT_CONSTRUCTOR (NODE) && ! TYPE_HAS_COMPLEX_DFLT (NODE))

/* Nonzero for class type means that copy initialization of this type can use
   a bitwise copy.  */
#define TYPE_HAS_TRIVIAL_COPY_CTOR(NODE) \
  (TYPE_HAS_COPY_CTOR (NODE) && ! TYPE_HAS_COMPLEX_COPY_CTOR (NODE))

/* Nonzero for class type means that assignment of this type can use
   a bitwise copy.  */
#define TYPE_HAS_TRIVIAL_COPY_ASSIGN(NODE) \
  (TYPE_HAS_COPY_ASSIGN (NODE) && ! TYPE_HAS_COMPLEX_COPY_ASSIGN (NODE))

/* Returns true if NODE is a pointer-to-data-member.  */
#define TYPE_PTRDATAMEM_P(NODE)			\
  (TREE_CODE (NODE) == OFFSET_TYPE)

/* Returns true if NODE is a pointer.  */
#define TYPE_PTR_P(NODE)			\
  (TREE_CODE (NODE) == POINTER_TYPE)

/* Returns true if NODE is a reference.  */
#define TYPE_REF_P(NODE)			\
  (TREE_CODE (NODE) == REFERENCE_TYPE)

/* Returns true if NODE is a pointer or a reference.  */
#define INDIRECT_TYPE_P(NODE)			\
  (TYPE_PTR_P (NODE) || TYPE_REF_P (NODE))

/* Returns true if NODE is an object type:

     [basic.types]

     An object type is a (possibly cv-qualified) type that is not a
     function type, not a reference type, and not a void type.

   Keep these checks in ascending order, for speed.  */
#define TYPE_OBJ_P(NODE)			\
  (!TYPE_REF_P (NODE)				\
   && !VOID_TYPE_P (NODE)  		        \
   && !FUNC_OR_METHOD_TYPE_P (NODE))

/* Returns true if NODE is a pointer to an object.  Keep these checks
   in ascending tree code order.  */
#define TYPE_PTROB_P(NODE)					\
  (TYPE_PTR_P (NODE) && TYPE_OBJ_P (TREE_TYPE (NODE)))

/* Returns true if NODE is a reference to an object.  Keep these checks
   in ascending tree code order.  */
#define TYPE_REF_OBJ_P(NODE)					\
  (TYPE_REF_P (NODE) && TYPE_OBJ_P (TREE_TYPE (NODE)))

/* Returns true if NODE is a pointer to an object, or a pointer to
   void.  Keep these checks in ascending tree code order.  */
#define TYPE_PTROBV_P(NODE)					\
  (TYPE_PTR_P (NODE)						\
   && !FUNC_OR_METHOD_TYPE_P (TREE_TYPE (NODE)))

/* Returns true if NODE is a pointer to function type.  */
#define TYPE_PTRFN_P(NODE)				\
  (TYPE_PTR_P (NODE)			                \
   && TREE_CODE (TREE_TYPE (NODE)) == FUNCTION_TYPE)

/* Returns true if NODE is a reference to function type.  */
#define TYPE_REFFN_P(NODE)				\
  (TYPE_REF_P (NODE)					\
   && TREE_CODE (TREE_TYPE (NODE)) == FUNCTION_TYPE)

/* Returns true if NODE is a pointer to member function type.  */
#define TYPE_PTRMEMFUNC_P(NODE)		\
  (TREE_CODE (NODE) == RECORD_TYPE	\
   && TYPE_PTRMEMFUNC_FLAG (NODE))

#define TYPE_PTRMEMFUNC_FLAG(NODE) \
  (TYPE_LANG_FLAG_2 (RECORD_TYPE_CHECK (NODE)))

/* Returns true if NODE is a pointer-to-member.  */
#define TYPE_PTRMEM_P(NODE) \
  (TYPE_PTRDATAMEM_P (NODE) || TYPE_PTRMEMFUNC_P (NODE))

/* Returns true if NODE is a pointer or a pointer-to-member.  */
#define TYPE_PTR_OR_PTRMEM_P(NODE) \
  (TYPE_PTR_P (NODE) || TYPE_PTRMEM_P (NODE))

/* Indicates when overload resolution may resolve to a pointer to
   member function. [expr.unary.op]/3 */
#define PTRMEM_OK_P(NODE) \
  TREE_LANG_FLAG_0 (TREE_CHECK3 ((NODE), ADDR_EXPR, OFFSET_REF, SCOPE_REF))

/* True if this ADDR_EXPR denotes a function call; that is, it's
   fn() rather than &fn.  */
#define ADDR_EXPR_DENOTES_CALL_P(NODE) \
  (ADDR_EXPR_CHECK(NODE)->base.protected_flag)

/* Get the POINTER_TYPE to the METHOD_TYPE associated with this
   pointer to member function.  TYPE_PTRMEMFUNC_P _must_ be true,
   before using this macro.  */
#define TYPE_PTRMEMFUNC_FN_TYPE(NODE) \
  (cp_build_qualified_type (TREE_TYPE (TYPE_FIELDS (NODE)),\
			    cp_type_quals (NODE)))

/* As above, but can be used in places that want an lvalue at the expense
   of not necessarily having the correct cv-qualifiers.  */
#define TYPE_PTRMEMFUNC_FN_TYPE_RAW(NODE) \
  (TREE_TYPE (TYPE_FIELDS (NODE)))

/* Returns `A' for a type like `int (A::*)(double)' */
#define TYPE_PTRMEMFUNC_OBJECT_TYPE(NODE) \
  TYPE_METHOD_BASETYPE (TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (NODE)))

/* The canonical internal RECORD_TYPE from the POINTER_TYPE to
   METHOD_TYPE.  */
#define TYPE_PTRMEMFUNC_TYPE(NODE) \
  TYPE_LANG_SLOT_1 (NODE)

/* For a pointer-to-member type of the form `T X::*', this is `X'.
   For a type like `void (X::*)() const', this type is `X', not `const
   X'.  To get at the `const X' you have to look at the
   TYPE_PTRMEM_POINTED_TO_TYPE; there, the first parameter will have
   type `const X*'.  */
#define TYPE_PTRMEM_CLASS_TYPE(NODE)			\
  (TYPE_PTRDATAMEM_P (NODE)					\
   ? TYPE_OFFSET_BASETYPE (NODE)		\
   : TYPE_PTRMEMFUNC_OBJECT_TYPE (NODE))

/* For a pointer-to-member type of the form `T X::*', this is `T'.  */
#define TYPE_PTRMEM_POINTED_TO_TYPE(NODE)		\
   (TYPE_PTRDATAMEM_P (NODE)				\
    ? TREE_TYPE (NODE)					\
    : TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (NODE)))

/* For a pointer-to-member constant `X::Y' this is the RECORD_TYPE for
   `X'.  */
#define PTRMEM_CST_CLASS(NODE) \
  TYPE_PTRMEM_CLASS_TYPE (TREE_TYPE (PTRMEM_CST_CHECK (NODE)))

/* For a pointer-to-member constant `X::Y' this is the _DECL for
   `Y'.  */
#define PTRMEM_CST_MEMBER(NODE) \
  (((ptrmem_cst_t)PTRMEM_CST_CHECK (NODE))->member)

/* For a pointer-to-member constant `X::Y' this is a location where
   the address of the member has been taken.  */
#define PTRMEM_CST_LOCATION(NODE) \
  (((ptrmem_cst_t)PTRMEM_CST_CHECK (NODE))->locus)

/* The expression in question for a TYPEOF_TYPE.  */
#define TYPEOF_TYPE_EXPR(NODE) (TYPE_VALUES_RAW (TYPEOF_TYPE_CHECK (NODE)))

/* The type in question for an UNDERLYING_TYPE.  */
#define UNDERLYING_TYPE_TYPE(NODE) \
  (TYPE_VALUES_RAW (UNDERLYING_TYPE_CHECK (NODE)))

/* The type in question for BASES.  */
#define BASES_TYPE(NODE) \
  (TYPE_VALUES_RAW (BASES_CHECK (NODE)))

#define BASES_DIRECT(NODE) \
  TREE_LANG_FLAG_0 (BASES_CHECK (NODE))

/* The expression in question for a DECLTYPE_TYPE.  */
#define DECLTYPE_TYPE_EXPR(NODE) (TYPE_VALUES_RAW (DECLTYPE_TYPE_CHECK (NODE)))

/* Whether the DECLTYPE_TYPE_EXPR of NODE was originally parsed as an
   id-expression or a member-access expression. When false, it was
   parsed as a full expression.  */
#define DECLTYPE_TYPE_ID_EXPR_OR_MEMBER_ACCESS_P(NODE) \
  (DECLTYPE_TYPE_CHECK (NODE))->type_common.string_flag

/* These flags indicate that we want different semantics from normal
   decltype: lambda capture just drops references,
   lambda proxies look through implicit dereference.  */
#define DECLTYPE_FOR_LAMBDA_CAPTURE(NODE) \
  TREE_LANG_FLAG_0 (DECLTYPE_TYPE_CHECK (NODE))
#define DECLTYPE_FOR_LAMBDA_PROXY(NODE) \
  TREE_LANG_FLAG_2 (DECLTYPE_TYPE_CHECK (NODE))
#define DECLTYPE_FOR_REF_CAPTURE(NODE) \
  TREE_LANG_FLAG_3 (DECLTYPE_TYPE_CHECK (NODE))

/* Nonzero for VAR_DECL and FUNCTION_DECL node means that `extern' was
   specified in its declaration.  This can also be set for an
   erroneously declared PARM_DECL.  */
#define DECL_THIS_EXTERN(NODE) \
  DECL_LANG_FLAG_2 (VAR_FUNCTION_OR_PARM_DECL_CHECK (NODE))

/* Nonzero for VAR_DECL and FUNCTION_DECL node means that `static' was
   specified in its declaration.  This can also be set for an
   erroneously declared PARM_DECL.  */
#define DECL_THIS_STATIC(NODE) \
  DECL_LANG_FLAG_6 (VAR_FUNCTION_OR_PARM_DECL_CHECK (NODE))

/* Nonzero for FIELD_DECL node means that this field is a lambda capture
   field for an array of runtime bound.  */
#define DECL_VLA_CAPTURE_P(NODE) \
  DECL_LANG_FLAG_1 (FIELD_DECL_CHECK (NODE))

/* Nonzero for PARM_DECL node means that this is an array function
   parameter, i.e, a[] rather than *a.  */
#define DECL_ARRAY_PARAMETER_P(NODE) \
  DECL_LANG_FLAG_1 (PARM_DECL_CHECK (NODE))

/* Nonzero for a FIELD_DECL who's NSMDI is currently being
   instantiated.  */
#define DECL_INSTANTIATING_NSDMI_P(NODE) \
  DECL_LANG_FLAG_2 (FIELD_DECL_CHECK (NODE))

/* Nonzero for FIELD_DECL node means that this field is a base class
   of the parent object, as opposed to a member field.  */
#define DECL_FIELD_IS_BASE(NODE) \
  DECL_LANG_FLAG_6 (FIELD_DECL_CHECK (NODE))

/* Nonzero for FIELD_DECL node means that this field is a simple (no
   explicit initializer) lambda capture field, making it invisible to
   name lookup in unevaluated contexts.  */
#define DECL_NORMAL_CAPTURE_P(NODE) \
  DECL_LANG_FLAG_7 (FIELD_DECL_CHECK (NODE))

/* Nonzero if TYPE is an anonymous union or struct type.  We have to use a
   flag for this because "A union for which objects or pointers are
   declared is not an anonymous union" [class.union].  */
#define ANON_AGGR_TYPE_P(NODE)				\
  (CLASS_TYPE_P (NODE) && LANG_TYPE_CLASS_CHECK (NODE)->anon_aggr)
#define SET_ANON_AGGR_TYPE_P(NODE)			\
  (LANG_TYPE_CLASS_CHECK (NODE)->anon_aggr = 1)

/* Nonzero if TYPE is an anonymous union type.  */
#define ANON_UNION_TYPE_P(NODE) \
  (TREE_CODE (NODE) == UNION_TYPE && ANON_AGGR_TYPE_P (NODE))

/* For an ANON_AGGR_TYPE_P the single FIELD_DECL it is used with.  */
#define ANON_AGGR_TYPE_FIELD(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->typeinfo_var)

/* Define fields and accessors for nodes representing declared names.  */

/* True if TYPE is an unnamed structured type with a typedef for
   linkage purposes.  In that case TYPE_NAME and TYPE_STUB_DECL of the
   MAIN-VARIANT are different.  */
#define TYPE_WAS_UNNAMED(NODE)				\
  (TYPE_NAME (TYPE_MAIN_VARIANT (NODE))			\
   != TYPE_STUB_DECL (TYPE_MAIN_VARIANT (NODE)))

/* C++: all of these are overloaded!  These apply only to TYPE_DECLs.  */

/* The format of each node in the DECL_FRIENDLIST is as follows:

   The TREE_PURPOSE will be the name of a function, i.e., an
   IDENTIFIER_NODE.  The TREE_VALUE will be itself a TREE_LIST, whose
   TREE_VALUEs are friends with the given name.  */
#define DECL_FRIENDLIST(NODE)		(DECL_INITIAL (NODE))
#define FRIEND_NAME(LIST) (TREE_PURPOSE (LIST))
#define FRIEND_DECLS(LIST) (TREE_VALUE (LIST))

/* The DECL_ACCESS, if non-NULL, is a TREE_LIST.  The TREE_PURPOSE of
   each node is a type; the TREE_VALUE is the access granted for this
   DECL in that type.  The DECL_ACCESS is set by access declarations.
   For example, if a member that would normally be public in a
   derived class is made protected, then the derived class and the
   protected_access_node will appear in the DECL_ACCESS for the node.  */
#define DECL_ACCESS(NODE) (LANG_DECL_MIN_CHECK (NODE)->access)

/* In artificial VAR_DECL created by cxa_allocate_exception
   this is reference count.  */
#define DECL_EXCEPTION_REFCOUNT(NODE) (LANG_DECL_MIN_CHECK (NODE)->access)

/* Nonzero if the FUNCTION_DECL is a global constructor.  */
#define DECL_GLOBAL_CTOR_P(NODE) \
  (LANG_DECL_FN_CHECK (NODE)->global_ctor_p)

/* Nonzero if the FUNCTION_DECL is a global destructor.  */
#define DECL_GLOBAL_DTOR_P(NODE) \
  (LANG_DECL_FN_CHECK (NODE)->global_dtor_p)

/* Accessor macros for C++ template decl nodes.  */

/* The DECL_TEMPLATE_PARMS are a list.  The TREE_PURPOSE of each node
   is a INT_CST whose TREE_INT_CST_LOW indicates the level of the
   template parameters, with 1 being the outermost set of template
   parameters.  The TREE_TYPE is TEMPLATE_PARMS_CONSTRAINTS.
   The TREE_VALUE is a vector, whose elements are the
   template parameters at each level.  Each element in the vector is a
   TREE_LIST, whose TREE_VALUE is a PARM_DECL (if the parameter is a
   non-type parameter), or a TYPE_DECL (if the parameter is a type
   parameter) or a TEMPLATE_DECL (if the parameter is a template
   parameter).  The TREE_PURPOSE is the default value, if any.
   The TREE_TYPE is TEMPLATE_PARM_CONSTRAINTS.  The
   TEMPLATE_PARM_INDEX for the parameter is available as the
   DECL_INITIAL (for a PARM_DECL) or as the TREE_TYPE (for a
   TYPE_DECL).

   FIXME: CONST_CAST_TREE is a hack that hopefully will go away after
   tree is converted to C++ class hiearchy.  */
#define DECL_TEMPLATE_PARMS(NODE)       \
   ((struct tree_template_decl *)CONST_CAST_TREE (TEMPLATE_DECL_CHECK (NODE)))->arguments
#define DECL_INNERMOST_TEMPLATE_PARMS(NODE) \
   INNERMOST_TEMPLATE_PARMS (DECL_TEMPLATE_PARMS (NODE))
#define DECL_NTPARMS(NODE) \
   TREE_VEC_LENGTH (DECL_INNERMOST_TEMPLATE_PARMS (NODE))
/* For function, method, class-data templates.

   FIXME: CONST_CAST_TREE is a hack that hopefully will go away after
   tree is converted to C++ class hiearchy.  */
#define DECL_TEMPLATE_RESULT(NODE)      \
   ((struct tree_template_decl *)CONST_CAST_TREE(TEMPLATE_DECL_CHECK (NODE)))->result
/* For a forward-declared function template at namespace scope, or for any
   function template in an exporting module, DECL_TEMPLATE_INSTANTIATIONS lists
   all instantiations and specializations of the function so that
   tsubst_friend_function can reassign them to another template if we find that
   the namespace-scope template is really a partial instantiation of a friend
   template.

   For a class or variable template the DECL_TEMPLATE_INSTANTIATIONS lists
   holds all instantiations and specializations, including partial
   instantiations and partial specializations, so that if we explicitly
   specialize a partial instantiation we can walk the list in
   maybe_process_partial_specialization and reassign them or complain as
   appropriate.

   In both cases, the TREE_PURPOSE of each node contains the arguments
   used; the TREE_VALUE contains the generated variable.  The template
   arguments are always complete.  For example, given:

      template <class T> struct S1 {
	template <class U> struct S2 {};
	template <class U> struct S2<U*> {};
      };

   the record for the partial specialization will contain, as its
   argument list, { {T}, {U*} }, and will be on the
   DECL_TEMPLATE_INSTANTIATIONS list for `template <class T> template
   <class U> struct S1<T>::S2'.

   This list is not used for other templates.  */
#define DECL_TEMPLATE_INSTANTIATIONS(NODE) \
  DECL_SIZE_UNIT (TEMPLATE_DECL_CHECK (NODE))

/* For a class template, this list contains the partial
   specializations of this template.  (Full specializations are not
   recorded on this list.)  The TREE_PURPOSE holds the arguments used
   in the partial specialization (e.g., for `template <class T> struct
   S<T*, int>' this will be `T*, int'.)  The arguments will also include
   any outer template arguments.  The TREE_VALUE holds the TEMPLATE_DECL
   for the partial specialization.  The TREE_TYPE is the _TYPE node for
   the partial specialization.

   This list is not used for other templates.  */
#define DECL_TEMPLATE_SPECIALIZATIONS(NODE)     \
  DECL_SIZE (TEMPLATE_DECL_CHECK (NODE))

/* Nonzero for a DECL which is actually a template parameter.  Keep
   these checks in ascending tree code order.   */
#define DECL_TEMPLATE_PARM_P(NODE)		\
  (DECL_LANG_FLAG_0 (NODE)			\
   && (TREE_CODE (NODE) == CONST_DECL		\
       || TREE_CODE (NODE) == PARM_DECL		\
       || TREE_CODE (NODE) == TYPE_DECL		\
       || TREE_CODE (NODE) == TEMPLATE_DECL))

#if ENABLE_TREE_CHECKING
inline tree
decl_template_parm_check (const_tree t, const char *f, int l, const char *fn)
{
  if (!DECL_TEMPLATE_PARM_P (t))
    tree_check_failed (t, f, l, fn, 0);
  return const_cast<tree>(t);
}
#endif

/* Nonzero for a raw template parameter node.  */
#define TEMPLATE_PARM_P(NODE)					\
  (TREE_CODE (NODE) == TEMPLATE_TYPE_PARM			\
   || TREE_CODE (NODE) == TEMPLATE_TEMPLATE_PARM		\
   || TREE_CODE (NODE) == TEMPLATE_PARM_INDEX)

/* Mark NODE as a template parameter.  */
#define SET_DECL_TEMPLATE_PARM_P(NODE) \
  (DECL_LANG_FLAG_0 (NODE) = 1)

/* Nonzero if NODE is a template template parameter.  */
#define DECL_TEMPLATE_TEMPLATE_PARM_P(NODE) \
  (TREE_CODE (NODE) == TEMPLATE_DECL && DECL_TEMPLATE_PARM_P (NODE))

/* Nonzero for a DECL that represents a function template.  */
#define DECL_FUNCTION_TEMPLATE_P(NODE)                          \
  (TREE_CODE (NODE) == TEMPLATE_DECL                            \
   && DECL_TEMPLATE_RESULT (NODE) != NULL_TREE			\
   && TREE_CODE (DECL_TEMPLATE_RESULT (NODE)) == FUNCTION_DECL)

/* Nonzero for a DECL that represents a class template or alias
   template.  */
#define DECL_TYPE_TEMPLATE_P(NODE)				\
  (TREE_CODE (NODE) == TEMPLATE_DECL				\
   && DECL_TEMPLATE_RESULT (NODE) != NULL_TREE			\
   && TREE_CODE (DECL_TEMPLATE_RESULT (NODE)) == TYPE_DECL)

/* Nonzero for a DECL that represents a class template.  */
#define DECL_CLASS_TEMPLATE_P(NODE)				\
  (DECL_TYPE_TEMPLATE_P (NODE)					\
   && DECL_IMPLICIT_TYPEDEF_P (DECL_TEMPLATE_RESULT (NODE)))

/* Nonzero for a TEMPLATE_DECL that represents an alias template.  */
#define DECL_ALIAS_TEMPLATE_P(NODE)			\
  (DECL_TYPE_TEMPLATE_P (NODE)				\
   && !DECL_ARTIFICIAL (DECL_TEMPLATE_RESULT (NODE)))

/* Nonzero for a NODE which declares a type.  */
#define DECL_DECLARES_TYPE_P(NODE) \
  (TREE_CODE (NODE) == TYPE_DECL || DECL_TYPE_TEMPLATE_P (NODE))

/* Nonzero if NODE declares a function.  */
#define DECL_DECLARES_FUNCTION_P(NODE) \
  (TREE_CODE (NODE) == FUNCTION_DECL || DECL_FUNCTION_TEMPLATE_P (NODE))

/* Nonzero if NODE is the typedef implicitly generated for a type when
   the type is declared.  In C++, `struct S {};' is roughly
   equivalent to `struct S {}; typedef struct S S;' in C.
   DECL_IMPLICIT_TYPEDEF_P will hold for the typedef indicated in this
   example.  In C++, there is a second implicit typedef for each
   class, called the injected-class-name, in the scope of `S' itself, so that
   you can say `S::S'.  DECL_SELF_REFERENCE_P will hold for that typedef.  */
#define DECL_IMPLICIT_TYPEDEF_P(NODE) \
  (TREE_CODE (NODE) == TYPE_DECL && DECL_LANG_FLAG_2 (NODE))
#define SET_DECL_IMPLICIT_TYPEDEF_P(NODE) \
  (DECL_LANG_FLAG_2 (NODE) = 1)
#define DECL_SELF_REFERENCE_P(NODE) \
  (TREE_CODE (NODE) == TYPE_DECL && DECL_LANG_FLAG_4 (NODE))
#define SET_DECL_SELF_REFERENCE_P(NODE) \
  (DECL_LANG_FLAG_4 (NODE) = 1)

/* A `primary' template is one that has its own template header and is not
   a partial specialization.  A member function of a class template is a
   template, but not primary.  A member template is primary.  Friend
   templates are primary, too.  */

/* Returns the primary template corresponding to these parameters.  */
#define TPARMS_PRIMARY_TEMPLATE(NODE) (TREE_TYPE (TREE_VEC_CHECK (NODE)))

#define DECL_PRIMARY_TEMPLATE(NODE) \
  (TPARMS_PRIMARY_TEMPLATE (DECL_INNERMOST_TEMPLATE_PARMS (NODE)))

/* Returns nonzero if NODE is a primary template.  */
#define PRIMARY_TEMPLATE_P(NODE) (DECL_PRIMARY_TEMPLATE (NODE) == (NODE))

/* Nonzero iff NODE is a specialization of a template.  The value
   indicates the type of specializations:

     1=implicit instantiation

     2=partial or explicit specialization, e.g.:

        template <> int min<int> (int, int),

     3=explicit instantiation, e.g.:

        template int min<int> (int, int);

   Note that NODE will be marked as a specialization even if the
   template it is instantiating is not a primary template.  For
   example, given:

     template <typename T> struct O {
       void f();
       struct I {};
     };

   both O<int>::f and O<int>::I will be marked as instantiations.

   If DECL_USE_TEMPLATE is nonzero, then DECL_TEMPLATE_INFO will also
   be non-NULL.  */
#define DECL_USE_TEMPLATE(NODE) (DECL_LANG_SPECIFIC (NODE)->u.base.use_template)

/* Like DECL_USE_TEMPLATE, but for class types.  */
#define CLASSTYPE_USE_TEMPLATE(NODE) \
  (LANG_TYPE_CLASS_CHECK (NODE)->use_template)

/* True if NODE is a specialization of a primary template.  */
#define CLASSTYPE_SPECIALIZATION_OF_PRIMARY_TEMPLATE_P(NODE)	\
  (CLASS_TYPE_P (NODE)						\
   && CLASSTYPE_USE_TEMPLATE (NODE)				\
   && PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (NODE)))

#define DECL_TEMPLATE_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) & 1)
#define CLASSTYPE_TEMPLATE_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) & 1)

#define DECL_TEMPLATE_SPECIALIZATION(NODE) (DECL_USE_TEMPLATE (NODE) == 2)
#define SET_DECL_TEMPLATE_SPECIALIZATION(NODE) (DECL_USE_TEMPLATE (NODE) = 2)

/* Returns true for an explicit or partial specialization of a class
   template.  */
#define CLASSTYPE_TEMPLATE_SPECIALIZATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) == 2)
#define SET_CLASSTYPE_TEMPLATE_SPECIALIZATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) = 2)

#define DECL_IMPLICIT_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) == 1)
#define SET_DECL_IMPLICIT_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) = 1)
#define CLASSTYPE_IMPLICIT_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) == 1)
#define SET_CLASSTYPE_IMPLICIT_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) = 1)

#define DECL_EXPLICIT_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) == 3)
#define SET_DECL_EXPLICIT_INSTANTIATION(NODE) (DECL_USE_TEMPLATE (NODE) = 3)
#define CLASSTYPE_EXPLICIT_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) == 3)
#define SET_CLASSTYPE_EXPLICIT_INSTANTIATION(NODE) \
  (CLASSTYPE_USE_TEMPLATE (NODE) = 3)

/* Nonzero if DECL is a friend function which is an instantiation
   from the point of view of the compiler, but not from the point of
   view of the language.  For example given:
      template <class T> struct S { friend void f(T) {}; };
   the declaration of `void f(int)' generated when S<int> is
   instantiated will not be a DECL_TEMPLATE_INSTANTIATION, but will be
   a DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION.  */
#define DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION(DECL) \
  (DECL_LANG_SPECIFIC (DECL) && DECL_TEMPLATE_INFO (DECL) \
   && !DECL_USE_TEMPLATE (DECL))

/* Nonzero if DECL is a function generated from a function 'temploid',
   i.e. template, member of class template, or dependent friend.  */
#define DECL_TEMPLOID_INSTANTIATION(DECL)		\
  (DECL_TEMPLATE_INSTANTIATION (DECL)			\
   || DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION (DECL))

/* Nonzero if DECL is either defined implicitly by the compiler or
   generated from a temploid.  */
#define DECL_GENERATED_P(DECL) \
  (DECL_TEMPLOID_INSTANTIATION (DECL) || DECL_DEFAULTED_FN (DECL))

/* Nonzero iff we are currently processing a declaration for an
   entity with its own template parameter list, and which is not a
   full specialization.  */
#define PROCESSING_REAL_TEMPLATE_DECL_P() \
  (!processing_template_parmlist \
   && current_template_depth > template_class_depth (current_scope ()))

/* Nonzero if this VAR_DECL or FUNCTION_DECL has already been
   instantiated, i.e. its definition has been generated from the
   pattern given in the template.  */
#define DECL_TEMPLATE_INSTANTIATED(NODE) \
  DECL_LANG_FLAG_1 (VAR_OR_FUNCTION_DECL_CHECK (NODE))

/* We know what we're doing with this decl now.  */
#define DECL_INTERFACE_KNOWN(NODE) DECL_LANG_FLAG_5 (NODE)

/* DECL_EXTERNAL must be set on a decl until the decl is actually emitted,
   so that assemble_external will work properly.  So we have this flag to
   tell us whether the decl is really not external.

   This flag does not indicate whether or not the decl is defined in the
   current translation unit; it indicates whether or not we should emit the
   decl at the end of compilation if it is defined and needed.  */
#define DECL_NOT_REALLY_EXTERN(NODE) \
  (DECL_LANG_SPECIFIC (NODE)->u.base.not_really_extern)

#define DECL_REALLY_EXTERN(NODE) \
  (DECL_EXTERNAL (NODE)				\
   && (!DECL_LANG_SPECIFIC (NODE) || !DECL_NOT_REALLY_EXTERN (NODE)))

/* A thunk is a stub function.

   A thunk is an alternate entry point for an ordinary FUNCTION_DECL.
   The address of the ordinary FUNCTION_DECL is given by the
   DECL_INITIAL, which is always an ADDR_EXPR whose operand is a
   FUNCTION_DECL.  The job of the thunk is to either adjust the this
   pointer before transferring control to the FUNCTION_DECL, or call
   FUNCTION_DECL and then adjust the result value. Note, the result
   pointer adjusting thunk must perform a call to the thunked
   function, (or be implemented via passing some invisible parameter
   to the thunked function, which is modified to perform the
   adjustment just before returning).

   A thunk may perform either, or both, of the following operations:

   o Adjust the this or result pointer by a constant offset.
   o Adjust the this or result pointer by looking up a vcall or vbase offset
     in the vtable.

   A this pointer adjusting thunk converts from a base to a derived
   class, and hence adds the offsets. A result pointer adjusting thunk
   converts from a derived class to a base, and hence subtracts the
   offsets.  If both operations are performed, then the constant
   adjustment is performed first for this pointer adjustment and last
   for the result pointer adjustment.

   The constant adjustment is given by THUNK_FIXED_OFFSET.  If the
   vcall or vbase offset is required, THUNK_VIRTUAL_OFFSET is
   used. For this pointer adjusting thunks, it is the vcall offset
   into the vtable.  For result pointer adjusting thunks it is the
   binfo of the virtual base to convert to.  Use that binfo's vbase
   offset.

   It is possible to have equivalent covariant thunks.  These are
   distinct virtual covariant thunks whose vbase offsets happen to
   have the same value.  THUNK_ALIAS is used to pick one as the
   canonical thunk, which will get all the this pointer adjusting
   thunks attached to it.  */

/* An integer indicating how many bytes should be subtracted from the
   this or result pointer when this function is called.  */
#define THUNK_FIXED_OFFSET(DECL) \
  (DECL_LANG_SPECIFIC (THUNK_FUNCTION_CHECK (DECL))->u.fn.u5.fixed_offset)

/* A tree indicating how to perform the virtual adjustment. For a this
   adjusting thunk it is the number of bytes to be added to the vtable
   to find the vcall offset. For a result adjusting thunk, it is the
   binfo of the relevant virtual base.  If NULL, then there is no
   virtual adjust.  (The vptr is always located at offset zero from
   the this or result pointer.)  (If the covariant type is within the
   class hierarchy being laid out, the vbase index is not yet known
   at the point we need to create the thunks, hence the need to use
   binfos.)  */

#define THUNK_VIRTUAL_OFFSET(DECL) \
  (LANG_DECL_MIN_CHECK (FUNCTION_DECL_CHECK (DECL))->access)

/* A thunk which is equivalent to another thunk.  */
#define THUNK_ALIAS(DECL) \
  (DECL_LANG_SPECIFIC (FUNCTION_DECL_CHECK (DECL))->u.min.template_info)

/* For thunk NODE, this is the FUNCTION_DECL thunked to.  It is
   possible for the target to be a thunk too.  */
#define THUNK_TARGET(NODE)				\
  (LANG_DECL_FN_CHECK (NODE)->befriending_classes)

/* True for a SCOPE_REF iff the "template" keyword was used to
   indicate that the qualified name denotes a template.  */
#define QUALIFIED_NAME_IS_TEMPLATE(NODE) \
  (TREE_LANG_FLAG_1 (SCOPE_REF_CHECK (NODE)))

/* [coroutines]
*/

/* True if NODE is a co-routine FUNCTION_DECL.  */
#define DECL_COROUTINE_P(NODE) \
  (LANG_DECL_FN_CHECK (DECL_COMMON_CHECK (NODE))->coroutine_p)

/* For a FUNCTION_DECL of a coroutine, this holds the ACTOR helper function
   decl.  */
#define DECL_ACTOR_FN(NODE) \
  (coro_get_actor_function ((NODE)))

/* For a FUNCTION_DECL of a coroutine, this holds the DESTROY helper function
  decl.  */
#define DECL_DESTROY_FN(NODE) \
  (coro_get_destroy_function ((NODE)))

/* For a FUNCTION_DECL of a coroutine helper (ACTOR or DESTROY), this points
   back to the original (ramp) function.  */
#define DECL_RAMP_FN(NODE) \
  (coro_get_ramp_function (NODE))

/* For a FUNCTION_DECL this is true if it is a coroutine ramp.  */
#define DECL_RAMP_P(NODE) \
  DECL_COROUTINE_P (NODE) && !DECL_RAMP_FN (NODE)

/* True for an OMP_ATOMIC that has dependent parameters.  These are stored
   as an expr in operand 1, and integer_zero_node or clauses in operand 0.  */
#define OMP_ATOMIC_DEPENDENT_P(NODE) \
  (TREE_CODE (TREE_OPERAND (OMP_ATOMIC_CHECK (NODE), 0)) == INTEGER_CST \
   || TREE_CODE (TREE_OPERAND (OMP_ATOMIC_CHECK (NODE), 0)) == OMP_CLAUSE)

/* Used while gimplifying continue statements bound to OMP_FOR nodes.  */
#define OMP_FOR_GIMPLIFYING_P(NODE) \
  (TREE_LANG_FLAG_0 (OMP_LOOPING_CHECK (NODE)))

/* A language-specific token attached to the OpenMP data clauses to
   hold code (or code fragments) related to ctors, dtors, and op=.
   See semantics.cc for details.  */
#define CP_OMP_CLAUSE_INFO(NODE) \
  TREE_TYPE (OMP_CLAUSE_RANGE_CHECK (NODE, OMP_CLAUSE_PRIVATE, \
				     OMP_CLAUSE__CONDTEMP_))

/* Nonzero if this transaction expression's body contains statements.  */
#define TRANSACTION_EXPR_IS_STMT(NODE) \
   TREE_LANG_FLAG_0 (TRANSACTION_EXPR_CHECK (NODE))

/* These macros provide convenient access to the various _STMT nodes
   created when parsing template declarations.  */
#define TRY_STMTS(NODE)		TREE_OPERAND (TRY_BLOCK_CHECK (NODE), 0)
#define TRY_HANDLERS(NODE)	TREE_OPERAND (TRY_BLOCK_CHECK (NODE), 1)

#define EH_SPEC_STMTS(NODE)	TREE_OPERAND (EH_SPEC_BLOCK_CHECK (NODE), 0)
#define EH_SPEC_RAISES(NODE)	TREE_OPERAND (EH_SPEC_BLOCK_CHECK (NODE), 1)

#define USING_STMT_NAMESPACE(NODE) TREE_OPERAND (USING_STMT_CHECK (NODE), 0)

/* Nonzero if this try block is a function try block.  */
#define FN_TRY_BLOCK_P(NODE)	TREE_LANG_FLAG_3 (TRY_BLOCK_CHECK (NODE))
#define HANDLER_PARMS(NODE)	TREE_OPERAND (HANDLER_CHECK (NODE), 0)
#define HANDLER_BODY(NODE)	TREE_OPERAND (HANDLER_CHECK (NODE), 1)
#define HANDLER_TYPE(NODE)	TREE_TYPE (HANDLER_CHECK (NODE))

/* CLEANUP_STMT accessors.  The statement(s) covered, the cleanup to run
   and the VAR_DECL for which this cleanup exists.  */
#define CLEANUP_BODY(NODE)	TREE_OPERAND (CLEANUP_STMT_CHECK (NODE), 0)
#define CLEANUP_EXPR(NODE)	TREE_OPERAND (CLEANUP_STMT_CHECK (NODE), 1)
#define CLEANUP_DECL(NODE)	TREE_OPERAND (CLEANUP_STMT_CHECK (NODE), 2)

/* IF_STMT accessors. These give access to the condition of the if
   statement, the then block of the if statement, and the else block
   of the if statement if it exists.  */
#define IF_COND(NODE)		TREE_OPERAND (IF_STMT_CHECK (NODE), 0)
#define THEN_CLAUSE(NODE)	TREE_OPERAND (IF_STMT_CHECK (NODE), 1)
#define ELSE_CLAUSE(NODE)	TREE_OPERAND (IF_STMT_CHECK (NODE), 2)
#define IF_SCOPE(NODE)		TREE_OPERAND (IF_STMT_CHECK (NODE), 3)
#define IF_STMT_CONSTEXPR_P(NODE) TREE_LANG_FLAG_0 (IF_STMT_CHECK (NODE))
#define IF_STMT_CONSTEVAL_P(NODE) TREE_LANG_FLAG_2 (IF_STMT_CHECK (NODE))

/* Like PACK_EXPANSION_EXTRA_ARGS, for constexpr if.  IF_SCOPE is used while
   building an IF_STMT; IF_STMT_EXTRA_ARGS is used after it is complete.  */
#define IF_STMT_EXTRA_ARGS(NODE) IF_SCOPE (NODE)

/* RANGE_FOR_STMT accessors. These give access to the declarator,
   expression, body, and scope of the statement, respectively.  */
#define RANGE_FOR_DECL(NODE)	TREE_OPERAND (RANGE_FOR_STMT_CHECK (NODE), 0)
#define RANGE_FOR_EXPR(NODE)	TREE_OPERAND (RANGE_FOR_STMT_CHECK (NODE), 1)
#define RANGE_FOR_BODY(NODE)	TREE_OPERAND (RANGE_FOR_STMT_CHECK (NODE), 2)
#define RANGE_FOR_SCOPE(NODE)	TREE_OPERAND (RANGE_FOR_STMT_CHECK (NODE), 3)
#define RANGE_FOR_UNROLL(NODE)	TREE_OPERAND (RANGE_FOR_STMT_CHECK (NODE), 4)
#define RANGE_FOR_INIT_STMT(NODE) TREE_OPERAND (RANGE_FOR_STMT_CHECK (NODE), 5)
#define RANGE_FOR_IVDEP(NODE)	TREE_LANG_FLAG_6 (RANGE_FOR_STMT_CHECK (NODE))
#define RANGE_FOR_NOVECTOR(NODE) TREE_LANG_FLAG_5 (RANGE_FOR_STMT_CHECK (NODE))

/* STMT_EXPR accessor.  */
#define STMT_EXPR_STMT(NODE)	TREE_OPERAND (STMT_EXPR_CHECK (NODE), 0)

/* EXPR_STMT accessor. This gives the expression associated with an
   expression statement.  */
#define EXPR_STMT_EXPR(NODE)	TREE_OPERAND (EXPR_STMT_CHECK (NODE), 0)

/* True if this TARGET_EXPR was created by build_cplus_new, and so we can
   discard it if it isn't useful.  */
#define TARGET_EXPR_IMPLICIT_P(NODE) \
  TREE_LANG_FLAG_0 (TARGET_EXPR_CHECK (NODE))

/* True if this TARGET_EXPR is the result of list-initialization of a
   temporary.  */
#define TARGET_EXPR_LIST_INIT_P(NODE) \
  TREE_LANG_FLAG_1 (TARGET_EXPR_CHECK (NODE))

/* True if this TARGET_EXPR expresses direct-initialization of an object
   to be named later.  */
#define TARGET_EXPR_DIRECT_INIT_P(NODE) \
  TREE_LANG_FLAG_2 (TARGET_EXPR_CHECK (NODE))

/* True if we expect this TARGET_EXPR to be used as an initializer, not to
   materialize as a temporary.  */
#define TARGET_EXPR_ELIDING_P(NODE) \
  TREE_LANG_FLAG_3 (TARGET_EXPR_CHECK (NODE))

/* True if this TARGET_EXPR is for holding an implementation detail like a
   cleanup flag or loop index, and should be ignored by extend_all_temps.  */
#define TARGET_EXPR_INTERNAL_P(NODE) \
  TREE_LANG_FLAG_4 (TARGET_EXPR_CHECK (NODE))

/* True if NODE is a TARGET_EXPR that just expresses a copy of its INITIAL; if
   the initializer has void type, it's doing something more complicated.  */
#define SIMPLE_TARGET_EXPR_P(NODE)				\
  (TREE_CODE (NODE) == TARGET_EXPR				\
   && TARGET_EXPR_INITIAL (NODE)				\
   && !VOID_TYPE_P (TREE_TYPE (TARGET_EXPR_INITIAL (NODE))))

/* True if T is a TARGET_EXPR for which we'll need to replace_decl to use it as
   an initializer.  */
inline bool
target_expr_needs_replace (tree t)
{
  if (!t || TREE_CODE (t) != TARGET_EXPR)
    return false;
  tree init = TARGET_EXPR_INITIAL (t);
  if (!init || !VOID_TYPE_P (TREE_TYPE (init)))
    return false;
  while (TREE_CODE (init) == COMPOUND_EXPR)
    init = TREE_OPERAND (init, 1);
  return (TREE_CODE (init) != AGGR_INIT_EXPR
	  && TREE_CODE (init) != VEC_INIT_EXPR);
}

/* True if EXPR expresses direct-initialization of a TYPE.  */
#define DIRECT_INIT_EXPR_P(TYPE,EXPR)					\
  (TREE_CODE (EXPR) == TARGET_EXPR && TREE_LANG_FLAG_2 (EXPR)		\
   && same_type_ignoring_top_level_qualifiers_p (TYPE, TREE_TYPE (EXPR)))

/* True if this CONVERT_EXPR is for a conversion to virtual base in
   an NSDMI, and should be re-evaluated when used in a constructor.  */
#define CONVERT_EXPR_VBASE_PATH(NODE) \
  TREE_LANG_FLAG_0 (CONVERT_EXPR_CHECK (NODE))

/* True if SIZEOF_EXPR argument is type.  */
#define SIZEOF_EXPR_TYPE_P(NODE) \
  TREE_LANG_FLAG_0 (SIZEOF_EXPR_CHECK (NODE))

/* True if the ALIGNOF_EXPR was spelled "alignof".  */
#define ALIGNOF_EXPR_STD_P(NODE) \
  TREE_LANG_FLAG_0 (ALIGNOF_EXPR_CHECK (NODE))

/* OMP_DEPOBJ accessors. These give access to the depobj expression of the
   #pragma omp depobj directive and the clauses, respectively.  If
   OMP_DEPOBJ_CLAUSES is INTEGER_CST, it is instead the update clause kind
   or OMP_CLAUSE_DEPEND_LAST for destroy clause.  */
#define OMP_DEPOBJ_DEPOBJ(NODE)	 TREE_OPERAND (OMP_DEPOBJ_CHECK (NODE), 0)
#define OMP_DEPOBJ_CLAUSES(NODE) TREE_OPERAND (OMP_DEPOBJ_CHECK (NODE), 1)

/* An enumeration of the kind of tags that C++ accepts.  */
enum tag_types {
  none_type = 0, /* Not a tag type.  */
  record_type,   /* "struct" types.  */
  class_type,    /* "class" types.  */
  union_type,    /* "union" types.  */
  enum_type,     /* "enum" types.  */
  typename_type, /* "typename" types.  */
  scope_type	 /* namespace or tagged type name followed by :: */
};

/* The various kinds of lvalues we distinguish.  */
enum cp_lvalue_kind_flags {
  clk_none = 0,     /* Things that are not an lvalue.  */
  clk_ordinary = 1, /* An ordinary lvalue.  */
  clk_rvalueref = 2,/* An xvalue (rvalue formed using an rvalue reference) */
  clk_class = 4,    /* A prvalue of class or array type.  */
  clk_bitfield = 8, /* An lvalue for a bit-field.  */
  clk_packed = 16,  /* An lvalue for a packed field.  */
  clk_implicit_rval = 1<<5, /* An lvalue being treated as an xvalue.  */
  clk_mergeable = 1<<6
};

/* This type is used for parameters and variables which hold
   combinations of the flags in enum cp_lvalue_kind_flags.  */
typedef int cp_lvalue_kind;

/* Various kinds of template specialization, instantiation, etc.  */
enum tmpl_spec_kind {
  tsk_none,		   /* Not a template at all.  */
  tsk_invalid_member_spec, /* An explicit member template
			      specialization, but the enclosing
			      classes have not all been explicitly
			      specialized.  */
  tsk_invalid_expl_inst,   /* An explicit instantiation containing
			      template parameter lists.  */
  tsk_excessive_parms,	   /* A template declaration with too many
			      template parameter lists.  */
  tsk_insufficient_parms,  /* A template declaration with too few
			      parameter lists.  */
  tsk_template,		   /* A template declaration.  */
  tsk_expl_spec,	   /* An explicit specialization.  */
  tsk_expl_inst		   /* An explicit instantiation.  */
};

/* The various kinds of access.  BINFO_ACCESS depends on these being
   two bit quantities.  The numerical values are important; they are
   used to initialize RTTI data structures, so changing them changes
   the ABI.  */
enum access_kind {
  ak_none = 0,		   /* Inaccessible.  */
  ak_public = 1,	   /* Accessible, as a `public' thing.  */
  ak_protected = 2,	   /* Accessible, as a `protected' thing.  */
  ak_private = 3	   /* Accessible, as a `private' thing.  */
};

/* The various kinds of special functions.  If you add to this list,
   you should update special_function_p as well.  */
enum special_function_kind {
  sfk_none = 0,		   /* Not a special function.  This enumeral
			      must have value zero; see
			      special_function_p.  */
  /* The following are ordered, for use by member synthesis fns.  */
  sfk_destructor,	   /* A destructor.  */
  sfk_constructor,	   /* A constructor.  */
  sfk_inheriting_constructor, /* An inheriting constructor */
  sfk_copy_constructor,    /* A copy constructor.  */
  sfk_move_constructor,    /* A move constructor.  */
  sfk_copy_assignment,     /* A copy assignment operator.  */
  sfk_move_assignment,     /* A move assignment operator.  */
  /* The following are unordered.  */
  sfk_complete_destructor, /* A destructor for complete objects.  */
  sfk_base_destructor,     /* A destructor for base subobjects.  */
  sfk_deleting_destructor, /* A destructor for complete objects that
			      deletes the object after it has been
			      destroyed.  */
  sfk_conversion,	   /* A conversion operator.  */
  sfk_deduction_guide,	   /* A class template deduction guide.  */
  sfk_comparison,	   /* A comparison operator (e.g. ==, <, <=>).  */
  sfk_virtual_destructor   /* Used by member synthesis fns.  */
};

/* The various kinds of linkage.  From [basic.link],

      A name is said to have linkage when it might denote the same
      object, reference, function, type, template, namespace or value
      as a name introduced in another scope:

      -- When a name has external linkage, the entity it denotes can
	 be referred to from scopes of other translation units or from
	 other scopes of the same translation unit.

      -- When a name has internal linkage, the entity it denotes can
	 be referred to by names from other scopes in the same
	 translation unit.

      -- When a name has no linkage, the entity it denotes cannot be
	 referred to by names from other scopes.  */

enum linkage_kind {
  lk_none,			/* No linkage.  */
  lk_internal,			/* Internal linkage.  */
  lk_external			/* External linkage.  */
};

enum duration_kind {
  dk_static,
  dk_thread,
  dk_auto,
  dk_dynamic
};

/* Bitmask flags to control type substitution.  */
enum tsubst_flags {
  tf_none = 0,			 /* nothing special */
  tf_error = 1 << 0,		 /* give error messages  */
  tf_warning = 1 << 1,	 	 /* give warnings too  */
  tf_ignore_bad_quals = 1 << 2,	 /* ignore bad cvr qualifiers */
  tf_keep_type_decl = 1 << 3,	 /* retain typedef type decls
				    (make_typename_type use) */
  tf_ptrmem_ok = 1 << 4,	 /* pointers to member ok (internal
				    instantiate_type use) */
  tf_user = 1 << 5,		 /* found template must be a user template
				    (lookup_template_class use) */
  tf_conv = 1 << 6,		 /* We are determining what kind of
				    conversion might be permissible,
				    not actually performing the
				    conversion.  */
  tf_decltype = 1 << 7,          /* We are the operand of decltype.
				    Used to implement the special rules
				    for calls in decltype (5.2.2/11).  */
  tf_partial = 1 << 8,		 /* Doing initial explicit argument
				    substitution in fn_type_unification.  */
  tf_fndecl_type = 1 << 9,   /* Substituting the type of a function
				declaration.  */
  tf_no_cleanup = 1 << 10,   /* Do not build a cleanup
				(build_target_expr and friends) */
  /* 1 << 11 is available.  */
  tf_tst_ok = 1 << 12,		 /* Allow a typename-specifier to name
				    a template (C++17 or later).  */
  tf_dguide = 1 << 13,		/* Building a deduction guide from a ctor.  */
  tf_qualifying_scope = 1 << 14, /* Substituting the LHS of the :: operator.
				    Affects TYPENAME_TYPE resolution from
				    make_typename_type.  */
  tf_no_name_lookup = 1 << 15, /* Don't look up the terminal name of an
				  outermost id-expression, or resolve its
				  constituent template-ids or qualified-ids.  */
  /* Convenient substitution flags combinations.  */
  tf_warning_or_error = tf_warning | tf_error
};

/* This type is used for parameters and variables which hold
   combinations of the flags in enum tsubst_flags.  */
typedef int tsubst_flags_t;

/* The kind of checking we can do looking in a class hierarchy.  */
enum base_access_flags {
  ba_any = 0,  /* Do not check access, allow an ambiguous base,
		      prefer a non-virtual base */
  ba_unique = 1 << 0,  /* Must be a unique base.  */
  ba_check_bit = 1 << 1,   /* Check access.  */
  ba_check = ba_unique | ba_check_bit,
  ba_ignore_scope = 1 << 2, /* Ignore access allowed by local scope.  */
  ba_require_virtual = 1 << 3 /* Require a virtual base.  */
};

/* This type is used for parameters and variables which hold
   combinations of the flags in enum base_access_flags.  */
typedef int base_access;

/* The various kinds of access check during parsing.  */
enum deferring_kind {
  dk_no_deferred = 0, /* Check access immediately */
  dk_deferred = 1,    /* Deferred check */
  dk_no_check = 2     /* No access check */
};

/* The kind of base we can find, looking in a class hierarchy.
   Values <0 indicate we failed.  */
enum base_kind {
  bk_inaccessible = -3,   /* The base is inaccessible */
  bk_ambig = -2,	  /* The base is ambiguous */
  bk_not_base = -1,	  /* It is not a base */
  bk_same_type = 0,	  /* It is the same type */
  bk_proper_base = 1,	  /* It is a proper base */
  bk_via_virtual = 2	  /* It is a proper base, but via a virtual
			     path. This might not be the canonical
			     binfo.  */
};

/* Node for "pointer to (virtual) function".
   This may be distinct from ptr_type_node so gdb can distinguish them.  */
#define vfunc_ptr_type_node  vtable_entry_type


/* For building calls to `delete'.  */
extern GTY(()) tree integer_two_node;

/* The number of function bodies which we are currently processing.
   (Zero if we are at namespace scope, one inside the body of a
   function, two inside the body of a function in a local class, etc.)  */
extern int function_depth;

/* Nonzero if we are inside spec_hasher::equal, which affects
   comparison of PARM_DECLs in cp_tree_equal.  */
extern int comparing_specializations;

/* Nonzero if we want different dependent aliases to compare as unequal.
   FIXME we should always do this except during deduction/ordering.  */
extern int comparing_dependent_aliases;

/* Nonzero if we want to consider different member expressions to compare
   equal if they designate the same entity. This is set when comparing
   contract conditions of overrides.  */
extern bool comparing_override_contracts;

/* In parser.cc.  */

/* Nonzero if we are parsing an unevaluated operand: an operand to
   sizeof, typeof, or alignof.  This is a count since operands to
   sizeof can be nested.  */

extern int cp_unevaluated_operand;

/* RAII class used to inhibit the evaluation of operands during parsing
   and template instantiation.  Evaluation warnings are also inhibited.  */

class cp_unevaluated
{
public:
  cp_unevaluated ();
  ~cp_unevaluated ();
};

/* The reverse: an RAII class used for nested contexts that are evaluated even
   if the enclosing context is not.  */

class cp_evaluated
{
public:
  int uneval;
  int inhibit;
  cp_evaluated (bool reset = true)
    : uneval(cp_unevaluated_operand), inhibit(c_inhibit_evaluation_warnings)
  { if (reset)
      cp_unevaluated_operand = c_inhibit_evaluation_warnings = 0; }
  ~cp_evaluated ()
  { cp_unevaluated_operand = uneval;
    c_inhibit_evaluation_warnings = inhibit; }
};

/* in pt.cc  */

/* These values are used for the `STRICT' parameter to type_unification and
   fn_type_unification.  Their meanings are described with the
   documentation for fn_type_unification.  */

enum unification_kind_t {
  DEDUCE_CALL,
  DEDUCE_CONV,
  DEDUCE_EXACT
};

// An RAII class used to create a new pointer map for local
// specializations. When the stack goes out of scope, the
// previous pointer map is restored.
enum lss_policy { lss_blank, lss_copy, lss_nop };
class local_specialization_stack
{
public:
  local_specialization_stack (lss_policy = lss_blank);
  ~local_specialization_stack ();

  hash_map<tree, tree> *saved;
};

/* Entry in the specialization hash table.  */
struct GTY((for_user)) spec_entry
{
  /* The general template this is a specialization of.  */
  tree tmpl;
  /* The args for this (maybe-partial) specialization.  */
  tree args;
  /* The specialization itself.  */
  tree spec = NULL_TREE;
  /* The cached result of hash_tmpl_and_args (tmpl, args).  */
  hashval_t hash = 0;
};

/* in class.cc */

extern int current_class_depth;

/* in decl.cc */

/* An array of static vars & fns.  */
extern GTY(()) vec<tree, va_gc> *static_decls;

/* An array of vtable-needing types that have no key function, or have
   an emitted key function.  */
extern GTY(()) vec<tree, va_gc> *keyed_classes;

/* Here's where we control how name mangling takes place.  */

/* Cannot use '$' up front, because this confuses gdb
   (names beginning with '$' are gdb-local identifiers).

   Note that all forms in which the '$' is significant are long enough
   for direct indexing (meaning that if we know there is a '$'
   at a particular location, we can index into the string at
   any other location that provides distinguishing characters).  */

/* Define NO_DOT_IN_LABEL in your favorite tm file if your assembler
   doesn't allow '.' in symbol names.  */
#ifndef NO_DOT_IN_LABEL

#define JOINER '.'
#define JOIN_STR "."

#define AUTO_TEMP_NAME "_.tmp_"
#define VFIELD_BASE ".vf"
#define VFIELD_NAME "_vptr."
#define VFIELD_NAME_FORMAT "_vptr.%s"

#else /* NO_DOT_IN_LABEL */

#ifndef NO_DOLLAR_IN_LABEL

#define JOINER '$'
#define JOIN_STR "$"

#define AUTO_TEMP_NAME "_$tmp_"
#define VFIELD_BASE "$vf"
#define VFIELD_NAME "_vptr$"
#define VFIELD_NAME_FORMAT "_vptr$%s"

#else /* NO_DOLLAR_IN_LABEL */

#define JOIN_STR "_"

#define VTABLE_NAME "__vt_"
#define VTABLE_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), VTABLE_NAME, \
	     sizeof (VTABLE_NAME) - 1))
#define VFIELD_BASE "__vfb"
#define VFIELD_NAME "__vptr_"
#define VFIELD_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), VFIELD_NAME, \
	    sizeof (VFIELD_NAME) - 1))
#define VFIELD_NAME_FORMAT "__vptr_%s"

#endif	/* NO_DOLLAR_IN_LABEL */
#endif	/* NO_DOT_IN_LABEL */

#define UDLIT_OP_ANSI_PREFIX "operator\"\""
#define UDLIT_OP_ANSI_FORMAT UDLIT_OP_ANSI_PREFIX "%s"
#define UDLIT_OP_MANGLED_PREFIX "li"
#define UDLIT_OP_MANGLED_FORMAT UDLIT_OP_MANGLED_PREFIX "%s"
#define UDLIT_OPER_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), \
             UDLIT_OP_ANSI_PREFIX, \
	     sizeof (UDLIT_OP_ANSI_PREFIX) - 1))
#define UDLIT_OP_SUFFIX(ID_NODE) \
  (IDENTIFIER_POINTER (ID_NODE) + sizeof (UDLIT_OP_ANSI_PREFIX) - 1)

#if !defined(NO_DOLLAR_IN_LABEL) || !defined(NO_DOT_IN_LABEL)

#define VTABLE_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[1] == 'v' \
  && IDENTIFIER_POINTER (ID_NODE)[2] == 't' \
  && IDENTIFIER_POINTER (ID_NODE)[3] == JOINER)

#define VFIELD_NAME_P(ID_NODE) \
  (!strncmp (IDENTIFIER_POINTER (ID_NODE), VFIELD_NAME, sizeof(VFIELD_NAME)-1))

#endif /* !defined(NO_DOLLAR_IN_LABEL) || !defined(NO_DOT_IN_LABEL) */


/* Nonzero if we're done parsing and into end-of-file activities.
   2 if all templates have been instantiated.
   3 if we're done with front-end processing.  */

extern int at_eof;

/* True if note_mangling_alias should enqueue mangling aliases for
   later generation, rather than emitting them right away.  */

extern bool defer_mangling_aliases;

/* True if noexcept is part of the type (i.e. in C++17).  */

extern bool flag_noexcept_type;

/* True if this TREE_LIST in {static,tls}_aggregates is a for dynamic
   initialization of namespace scope structured binding base or related
   extended ref init temps.  Temporaries from the initialization of
   STATIC_INIT_DECOMP_BASE_P dynamic initializers should be destroyed only
   after the last STATIC_INIT_DECOMP_NONBASE_P dynamic initializer following
   it.  */
#define STATIC_INIT_DECOMP_BASE_P(NODE) \
  TREE_LANG_FLAG_1 (TREE_LIST_CHECK (NODE))

/* True if this TREE_LIST in {static,tls}_aggregates is a for dynamic
   initialization of namespace scope structured binding non-base
   variable using get.  */
#define STATIC_INIT_DECOMP_NONBASE_P(NODE) \
  TREE_LANG_FLAG_2 (TREE_LIST_CHECK (NODE))

/* A list of namespace-scope objects which have constructors or
   destructors which reside in the global scope.  The decl is stored
   in the TREE_VALUE slot and the initializer is stored in the
   TREE_PURPOSE slot.  */
extern GTY(()) tree static_aggregates;
/* Likewise, for thread local storage.  */
extern GTY(()) tree tls_aggregates;

/* A hash-map mapping from variable decls to the dynamic initializer for
   the decl.  This is currently only used by OpenMP.  */
extern GTY(()) decl_tree_map *dynamic_initializers;

enum overload_flags { NO_SPECIAL = 0, DTOR_FLAG, TYPENAME_FLAG };

/* These are uses as bits in flags passed to various functions to
   control their behavior.  Despite the LOOKUP_ prefix, many of these
   do not control name lookup.  ??? Functions using these flags should
   probably be modified to accept explicit boolean flags for the
   behaviors relevant to them.  */
/* Check for access violations.  */
#define LOOKUP_PROTECT (1 << 0)
#define LOOKUP_NORMAL (LOOKUP_PROTECT)
/* Even if the function found by lookup is a virtual function, it
   should be called directly.  */
#define LOOKUP_NONVIRTUAL (1 << 1)
/* Non-converting (i.e., "explicit") constructors are not tried.  This flag
   indicates that we are not performing direct-initialization.  */
#define LOOKUP_ONLYCONVERTING (1 << 2)
#define LOOKUP_IMPLICIT (LOOKUP_NORMAL | LOOKUP_ONLYCONVERTING)
/* If a temporary is created, it should be created so that it lives
   as long as the current variable bindings; otherwise it only lives
   until the end of the complete-expression.  It also forces
   direct-initialization in cases where other parts of the compiler
   have already generated a temporary, such as reference
   initialization and the catch parameter.  */
#define DIRECT_BIND (1 << 3)
/* We're performing a user-defined conversion, so more user-defined
   conversions are not permitted (only built-in conversions).  */
#define LOOKUP_NO_CONVERSION (1 << 4)
/* The user has explicitly called a destructor.  (Therefore, we do
   not need to check that the object is non-NULL before calling the
   destructor.)  */
#define LOOKUP_DESTRUCTOR (1 << 5)
/* Do not permit references to bind to temporaries.  */
#define LOOKUP_NO_TEMP_BIND (1 << 6)
/* We're inside an init-list, so narrowing conversions are ill-formed.  */
#define LOOKUP_NO_NARROWING (LOOKUP_NO_TEMP_BIND << 1)
/* We're looking up a constructor for list-initialization.  */
#define LOOKUP_LIST_INIT_CTOR (LOOKUP_NO_NARROWING << 1)
/* This is the first parameter of a copy constructor.  */
#define LOOKUP_COPY_PARM (LOOKUP_LIST_INIT_CTOR << 1)
/* We only want to consider list constructors.  */
#define LOOKUP_LIST_ONLY (LOOKUP_COPY_PARM << 1)
/* Return after determining which function to call and checking access.
   Used by sythesized_method_walk to determine which functions will
   be called to initialize subobjects, in order to determine exception
   specification and possible implicit delete.
   This is kind of a hack, but exiting early avoids problems with trying
   to perform argument conversions when the class isn't complete yet.  */
#define LOOKUP_SPECULATIVE (LOOKUP_LIST_ONLY << 1)
/* Used by calls from defaulted functions to limit the overload set to avoid
   cycles trying to declare them (core issue 1092).  */
#define LOOKUP_DEFAULTED (LOOKUP_SPECULATIVE << 1)
/* Used in calls to store_init_value to suppress its usual call to
   digest_init.  */
#define LOOKUP_ALREADY_DIGESTED (LOOKUP_DEFAULTED << 1)
/* Like LOOKUP_NO_TEMP_BIND, but also prevent binding to xvalues.  */
#define LOOKUP_NO_RVAL_BIND (LOOKUP_ALREADY_DIGESTED << 1)
/* Used by case_conversion to disregard non-integral conversions.  */
#define LOOKUP_NO_NON_INTEGRAL (LOOKUP_NO_RVAL_BIND << 1)
/* Used for delegating constructors in order to diagnose self-delegation.  */
#define LOOKUP_DELEGATING_CONS (LOOKUP_NO_NON_INTEGRAL << 1)
/* Allow initialization of a flexible array members.  */
#define LOOKUP_ALLOW_FLEXARRAY_INIT (LOOKUP_DELEGATING_CONS << 1)
/* We're looking for either a rewritten comparison operator candidate or the
   operator to use on the former's result.  We distinguish between the two by
   knowing that comparisons other than == and <=> must be the latter, as must
   a <=> expression trying to rewrite to <=> without reversing.  */
#define LOOKUP_REWRITTEN (LOOKUP_ALLOW_FLEXARRAY_INIT << 1)
/* Reverse the order of the two arguments for comparison rewriting.  First we
   swap the arguments in add_operator_candidates, then we swap the conversions
   in add_candidate (so that they correspond to the original order of the
   args), then we swap the conversions back in build_new_op_1 (so they
   correspond to the order of the args in the candidate).  */
#define LOOKUP_REVERSED (LOOKUP_REWRITTEN << 1)
/* We're initializing an aggregate from a parenthesized list of values.  */
#define LOOKUP_AGGREGATE_PAREN_INIT (LOOKUP_REVERSED << 1)
/* We're computing conversions as part of a first pass of overload resolution
   wherein we don't try to distinguish an unviable candidate from a
   non-strictly viable candidate and thus can avoid computing unnecessary
   bad conversions.  */
#define LOOKUP_SHORTCUT_BAD_CONVS (LOOKUP_AGGREGATE_PAREN_INIT << 1)

/* These flags are used by the conversion code.
   CONV_IMPLICIT   :  Perform implicit conversions (standard and user-defined).
   CONV_STATIC     :  Perform the explicit conversions for static_cast.
   CONV_CONST      :  Perform the explicit conversions for const_cast.
   CONV_REINTERPRET:  Perform the explicit conversions for reinterpret_cast.
   CONV_PRIVATE    :  Perform upcasts to private bases.
   CONV_FORCE_TEMP :  Require a new temporary when converting to the same
		      aggregate type.  */

#define CONV_IMPLICIT    1
#define CONV_STATIC      2
#define CONV_CONST       4
#define CONV_REINTERPRET 8
#define CONV_PRIVATE	 16
#define CONV_FORCE_TEMP  32
#define CONV_FOLD	 64
#define CONV_OLD_CONVERT (CONV_IMPLICIT | CONV_STATIC | CONV_CONST \
			  | CONV_REINTERPRET)
#define CONV_C_CAST      (CONV_IMPLICIT | CONV_STATIC | CONV_CONST \
			  | CONV_REINTERPRET | CONV_PRIVATE | CONV_FORCE_TEMP)
#define CONV_BACKEND_CONVERT (CONV_OLD_CONVERT | CONV_FOLD)

/* Used by build_expr_type_conversion to indicate which types are
   acceptable as arguments to the expression under consideration.  */

#define WANT_INT	1 /* integer types, including bool */
#define WANT_FLOAT	2 /* floating point types */
#define WANT_ENUM	4 /* enumerated types */
#define WANT_POINTER	8 /* pointer types */
#define WANT_NULL      16 /* null pointer constant */
#define WANT_VECTOR_OR_COMPLEX 32 /* vector or complex types */
#define WANT_ARITH	(WANT_INT | WANT_FLOAT | WANT_VECTOR_OR_COMPLEX)

/* Used with comptypes, and related functions, to guide type
   comparison.  */

#define COMPARE_STRICT	      0 /* Just check if the types are the
				   same.  */
#define COMPARE_BASE	      1 /* Check to see if the second type is
				   derived from the first.  */
#define COMPARE_DERIVED	      2 /* Like COMPARE_BASE, but in
				   reverse.  */
#define COMPARE_REDECLARATION 4 /* The comparison is being done when
				   another declaration of an existing
				   entity is seen.  */
#define COMPARE_STRUCTURAL    8 /* The comparison is intended to be
				   structural. The actual comparison
				   will be identical to
				   COMPARE_STRICT.  */

/* Used with start function.  */
#define SF_DEFAULT	     0  /* No flags.  */
#define SF_PRE_PARSED	     1  /* The function declaration has
				   already been parsed.  */
#define SF_INCLASS_INLINE    2  /* The function is an inline, defined
				   in the class body.  */

/* Used with start_decl's initialized parameter.  */
#define SD_UNINITIALIZED     0
#define SD_INITIALIZED       1
/* Like SD_INITIALIZED, but also mark the new decl as DECL_DECOMPOSITION_P.  */
#define SD_DECOMPOSITION     2
#define SD_DEFAULTED         3
#define SD_DELETED           4

/* Returns nonzero iff TYPE1 and TYPE2 are the same type, or if TYPE2
   is derived from TYPE1, or if TYPE2 is a pointer (reference) to a
   class derived from the type pointed to (referred to) by TYPE1.  */
#define same_or_base_type_p(TYPE1, TYPE2) \
  comptypes ((TYPE1), (TYPE2), COMPARE_BASE)

/* These macros are used to access a TEMPLATE_PARM_INDEX.  */
#define TEMPLATE_PARM_INDEX_CAST(NODE) \
	((template_parm_index*)TEMPLATE_PARM_INDEX_CHECK (NODE))
#define TEMPLATE_PARM_IDX(NODE) (TEMPLATE_PARM_INDEX_CAST (NODE)->index)
#define TEMPLATE_PARM_LEVEL(NODE) (TEMPLATE_PARM_INDEX_CAST (NODE)->level)
#define TEMPLATE_PARM_DESCENDANTS(NODE) (TREE_CHAIN (TEMPLATE_PARM_INDEX_CHECK (NODE)))
#define TEMPLATE_PARM_ORIG_LEVEL(NODE) (TEMPLATE_PARM_INDEX_CAST (NODE)->orig_level)
#define TEMPLATE_PARM_DECL(NODE) (TEMPLATE_PARM_INDEX_CAST (NODE)->decl)
#define TEMPLATE_PARM_PARAMETER_PACK(NODE) \
  (TREE_LANG_FLAG_0 (TEMPLATE_PARM_INDEX_CHECK (NODE)))

/* These macros are for accessing the fields of TEMPLATE_TYPE_PARM,
   TEMPLATE_TEMPLATE_PARM and BOUND_TEMPLATE_TEMPLATE_PARM nodes.  */
#define TEMPLATE_TYPE_PARM_INDEX(NODE)					\
  (TYPE_VALUES_RAW (TREE_CHECK3 ((NODE), TEMPLATE_TYPE_PARM,		\
				 TEMPLATE_TEMPLATE_PARM,		\
				 BOUND_TEMPLATE_TEMPLATE_PARM)))
#define TEMPLATE_TYPE_IDX(NODE) \
  (TEMPLATE_PARM_IDX (TEMPLATE_TYPE_PARM_INDEX (NODE)))
#define TEMPLATE_TYPE_LEVEL(NODE) \
  (TEMPLATE_PARM_LEVEL (TEMPLATE_TYPE_PARM_INDEX (NODE)))
#define TEMPLATE_TYPE_ORIG_LEVEL(NODE) \
  (TEMPLATE_PARM_ORIG_LEVEL (TEMPLATE_TYPE_PARM_INDEX (NODE)))
#define TEMPLATE_TYPE_DESCENDANTS(NODE) \
  (TEMPLATE_PARM_DESCENDANTS (TEMPLATE_TYPE_PARM_INDEX (NODE)))
#define TEMPLATE_TYPE_DECL(NODE) \
  (TEMPLATE_PARM_DECL (TEMPLATE_TYPE_PARM_INDEX (NODE)))
#define TEMPLATE_TYPE_PARAMETER_PACK(NODE) \
  (TEMPLATE_PARM_PARAMETER_PACK (TEMPLATE_TYPE_PARM_INDEX (NODE)))

/* For a C++17 class deduction placeholder, the template it represents.  */
#define CLASS_PLACEHOLDER_TEMPLATE(NODE) \
  (DECL_INITIAL (TYPE_NAME (TEMPLATE_TYPE_PARM_CHECK (NODE))))

/* True iff the template parameters of this TEMPLATE_TEMPLATE_PARM don't
   use any outer template parameters.  */
#define TEMPLATE_TEMPLATE_PARM_SIMPLE_P(NODE) \
  (TYPE_LANG_FLAG_5 (TEMPLATE_TEMPLATE_PARM_CHECK (NODE)))

/* Contexts in which auto deduction occurs. These flags are
   used to control diagnostics in do_auto_deduction.  */

enum auto_deduction_context
{
  adc_unspecified,   /* Not given */
  adc_variable_type, /* Variable initializer deduction */
  adc_return_type,   /* Return type deduction */
  adc_unify,         /* Template argument deduction */
  adc_requirement,   /* Argument deduction constraint */
  adc_decomp_type    /* Decomposition declaration initializer deduction */
};

/* True iff this TEMPLATE_TYPE_PARM represents decltype(auto).  */
#define AUTO_IS_DECLTYPE(NODE) \
  (TYPE_LANG_FLAG_5 (TEMPLATE_TYPE_PARM_CHECK (NODE)))

/* These constants can used as bit flags in the process of tree formatting.

   TFF_PLAIN_IDENTIFIER: unqualified part of a name.
   TFF_SCOPE: include the class and namespace scope of the name.
   TFF_CHASE_TYPEDEF: print the original type-id instead of the typedef-name.
   TFF_DECL_SPECIFIERS: print decl-specifiers.
   TFF_CLASS_KEY_OR_ENUM: precede a class-type name (resp. enum name) with
       a class-key (resp. `enum').
   TFF_RETURN_TYPE: include function return type.
   TFF_FUNCTION_DEFAULT_ARGUMENTS: include function default parameter values.
   TFF_EXCEPTION_SPECIFICATION: show function exception specification.
   TFF_TEMPLATE_HEADER: show the template<...> header in a
       template-declaration.
   TFF_TEMPLATE_NAME: show only template-name.
   TFF_EXPR_IN_PARENS: parenthesize expressions.
   TFF_NO_FUNCTION_ARGUMENTS: don't show function arguments.
   TFF_UNQUALIFIED_NAME: do not print the qualifying scope of the
       top-level entity.
   TFF_NO_OMIT_DEFAULT_TEMPLATE_ARGUMENTS: do not omit template arguments
       identical to their defaults.
   TFF_NO_TEMPLATE_BINDINGS: do not print information about the template
       arguments for a function template specialization.
   TFF_POINTER: we are printing a pointer type.
   TFF_XOBJ_FUNC: we are printing an explicit object member function's
       parameters.  */

#define TFF_PLAIN_IDENTIFIER			(0)
#define TFF_SCOPE				(1)
#define TFF_CHASE_TYPEDEF			(1 << 1)
#define TFF_DECL_SPECIFIERS			(1 << 2)
#define TFF_CLASS_KEY_OR_ENUM			(1 << 3)
#define TFF_RETURN_TYPE				(1 << 4)
#define TFF_FUNCTION_DEFAULT_ARGUMENTS		(1 << 5)
#define TFF_EXCEPTION_SPECIFICATION		(1 << 6)
#define TFF_TEMPLATE_HEADER			(1 << 7)
#define TFF_TEMPLATE_NAME			(1 << 8)
#define TFF_EXPR_IN_PARENS			(1 << 9)
#define TFF_NO_FUNCTION_ARGUMENTS		(1 << 10)
#define TFF_UNQUALIFIED_NAME			(1 << 11)
#define TFF_NO_OMIT_DEFAULT_TEMPLATE_ARGUMENTS	(1 << 12)
#define TFF_NO_TEMPLATE_BINDINGS		(1 << 13)
#define TFF_POINTER		                (1 << 14)
#define TFF_XOBJ_FUNC				(1 << 15)

/* These constants can be used as bit flags to control strip_typedefs.

   STF_USER_VISIBLE: use heuristics to try to avoid stripping user-facing
       aliases of internal details.  This is intended for diagnostics,
       where it should (for example) give more useful "aka" types.

   STF_STRIP_DEPENDENT: allow the stripping of aliases with dependent
       template parameters, relying on code elsewhere to report any
       appropriate diagnostics.

   STF_KEEP_INJ_CLASS_NAME: don't strip injected-class-name typedefs
       because we're dealing with a non-coerced template argument.
*/
const unsigned int STF_USER_VISIBLE = 1U;
const unsigned int STF_STRIP_DEPENDENT = 1U << 1;
const unsigned int STF_KEEP_INJ_CLASS_NAME = 1U << 2;

/* Returns the TEMPLATE_DECL associated to a TEMPLATE_TEMPLATE_PARM
   node.  */
#define TEMPLATE_TEMPLATE_PARM_TEMPLATE_DECL(NODE)	\
  ((TREE_CODE (NODE) == BOUND_TEMPLATE_TEMPLATE_PARM)	\
   ? TYPE_TI_TEMPLATE (NODE)				\
   : TYPE_NAME (TEMPLATE_TEMPLATE_PARM_CHECK (NODE)))

/* in lex.cc  */

extern void init_reswords (void);

/* Various flags for the overloaded operator information.  */
enum ovl_op_flags {
  OVL_OP_FLAG_NONE = 0,	/* Don't care.  */
  OVL_OP_FLAG_UNARY = 1,	/* Is unary.  */
  OVL_OP_FLAG_BINARY = 2,	/* Is binary.  */
  OVL_OP_FLAG_AMBIARY = 3,	/* May be unary or binary.  */
  OVL_OP_FLAG_ALLOC = 4,  	/* operator new or delete.  */
  OVL_OP_FLAG_DELETE = 1,	/* operator delete.  */
  OVL_OP_FLAG_VEC = 2		/* vector new or delete.  */
};

/* Compressed operator codes.  Order is determined by operators.def
   and does not match that of tree_codes.  */
enum ovl_op_code {
  OVL_OP_ERROR_MARK,
  OVL_OP_NOP_EXPR,
#define DEF_OPERATOR(NAME, CODE, MANGLING, FLAGS) OVL_OP_##CODE,
#define DEF_ASSN_OPERATOR(NAME, CODE, MANGLING) /* NOTHING */
#include "operators.def"
  OVL_OP_MAX
};

/* Make sure it fits in lang_decl_fn::ovl_op_code.  */
STATIC_ASSERT (OVL_OP_MAX < (1 << 6));

struct GTY(()) ovl_op_info_t {
  /* The IDENTIFIER_NODE for the operator.  */
  tree identifier;
  /* The name of the operator.  */
  const char *name;
  /* The mangled name of the operator.  */
  const char *mangled_name;
  /* The (regular) tree code.  */
  enum tree_code tree_code : 16;
  /* The (compressed) operator code.  */
  enum ovl_op_code ovl_op_code : 8;
  /* The ovl_op_flags of the operator */
  unsigned flags : 8;
};

/* Overloaded operator info indexed by ass_op_p & ovl_op_code.  */
extern GTY(()) ovl_op_info_t ovl_op_info[2][OVL_OP_MAX];
/* Mapping from tree_codes to ovl_op_codes.  */
extern GTY(()) unsigned char ovl_op_mapping[MAX_TREE_CODES];
/* Mapping for ambi-ary operators from the binary to the unary.  */
extern GTY(()) unsigned char ovl_op_alternate[OVL_OP_MAX];

/* Given an ass_op_p boolean and a tree code, return a pointer to its
   overloaded operator info.  Tree codes for non-overloaded operators
   map to the error-operator.  */
#define OVL_OP_INFO(IS_ASS_P, TREE_CODE)			\
  (&ovl_op_info[(IS_ASS_P) != 0][ovl_op_mapping[(TREE_CODE)]])
/* Overloaded operator info for an identifier for which
   IDENTIFIER_OVL_OP_P is true.  */
#define IDENTIFIER_OVL_OP_INFO(NODE) \
  (&ovl_op_info[IDENTIFIER_KIND_BIT_0 (NODE)][IDENTIFIER_CP_INDEX (NODE)])
#define IDENTIFIER_OVL_OP_FLAGS(NODE) \
  (IDENTIFIER_OVL_OP_INFO (NODE)->flags)

inline tree ovl_op_identifier (bool isass, tree_code code)
{ return OVL_OP_INFO(isass, code)->identifier; }
inline tree ovl_op_identifier (tree_code code) { return ovl_op_identifier (false, code); }
#define assign_op_identifier (ovl_op_info[true][OVL_OP_NOP_EXPR].identifier)
#define call_op_identifier (ovl_op_info[false][OVL_OP_CALL_EXPR].identifier)

/* A type-qualifier, or bitmask therefore, using the TYPE_QUAL
   constants.  */

typedef int cp_cv_quals;

/* Non-static member functions have an optional virt-specifier-seq.
   There is a VIRT_SPEC value for each virt-specifier.
   They can be combined by bitwise-or to form the complete set of
   virt-specifiers for a member function.  */
enum virt_specifier
  {
    VIRT_SPEC_UNSPECIFIED = 0x0,
    VIRT_SPEC_FINAL       = 0x1,
    VIRT_SPEC_OVERRIDE    = 0x2,
    VIRT_SPEC_TRIVIALLY_RELOCATABLE_IF_ELIGIBLE = 0x4,
    VIRT_SPEC_REPLACEABLE_IF_ELIGIBLE = 0x8
  };

/* A type-qualifier, or bitmask therefore, using the VIRT_SPEC
   constants.  */

typedef int cp_virt_specifiers;

/* Wherever there is a function-cv-qual, there could also be a ref-qualifier:

   [dcl.fct]
   The return type, the parameter-type-list, the ref-qualifier, and
   the cv-qualifier-seq, but not the default arguments or the exception
   specification, are part of the function type.

   REF_QUAL_NONE    Ordinary member function with no ref-qualifier
   REF_QUAL_LVALUE  Member function with the &-ref-qualifier
   REF_QUAL_RVALUE  Member function with the &&-ref-qualifier */

enum cp_ref_qualifier {
  REF_QUAL_NONE = 0,
  REF_QUAL_LVALUE = 1,
  REF_QUAL_RVALUE = 2
};

/* A storage class.  */

enum cp_storage_class {
  /* sc_none must be zero so that zeroing a cp_decl_specifier_seq
     sets the storage_class field to sc_none.  */
  sc_none = 0,
  sc_auto,
  sc_register,
  sc_static,
  sc_extern,
  sc_mutable
};

/* An individual decl-specifier.  This is used to index the array of
   locations for the declspecs in struct cp_decl_specifier_seq
   below.
   A subset of these enums also corresponds to elements of
   cp_parser_set_decl_spec_type:decl_spec_names in parser.cc.  */

enum cp_decl_spec {
  ds_first,
  ds_signed = ds_first, /* Index of first element of decl_spec_names.  */
  ds_unsigned,
  ds_short,
  ds_long,
  ds_const,
  ds_volatile,
  ds_restrict,
  ds_inline,
  ds_virtual,
  ds_explicit,
  ds_friend,
  ds_typedef,
  ds_alias,
  ds_constexpr,
  ds_complex,
  ds_constinit,
  ds_consteval,
  ds_this, /* Index of last element of decl_spec_names.  */
  ds_thread,
  ds_type_spec,
  ds_redefined_builtin_type_spec,
  ds_attribute,
  ds_std_attribute,
  ds_storage_class,
  ds_long_long,
  ds_concept,
  ds_last /* This enumerator must always be the last one.  */
};

/* A decl-specifier-seq.  */

struct cp_decl_specifier_seq {
  /* An array of locations for the declaration sepecifiers, indexed by
     enum cp_decl_spec_word.  */
  location_t locations[ds_last];
  /* The primary type, if any, given by the decl-specifier-seq.
     Modifiers, like "short", "const", and "unsigned" are not
     reflected here.  This field will be a TYPE, unless a typedef-name
     was used, in which case it will be a TYPE_DECL.  */
  tree type;
  /* The attributes, if any, provided with the specifier sequence.  */
  tree attributes;
  /* The c++11 attributes that follows the type specifier.  */
  tree std_attributes;
  /* If non-NULL, a built-in type that the user attempted to redefine
     to some other type.  */
  tree redefined_builtin_type;
  /* The explicit-specifier, if any.  */
  tree explicit_specifier;
  /* The storage class specified -- or sc_none if no storage class was
     explicitly specified.  */
  cp_storage_class storage_class;
  /* For the __intN declspec, this stores the index into the int_n_* arrays.  */
  int int_n_idx;
  /* True iff TYPE_SPEC defines a class or enum.  */
  BOOL_BITFIELD type_definition_p : 1;
  /* True iff multiple types were (erroneously) specified for this
     decl-specifier-seq.  */
  BOOL_BITFIELD multiple_types_p : 1;
  /* True iff multiple storage classes were (erroneously) specified
     for this decl-specifier-seq or a combination of a storage class
     with a typedef specifier.  */
  BOOL_BITFIELD conflicting_specifiers_p : 1;
  /* True iff at least one decl-specifier was found.  */
  BOOL_BITFIELD any_specifiers_p : 1;
  /* True iff at least one type-specifier was found.  */
  BOOL_BITFIELD any_type_specifiers_p : 1;
  /* True iff "int" was explicitly provided.  */
  BOOL_BITFIELD explicit_int_p : 1;
  /* True iff "__intN" was explicitly provided.  */
  BOOL_BITFIELD explicit_intN_p : 1;
  /* True iff "char" was explicitly provided.  */
  BOOL_BITFIELD explicit_char_p : 1;
  /* True iff ds_thread is set for __thread, not thread_local.  */
  BOOL_BITFIELD gnu_thread_keyword_p : 1;
  /* True iff the type is a decltype.  */
  BOOL_BITFIELD decltype_p : 1;
  /* True iff the alternate "__intN__" form of the __intN type has been
     used.  */
  BOOL_BITFIELD int_n_alt: 1;
};

/* The various kinds of declarators.  */

enum cp_declarator_kind {
  cdk_id,
  cdk_function,
  cdk_array,
  cdk_pointer,
  cdk_reference,
  cdk_ptrmem,
  cdk_decomp,
  cdk_error
};

/* A declarator.  */

typedef struct cp_declarator cp_declarator;

typedef struct cp_parameter_declarator cp_parameter_declarator;

/* A parameter, before it has been semantically analyzed.  */
struct cp_parameter_declarator {
  /* The next parameter, or NULL_TREE if none.  */
  cp_parameter_declarator *next;
  /* The decl-specifiers-seq for the parameter.  */
  cp_decl_specifier_seq decl_specifiers;
  /* The declarator for the parameter.  */
  cp_declarator *declarator;
  /* The default-argument expression, or NULL_TREE, if none.  */
  tree default_argument;
  /* True iff this is a template parameter pack.  */
  bool template_parameter_pack_p;
  /* Location within source.  */
  location_t loc;
};

/* A declarator.  */
struct cp_declarator {
  /* The kind of declarator.  */
  ENUM_BITFIELD (cp_declarator_kind) kind : 4;
  /* Whether we parsed an ellipsis (`...') just before the declarator,
     to indicate this is a parameter pack.  */
  BOOL_BITFIELD parameter_pack_p : 1;
  /* If this declarator is parenthesized, this the open-paren.  It is
     UNKNOWN_LOCATION when not parenthesized.  */
  location_t parenthesized;
  /* Currently only set for cdk_id, cdk_decomp and cdk_function.  */
  location_t id_loc;
  /* If this declarator is part of an init-declarator, the location of the
     initializer.  */
  location_t init_loc;
  /* GNU Attributes that apply to this declarator.  If the declarator
     is a pointer or a reference, these attribute apply to the type
     pointed to.  */
  tree attributes;
  /* Standard C++11 attributes that apply to this declarator.  If the
     declarator is a pointer or a reference, these attributes apply
     to the pointer, rather than to the type pointed to.  */
  tree std_attributes;
  /* For all but cdk_id, cdk_decomp and cdk_error, the contained declarator.
     For cdk_id, cdk_decomp and cdk_error, guaranteed to be NULL.  */
  cp_declarator *declarator;
  union {
    /* For identifiers.  */
    struct {
      /* If non-NULL, the qualifying scope (a NAMESPACE_DECL or
	 *_TYPE) for this identifier.  */
      tree qualifying_scope;
      /* The unqualified name of the entity -- an IDENTIFIER_NODE,
	 BIT_NOT_EXPR, or TEMPLATE_ID_EXPR.  */
      tree unqualified_name;
      /* If this is the name of a function, what kind of special
	 function (if any).  */
      special_function_kind sfk;
    } id;
    /* For functions.  */
    struct {
      /* The parameters to the function as a TREE_LIST of decl/default.  */
      tree parameters;
      /* The cv-qualifiers for the function.  */
      cp_cv_quals qualifiers;
      /* The virt-specifiers for the function.  */
      cp_virt_specifiers virt_specifiers;
      /* The ref-qualifier for the function.  */
      cp_ref_qualifier ref_qualifier;
      /* The transaction-safety qualifier for the function.  */
      tree tx_qualifier;
      /* The exception-specification for the function.  */
      tree exception_specification;
      /* The late-specified return type, if any.  */
      tree late_return_type;
      /* The trailing requires-clause, if any.  */
      tree requires_clause;
      location_t parens_loc;
    } function;
    /* For arrays.  */
    struct {
      /* The bounds to the array.  */
      tree bounds;
    } array;
    /* For cdk_pointer and cdk_ptrmem.  */
    struct {
      /* The cv-qualifiers for the pointer.  */
      cp_cv_quals qualifiers;
      /* For cdk_ptrmem, the class type containing the member.  */
      tree class_type;
    } pointer;
    /* For cdk_reference */
    struct {
      /* The cv-qualifiers for the reference.  These qualifiers are
         only used to diagnose ill-formed code.  */
      cp_cv_quals qualifiers;
      /* Whether this is an rvalue reference */
      bool rvalue_ref;
    } reference;
  } u;
};

/* A level of template instantiation.  */
struct GTY((chain_next ("%h.next"))) tinst_level {
  /* The immediately deeper level in the chain.  */
  struct tinst_level *next;

  /* The original node.  TLDCL can be a DECL (for a function or static
     data member), a TYPE (for a class), depending on what we were
     asked to instantiate, or a TREE_LIST with the template as PURPOSE
     and the template args as VALUE, if we are substituting for
     overload resolution.  In all these cases, TARGS is NULL.
     However, to avoid creating TREE_LIST objects for substitutions if
     we can help, we store PURPOSE and VALUE in TLDCL and TARGS,
     respectively.  So TLDCL stands for TREE_LIST or DECL (the
     template is a DECL too), whereas TARGS stands for the template
     arguments.  */
  tree tldcl, targs;

  /* For modules we need to know (a) the modules on the path of
     instantiation and (b) the transitive imports along that path.
     Note that these two bitmaps may be inherited from NEXT, if this
     decl is in the same module as NEXT (or has no new information).  */
  bitmap path;
  bitmap visible;

 private:
  /* Return TRUE iff the original node is a split list.  */
  bool split_list_p () const { return targs; }

  /* Return TRUE iff the original node is a TREE_LIST object.  */
  bool tree_list_p () const
  {
    return !split_list_p () && TREE_CODE (tldcl) == TREE_LIST;
  }

  /* Return TRUE iff the original node is not a list, split or not.  */
  bool not_list_p () const
  {
    return !split_list_p () && !tree_list_p ();
  }

  /* Convert (in place) the original node from a split list to a
     TREE_LIST.  */
  tree to_list ();

 public:
  /* Release storage for OBJ and node, if it's a TREE_LIST.  */
  static void free (tinst_level *obj);

  /* Return TRUE iff the original node is a list, split or not.  */
  bool list_p () const { return !not_list_p (); }

  /* Return the original node; if it's a split list, make it a
     TREE_LIST first, so that it can be returned as a single tree
     object.  */
  tree get_node () {
    if (!split_list_p ()) return tldcl;
    else return to_list ();
  }

  /* Return the original node if it's a DECL or a TREE_LIST, but do
     NOT convert a split list to a TREE_LIST: return NULL instead.  */
  tree maybe_get_node () const {
    if (!split_list_p ()) return tldcl;
    else return NULL_TREE;
  }

  /* The location where the template is instantiated.  */
  location_t locus;

  /* errorcount + sorrycount when we pushed this level.  If the value
     overflows, it will always seem like we currently have more errors, so we
     will limit template recursion even from non-erroneous templates.  In a TU
     with over 32k errors, that's fine.  */
  unsigned short errors : 15;

  /* set in pop_tinst_level if there have been errors since we pushed.  */
  bool had_errors : 1;

  /* Count references to this object.  If refcount reaches
     refcount_infinity value, we don't increment or decrement the
     refcount anymore, as the refcount isn't accurate anymore.
     The object can be still garbage collected if unreferenced from
     anywhere, which might keep referenced objects referenced longer than
     otherwise necessary.  Hitting the infinity is rare though.  */
  unsigned short refcount;

  /* Infinity value for the above refcount.  */
  static const unsigned short refcount_infinity = (unsigned short) ~0;
};

/* BUILT_IN_FRONTEND function codes.  */
enum cp_built_in_function {
  CP_BUILT_IN_IS_CONSTANT_EVALUATED,
  CP_BUILT_IN_INTEGER_PACK,
  CP_BUILT_IN_IS_CORRESPONDING_MEMBER,
  CP_BUILT_IN_IS_POINTER_INTERCONVERTIBLE_WITH_CLASS,
  CP_BUILT_IN_SOURCE_LOCATION,
  CP_BUILT_IN_EH_PTR_ADJUST_REF,
  CP_BUILT_IN_LAST
};

bool decl_spec_seq_has_spec_p (const cp_decl_specifier_seq *, cp_decl_spec);

/* Return the type of the `this' parameter of FNTYPE.  */

inline tree
type_of_this_parm (const_tree fntype)
{
  function_args_iterator iter;
  gcc_assert (TREE_CODE (fntype) == METHOD_TYPE);
  function_args_iter_init (&iter, fntype);
  return function_args_iter_cond (&iter);
}

/* Return the class of the `this' parameter of FNTYPE.  */

inline tree
class_of_this_parm (const_tree fntype)
{
  return TREE_TYPE (type_of_this_parm (fntype));
}

/* A parameter list indicating for a function with no parameters,
   e.g  "int f(void)".  */
extern cp_parameter_declarator *no_parameters;

/* Various dump ids.  */
extern int class_dump_id;
extern int module_dump_id;
extern int raw_dump_id;
extern int coro_dump_id;
extern int tinst_dump_id;

/* Whether the current context is manifestly constant-evaluated.
   Used by the constexpr machinery to control folding of
   __builtin_is_constant_evaluated.  */

enum class mce_value
{
  /* Unknown, so treat __builtin_is_constant_evaluated as non-constant.  */
  mce_unknown = 0,
  /* Fold it to true.  */
  mce_true = 1,
  /* Fold it to false.  Primarily used during cp_fold_function and
     cp_fully_fold_init.  */
  mce_false = -1,
};
constexpr mce_value mce_unknown = mce_value::mce_unknown;
constexpr mce_value mce_true = mce_value::mce_true;
constexpr mce_value mce_false = mce_value::mce_false;

/* in call.cc */
extern bool check_dtor_name			(tree, tree);
int magic_varargs_p				(tree);

extern tree build_conditional_expr		(const op_location_t &,
						 tree, tree, tree,
                                                 tsubst_flags_t);
extern tree build_addr_func			(tree, tsubst_flags_t);
extern void set_flags_from_callee		(tree);
extern tree build_call_a			(tree, int, tree*);
extern tree build_call_n			(tree, int, ...);
extern bool null_ptr_cst_p			(tree);
extern bool null_member_pointer_value_p		(tree);
extern bool sufficient_parms_p			(const_tree);
extern tree type_decays_to			(tree);
extern tree extract_call_expr			(tree);
extern tree build_trivial_dtor_call		(tree, bool = false);
extern tristate ref_conv_binds_to_temporary	(tree, tree, bool = false);
extern unsigned HOST_WIDE_INT count_ctor_elements (tree);
extern tree build_user_type_conversion		(tree, tree, int,
						 tsubst_flags_t);
extern tree build_new_function_call		(tree, vec<tree, va_gc> **,
						 tsubst_flags_t);
extern tree build_operator_new_call		(tree, vec<tree, va_gc> **,
						 tree *, tree *, tree, tree,
						 tree *, tsubst_flags_t);
extern tree build_new_method_call		(tree, tree,
						 vec<tree, va_gc> **, tree,
						 int, tree *, tsubst_flags_t);
extern tree build_special_member_call		(tree, tree,
						 vec<tree, va_gc> **,
						 tree, int, tsubst_flags_t);
extern tree build_new_op			(const op_location_t &,
						 enum tree_code,
						 int, tree, tree, tree, tree,
						 tree *, tsubst_flags_t);
/* Wrapper that leaves out the usually-null op3 and overload parms.  */
inline tree build_new_op (const op_location_t &loc, enum tree_code code,
			  int flags, tree arg1, tree arg2,
			  tsubst_flags_t complain)
{
  return build_new_op (loc, code, flags, arg1, arg2, NULL_TREE, NULL_TREE,
		       NULL, complain);
}
extern tree keep_unused_object_arg		(tree, tree, tree);
extern tree build_op_call			(tree, vec<tree, va_gc> **,
						 tsubst_flags_t);
extern tree build_op_subscript			(const op_location_t &, tree,
						 vec<tree, va_gc> **, tree *,
						 tsubst_flags_t);
extern bool aligned_allocation_fn_p		(tree);
extern tree destroying_delete_p			(tree);
extern bool usual_deallocation_fn_p		(tree);
extern tree build_op_delete_call		(enum tree_code, tree, tree,
						 bool, tree, tree,
						 tsubst_flags_t);
extern tree build_coroutine_op_delete_call	(enum tree_code, tree, tree,
						 bool, tree, tree,
						 tsubst_flags_t complain);
extern bool can_convert				(tree, tree, tsubst_flags_t);
extern bool can_convert_standard		(tree, tree, tsubst_flags_t);
extern bool can_convert_arg			(tree, tree, tree, int,
						 tsubst_flags_t);
extern bool can_convert_arg_bad			(tree, tree, tree, int,
						 tsubst_flags_t);
extern int conv_flags				(int, int, tree, tree, int);
extern struct conversion * good_conversion	(tree, tree, tree, int, tsubst_flags_t);
extern location_t get_fndecl_argument_location  (tree, int);
extern void complain_about_bad_argument	(location_t arg_loc,
						 tree from_type, tree to_type,
						 tree fndecl, int parmnum);
extern void maybe_inform_about_fndecl_for_bogus_argument_init (tree, int,
							       const char * = nullptr);
extern tree perform_dguide_overload_resolution	(tree, const vec<tree, va_gc> *,
						 tsubst_flags_t);


/* A class for recording information about access failures (e.g. private
   fields), so that we can potentially supply a fix-it hint about
   an accessor (from a context in which the constness of the object
   is known).  */

class access_failure_info
{
 public:
  access_failure_info () : m_was_inaccessible (false),
    m_basetype_path (NULL_TREE),
    m_decl (NULL_TREE), m_diag_decl (NULL_TREE) {}

  void record_access_failure (tree basetype_path, tree decl, tree diag_decl);

  bool was_inaccessible_p () const { return m_was_inaccessible; }
  tree get_decl () const { return m_decl; }
  tree get_diag_decl () const { return m_diag_decl; }
  tree get_any_accessor (bool const_p) const;
  void maybe_suggest_accessor (bool const_p) const;
  static void add_fixit_hint (rich_location *richloc, tree accessor);

 private:
  bool m_was_inaccessible;
  tree m_basetype_path;
  tree m_decl;
  tree m_diag_decl;
};

extern void complain_about_access		(tree, tree, tree, bool,
						 access_kind);
extern void push_defarg_context			(tree);
extern void pop_defarg_context			(void);
extern tree convert_default_arg			(tree, tree, tree, int,
						 tsubst_flags_t);
extern tree convert_arg_to_ellipsis		(tree, tsubst_flags_t);
extern tree build_x_va_arg			(location_t, tree, tree);
extern tree cxx_type_promotes_to		(tree);
extern tree type_passed_as			(tree);
extern tree convert_for_arg_passing		(tree, tree, tsubst_flags_t);
extern bool is_properly_derived_from		(tree, tree);
extern tree initialize_reference		(tree, tree, int,
						 tsubst_flags_t);
extern tree extend_ref_init_temps		(tree, tree,
						 vec<tree, va_gc>**,
						 tree * = NULL);
extern tree make_temporary_var_for_ref_to_temp	(tree, tree);
extern bool type_has_extended_temps		(tree);
extern tree strip_top_quals			(tree);
extern bool reference_related_p			(tree, tree);
extern bool reference_compatible_p		(tree, tree);
extern bool handler_match_for_exception_type	(tree, tree);
extern int remaining_arguments			(tree);
extern tree build_implicit_conv_flags		(tree, tree, int);
extern tree perform_implicit_conversion		(tree, tree, tsubst_flags_t);
extern tree perform_implicit_conversion_flags	(tree, tree, tsubst_flags_t, int);
extern tree build_converted_constant_expr	(tree, tree, tsubst_flags_t);
extern tree build_converted_constant_bool_expr	(tree, tsubst_flags_t);
extern tree perform_direct_initialization_if_possible (tree, tree, bool,
                                                       tsubst_flags_t);
extern vec<tree,va_gc> *resolve_args (vec<tree,va_gc>*, tsubst_flags_t);
extern tree in_charge_arg_for_name		(tree);
extern bool in_immediate_context		();
extern bool immediate_invocation_p		(tree);
extern tree build_cxx_call			(tree, int, tree *,
						 tsubst_flags_t,
						 tree = NULL_TREE);
extern bool is_std_init_list			(tree);
extern bool is_list_ctor			(tree);
extern void validate_conversion_obstack		(void);
extern void mark_versions_used			(tree);
extern int unsafe_return_slot_p			(tree);
extern bool unsafe_copy_elision_p		(tree, tree);
extern bool make_safe_copy_elision		(tree, tree);
extern bool cp_handle_deprecated_or_unavailable (tree, tsubst_flags_t = tf_warning_or_error);
extern void cp_warn_deprecated_use_scopes	(tree);
extern tree get_function_version_dispatcher	(tree);
extern bool any_template_arguments_need_structural_equality_p (tree);
extern void maybe_show_nonconverting_candidate	(tree, tree, tree, int);
extern bool conv_binds_to_reference_parm_p	(tree, tree);

/* in class.cc */
extern tree build_vfield_ref			(tree, tree);
extern tree build_if_in_charge			(tree true_stmt, tree false_stmt = void_node);
extern tree build_base_path			(enum tree_code, tree,
						 tree, int, tsubst_flags_t);
extern tree convert_to_base			(tree, tree, bool, bool,
						 tsubst_flags_t);
extern tree convert_to_base_statically		(tree, tree);
extern bool is_empty_base_ref			(tree);
extern tree build_vtbl_ref			(tree, tree);
extern tree build_vfn_ref			(tree, tree);
extern tree get_vtable_decl			(tree, int);
extern bool object_parms_correspond		(tree, tree, tree);
extern bool iobj_parm_corresponds_to		(tree, tree, tree);
extern bool add_method				(tree, tree, bool);
extern tree declared_access			(tree);
extern bool maybe_push_used_methods		(tree);
extern tree currently_open_class		(tree);
extern tree currently_open_derived_class	(tree);
extern tree outermost_open_class		(void);
extern tree current_nonlambda_class_type	(void);
extern tree finish_struct			(tree, tree);
extern void finish_struct_1			(tree);
extern int resolves_to_fixed_type_p		(tree, int * = NULL);
extern void init_class_processing		(void);
extern int is_empty_class			(tree);
extern bool is_really_empty_class		(tree, bool);
extern void pushclass				(tree);
extern void popclass				(void);
extern void push_nested_class			(tree);
extern void pop_nested_class			(void);
extern int current_lang_depth			(void);
extern void push_lang_context			(tree);
extern void pop_lang_context			(void);
extern tree instantiate_type			(tree, tree, tsubst_flags_t);
extern void build_self_reference		(void);
extern int same_signature_p			(const_tree, const_tree);
extern tree lookup_vfn_in_binfo			(tree, tree);
extern void maybe_add_class_template_decl_list	(tree, tree, int);
extern void unreverse_member_declarations	(tree);
extern bool is_empty_field			(tree);
extern void invalidate_class_lookup_cache	(void);
extern void maybe_note_name_used_in_class	(tree, tree);
extern void note_name_declared_in_class		(tree, tree);
extern tree get_vtbl_decl_for_binfo		(tree);
extern bool vptr_via_virtual_p			(tree);
extern void debug_class				(tree);
extern void debug_thunks			(tree);
extern void set_linkage_according_to_type	(tree, tree);
extern void determine_key_method		(tree);
extern void check_for_override			(tree, tree);
extern void push_class_stack			(void);
extern void pop_class_stack			(void);
extern bool default_ctor_p			(const_tree);
extern bool type_has_user_nondefault_constructor (tree);
extern tree in_class_defaulted_default_constructor (tree);
extern bool user_provided_p			(tree);
extern bool type_has_user_provided_constructor  (tree);
extern bool type_has_non_user_provided_default_constructor (tree);
extern bool type_has_converting_constructor	(tree);
extern bool vbase_has_user_provided_move_assign (tree);
extern tree default_init_uninitialized_part (tree);
extern bool trivial_default_constructor_is_constexpr (tree);
extern bool type_has_constexpr_default_constructor (tree);
extern bool type_has_constexpr_destructor	(tree);
extern bool type_has_virtual_destructor		(tree);
extern bool type_has_non_deleted_trivial_default_ctor (tree);
extern bool classtype_has_move_assign_or_move_ctor_p (tree, bool user_declared);
extern bool classtype_has_non_deleted_move_ctor (tree);
extern tree classtype_has_depr_implicit_copy	(tree);
extern bool classtype_has_op (tree, tree_code);
extern tree classtype_has_defaulted_op (tree, tree_code);
extern bool type_build_ctor_call		(tree);
extern bool type_build_dtor_call		(tree);
extern void explain_non_literal_class		(tree);
extern void inherit_targ_abi_tags		(tree);
extern void maybe_delete_defaulted_fn		(tree, tree);
extern void defaulted_late_check		(tree, tristate = tristate::unknown ());
extern bool defaultable_fn_check		(tree);
extern void check_abi_tags			(tree);
extern tree missing_abi_tags			(tree);
extern void fixup_type_variants			(tree);
extern void fixup_attribute_variants		(tree);
extern void build_cdtor_clones 			(tree, bool, bool, bool);
extern void clone_cdtor				(tree, bool);
extern tree copy_operator_fn			(tree, tree_code code);
extern void adjust_clone_args			(tree);
extern void deduce_noexcept_on_destructor       (tree);
extern bool uniquely_derived_from_p             (tree, tree);
extern bool publicly_uniquely_derived_p         (tree, tree);
extern bool publicly_virtually_derived_p        (tree, tree);
extern tree common_enclosing_class		(tree, tree);

/* in cvt.cc */
extern tree convert_to_reference		(tree, tree, int, int, tree,
						 tsubst_flags_t);
extern tree convert_from_reference		(tree);
extern tree force_rvalue			(tree, tsubst_flags_t);
extern tree force_lvalue			(tree, tsubst_flags_t);
extern tree ocp_convert				(tree, tree, int, int,
						 tsubst_flags_t);
extern tree cp_convert				(tree, tree, tsubst_flags_t);
extern tree cp_convert_and_check                (tree, tree, tsubst_flags_t);
extern tree cp_fold_convert			(tree, tree);
extern tree cp_get_callee			(tree);
extern tree cp_get_callee_fndecl		(tree);
extern tree cp_get_callee_fndecl_nofold		(tree);
extern tree cp_get_fndecl_from_callee		(tree, bool fold = true);
extern tree convert_to_void			(tree, impl_conv_void,
                                 		 tsubst_flags_t);
extern tree convert_force			(tree, tree, int,
						 tsubst_flags_t);
extern tree build_expr_type_conversion		(int, tree, bool);
extern tree type_promotes_to			(tree);
extern bool can_convert_qual			(tree, tree);
extern tree perform_qualification_conversions	(tree, tree);
extern bool tx_safe_fn_type_p			(tree);
extern tree tx_unsafe_fn_variant		(tree);
extern bool fnptr_conv_p			(tree, tree);
extern tree strip_fnptr_conv			(tree);

/* in name-lookup.cc */
extern void maybe_push_cleanup_level		(tree);
extern tree maybe_push_decl			(tree);
extern tree current_decl_namespace		(void);

/* decl.cc */
extern tree poplevel				(int, int, int);
extern void cxx_init_decl_processing		(void);
enum cp_tree_node_structure_enum cp_tree_node_structure
						(union lang_tree_node *);
extern void finish_scope			(void);
extern void push_switch				(tree);
extern void pop_switch				(void);
extern void note_break_stmt			(void);
extern bool note_iteration_stmt_body_start	(void);
extern void note_iteration_stmt_body_end	(bool);
extern void determine_local_discriminator	(tree, tree = NULL_TREE);
extern bool member_like_constrained_friend_p	(tree);
extern bool fns_correspond			(tree, tree);
extern int decls_match				(tree, tree, bool = true);
extern bool maybe_version_functions		(tree, tree);
extern bool validate_constexpr_redeclaration	(tree, tree);
extern bool merge_default_template_args		(tree, tree, bool);
extern tree duplicate_decls			(tree, tree,
						 bool hiding = false,
						 bool was_hidden = false);
extern void mark_label_addressed		(tree);
extern tree declare_local_label			(tree);
extern tree define_label			(location_t, tree);
extern void check_goto				(tree);
extern bool check_omp_return			(void);
extern tree make_typename_type			(tree, tree, enum tag_types, tsubst_flags_t);
extern tree build_typename_type			(tree, tree, tree, tag_types);
extern tree make_unbound_class_template		(tree, tree, tree, tsubst_flags_t);
extern tree make_unbound_class_template_raw	(tree, tree, tree);
extern unsigned push_abi_namespace		(tree node = abi_node);
extern void pop_abi_namespace			(unsigned flags,
						 tree node = abi_node);
extern tree build_library_fn_ptr		(const char *, tree, int);
extern tree build_cp_library_fn_ptr		(const char *, tree, int);
extern tree push_library_fn			(tree, tree, tree, int);
extern tree push_throw_library_fn		(tree, tree);
extern void warn_misplaced_attr_for_class_type  (location_t location,
						 tree class_type);
extern tree check_tag_decl			(cp_decl_specifier_seq *, bool);
extern tree shadow_tag				(cp_decl_specifier_seq *);
extern tree groktypename			(cp_decl_specifier_seq *, const cp_declarator *, bool);
extern tree start_decl				(const cp_declarator *, cp_decl_specifier_seq *, int, tree, tree, tree *);
extern void start_decl_1			(tree, bool);
extern bool check_array_initializer		(tree, tree, tree);
extern void omp_declare_variant_finalize	(tree, tree);
struct cp_decomp { tree decl; unsigned int count; };
extern void cp_finish_decl			(tree, tree, bool, tree, int, cp_decomp * = nullptr);
extern tree lookup_decomp_type			(tree);
extern bool cp_finish_decomp			(tree, cp_decomp *, bool = false);
extern int cp_complete_array_type		(tree *, tree, bool);
extern int cp_complete_array_type_or_error	(tree *, tree, bool, tsubst_flags_t);
extern tree build_ptrmemfunc_type		(tree);
extern tree build_ptrmem_type			(tree, tree);
/* the grokdeclarator prototype is in decl.h */
extern tree build_this_parm			(tree, tree, cp_cv_quals);
extern tree grokparms				(tree, tree *);
extern int copy_fn_p				(const_tree);
extern bool move_fn_p                           (const_tree);
extern bool move_signature_fn_p                 (const_tree);
extern tree get_scope_of_declarator		(const cp_declarator *);
extern void grok_special_member_properties	(tree);
extern bool grok_ctor_properties		(const_tree, const_tree);
extern bool grok_op_properties			(tree, bool);
extern tree xref_tag				(tag_types, tree,
						 TAG_how = TAG_how::CURRENT_ONLY,
						 bool tpl_header_p = false);
extern void xref_basetypes			(tree, tree);
extern tree start_enum				(tree, tree, tree, tree, bool, bool *);
extern void finish_enum_value_list		(tree);
extern void finish_enum				(tree);
extern tree build_enumerator			(tree, tree, tree, tree, location_t);
extern tree lookup_enumerator			(tree, tree);
extern bool start_preparsed_function		(tree, tree, int);
extern bool start_function			(cp_decl_specifier_seq *,
						 const cp_declarator *, tree);
extern tree maybe_prepare_return_this		(tree);
extern void maybe_return_this			(void);
extern tree begin_function_body			(void);
extern void finish_function_body		(tree);
extern tree outer_curly_brace_block		(tree);
extern tree finish_function			(bool);
extern tree grokmethod				(cp_decl_specifier_seq *, const cp_declarator *, tree);
extern void maybe_register_incomplete_var	(tree);
extern void maybe_commonize_var			(tree);
extern void complete_vars			(tree);
extern tree static_fn_type			(tree);
extern void revert_static_member_fn		(tree);
extern void fixup_anonymous_aggr		(tree);
extern tree compute_array_index_type		(tree, tree, tsubst_flags_t);
extern tree check_default_argument		(tree, tree, tsubst_flags_t);
extern int wrapup_namespace_globals		();
extern tree create_implicit_typedef		(tree, tree);
extern int local_variable_p			(const_tree);
extern tree get_cxa_atexit_fn_ptr_type		();
extern tree register_dtor_fn			(tree, bool = false);
extern tmpl_spec_kind current_tmpl_spec_kind	(int);
extern tree cxx_builtin_function		(tree decl);
extern tree cxx_builtin_function_ext_scope	(tree decl);
extern tree cxx_simulate_builtin_function_decl	(tree);
extern tree check_elaborated_type_specifier	(enum tag_types, tree, bool);
extern void warn_extern_redeclared_static	(tree, tree);
extern tree cxx_comdat_group			(tree);
extern bool cp_missing_noreturn_ok_p		(tree);
extern bool is_direct_enum_init			(tree, tree);
extern void initialize_artificial_var		(tree, vec<constructor_elt, va_gc> *);
extern tree check_var_type			(tree, tree, location_t);
extern tree reshape_init                        (tree, tree, tsubst_flags_t);
extern tree next_aggregate_field		(tree);
extern tree next_subobject_field		(tree);
extern tree first_field				(const_tree);
extern tree fndecl_declared_return_type		(tree);
extern bool undeduced_auto_decl			(tree);
extern bool require_deduced_type		(tree, tsubst_flags_t = tf_warning_or_error);

extern tree finish_case_label			(location_t, tree, tree);
extern tree cxx_maybe_build_cleanup		(tree, tsubst_flags_t);
extern bool check_array_designated_initializer  (constructor_elt *,
						 unsigned HOST_WIDE_INT);
extern bool check_for_uninitialized_const_var   (tree, bool, tsubst_flags_t);
extern tree build_explicit_specifier		(tree, tsubst_flags_t);
extern bool use_eh_spec_block			(tree);
extern void do_push_parm_decls			(tree, tree, tree *);
extern tree do_aggregate_paren_init		(tree, tree);

/* in decl2.cc */
extern void record_mangling			(tree, bool);
extern void overwrite_mangling			(tree, tree);
extern void note_mangling_alias			(tree, tree);
extern void generate_mangling_aliases		(void);
extern tree build_memfn_type			(tree, tree, cp_cv_quals, cp_ref_qualifier);
extern tree build_pointer_ptrmemfn_type	(tree);
extern tree change_return_type			(tree, tree);
extern void maybe_retrofit_in_chrg		(tree);
extern void maybe_make_one_only			(tree);
extern bool vague_linkage_p			(tree);
extern void grokclassfn				(tree, tree,
						 enum overload_flags);
extern tree grok_array_decl			(location_t, tree, tree,
						 vec<tree, va_gc> **, tsubst_flags_t);
extern tree grok_omp_array_section		(location_t, tree, tree, tree);
extern tree delete_sanity			(location_t, tree, tree, bool,
						 int, tsubst_flags_t);
extern tree check_classfn			(tree, tree, tree);
extern void check_member_template		(tree);
extern tree grokfield (const cp_declarator *, cp_decl_specifier_seq *,
		       tree, bool, tree, tree);
extern tree grokbitfield (const cp_declarator *, cp_decl_specifier_seq *,
			  tree, tree, tree);
extern tree start_initialized_static_member	(const cp_declarator *,
						 cp_decl_specifier_seq *, tree);
extern bool is_static_data_member_initialized_in_class (tree decl);
extern void finish_initialized_static_member	(tree, tree, tree);
extern tree splice_template_attributes		(tree *, tree);
extern bool any_dependent_type_attributes_p	(tree);
extern tree cp_reconstruct_complex_type		(tree, tree);
extern bool attributes_naming_typedef_ok	(tree);
extern void cplus_decl_attributes		(tree *, tree, int);
extern void finish_anon_union			(tree);
extern void cxx_post_compilation_parsing_cleanups (void);
extern tree coerce_new_type			(tree, location_t);
extern void coerce_delete_type			(tree, location_t);
extern void comdat_linkage			(tree);
extern void determine_visibility		(tree);
extern void constrain_class_visibility		(tree);
extern void reset_type_linkage			(tree);
extern void tentative_decl_linkage		(tree);
extern void import_export_decl			(tree);
extern tree build_cleanup			(tree);
extern tree build_offset_ref_call_from_tree	(tree, vec<tree, va_gc> **,
						 tsubst_flags_t);
extern bool decl_defined_p			(tree);
extern bool decl_constant_var_p			(tree);
extern bool decl_maybe_constant_var_p		(tree);
extern void no_linkage_error			(tree);
extern void check_default_args			(tree);
extern bool mark_used			        (tree,
						 tsubst_flags_t = tf_warning_or_error);
extern bool mark_single_function	        (tree, tsubst_flags_t);
extern void finish_static_data_member_decl	(tree, tree, bool, tree, int);
extern tree cp_build_parm_decl			(tree, tree, tree);
extern void copy_linkage			(tree, tree);
extern tree get_guard				(tree);
extern tree get_guard_cond			(tree, bool);
extern tree set_guard				(tree);
extern bool var_needs_tls_wrapper		(tree);
extern tree maybe_get_tls_wrapper_call		(tree);
extern void mark_needed				(tree);
extern bool decl_needed_p			(tree);
extern void note_vague_linkage_fn		(tree);
extern void note_vague_linkage_variable		(tree);
extern tree build_artificial_parm		(tree, tree, tree);
extern bool possibly_inlined_p			(tree);
extern int parm_index                           (tree);
extern tree vtv_start_verification_constructor_init_function (void);
extern tree vtv_finish_verification_constructor_init_function (tree);
extern void cp_check_const_attributes (tree);
extern void maybe_propagate_warmth_attributes (tree, tree);

/* in error.cc */
/* A class for pretty-printing to -flang-dump-XXX files.  Used like

   if (cxx_dump_pretty_printer pp {foo_dump_id})
     {
       pp_printf (&pp, ...);
     }

   If the dump is enabled, the pretty printer will open the dump file and
   attach to it, and flush and close the file on destruction.  */

class cxx_dump_pretty_printer: public pretty_printer
{
  int phase;
  FILE *outf;
  dump_flags_t flags;

public:
  cxx_dump_pretty_printer (int phase);
  operator bool() { return outf != nullptr; }
  bool has_flag (dump_flags_t f) { return (flags & f); }
  ~cxx_dump_pretty_printer ();
};

extern const char *type_as_string		(tree, int);
extern const char *type_as_string_translate	(tree, int);
extern const char *decl_as_string		(tree, int);
extern const char *decl_as_string_translate	(tree, int);
extern const char *decl_as_dwarf_string		(tree, int);
extern const char *expr_as_string		(tree, int);
extern const char *expr_to_string		(tree);
extern const char *lang_decl_name		(tree, int, bool);
extern const char *lang_decl_dwarf_name		(tree, int, bool);
extern const char *language_to_string		(enum languages);
extern const char *class_key_or_enum_as_string	(tree);
extern void maybe_warn_variadic_templates       (void);
extern void maybe_warn_cpp0x			(cpp0x_warn_str str,
						 location_t = input_location);
extern bool pedwarn_cxx98                       (location_t,
						 diagnostics::option_id option_id,
						 const char *, ...) ATTRIBUTE_GCC_DIAG(3,4);
extern location_t location_of                   (tree);
extern void qualified_name_lookup_error		(tree, tree, tree,
						 location_t);

struct decl_location_traits
  : simple_cache_map_traits<tree_decl_hash, location_t> { };
typedef hash_map<tree, location_t, decl_location_traits> erroneous_templates_t;
extern GTY((cache)) erroneous_templates_t *erroneous_templates;

extern bool cp_seen_error ();
#define seen_error() cp_seen_error ()

/* in except.cc */
extern void init_terminate_fn			(void);
extern void init_exception_processing		(void);
extern tree expand_start_catch_block		(tree);
extern void expand_end_catch_block		(void);
extern tree build_exc_ptr			(void);
extern tree build_throw				(location_t, tree,
						 tsubst_flags_t);
extern int nothrow_libfn_p			(const_tree);
extern void check_handlers			(tree);
extern tree finish_noexcept_expr		(tree, tsubst_flags_t);
extern bool expr_noexcept_p			(tree, tsubst_flags_t);
extern void explain_not_noexcept		(tree);
extern void perform_deferred_noexcept_checks	(void);
extern bool nothrow_spec_p			(const_tree);
extern bool type_noexcept_p			(const_tree);
extern bool type_throw_all_p			(const_tree);
extern tree build_noexcept_spec			(tree, tsubst_flags_t);
extern void choose_personality_routine		(enum languages);
extern tree build_must_not_throw_expr		(tree,tree);
extern tree eh_type_info			(tree);
extern tree begin_eh_spec_block			(void);
extern void finish_eh_spec_block		(tree, tree);
extern tree build_eh_type_type			(tree);
extern tree cp_protect_cleanup_actions		(void);
extern void maybe_splice_retval_cleanup		(tree, bool);
extern tree maybe_set_retval_sentinel		(void);

extern tree template_parms_to_args		(tree);
extern tree template_parms_level_to_args	(tree);
extern tree generic_targs_for			(tree);
extern tree outer_template_args			(const_tree);

/* in expr.cc */
extern tree cplus_expand_constant		(tree);
extern tree mark_use (tree expr, bool rvalue_p, bool read_p,
		      location_t = UNKNOWN_LOCATION,
		      bool reject_builtin = true);
extern tree mark_rvalue_use			(tree,
                                                 location_t = UNKNOWN_LOCATION,
                                                 bool reject_builtin = true);
extern tree mark_lvalue_use			(tree);
extern tree mark_lvalue_use_nonread		(tree);
extern tree mark_type_use			(tree);
extern tree mark_discarded_use			(tree);
extern void mark_exp_read			(tree);

/* friend.cc */
extern int is_friend				(tree, tree);
extern void make_friend_class			(tree, tree, bool);
extern void add_friend				(tree, tree, bool);
extern tree do_friend				(tree, tree, tree,
						 enum overload_flags, bool);

extern void set_global_friend			(tree);
extern bool is_global_friend			(tree);

/* in init.cc */
extern tree find_temps_r			(tree *, int *, void *);
extern tree expand_member_init			(tree);
extern void emit_mem_initializers		(tree);
extern tree build_aggr_init			(tree, tree, int,
                                                 tsubst_flags_t);
extern int is_class_type			(tree, int);
extern bool is_copy_initialization		(tree);
extern tree build_zero_init			(tree, tree, bool);
extern tree build_value_init			(tree, tsubst_flags_t);
extern tree build_value_init_noctor		(tree, tsubst_flags_t);
extern tree maybe_instantiate_nsdmi_init	(tree, tsubst_flags_t);
extern tree get_nsdmi				(tree, bool, tsubst_flags_t);
extern tree build_offset_ref			(tree, tree, bool,
						 tsubst_flags_t);
extern tree throw_bad_array_new_length		(void);
extern bool type_has_new_extended_alignment	(tree);
extern unsigned malloc_alignment		(void);
extern bool std_placement_new_fn_p		(tree);
extern tree build_new_constexpr_heap_type	(tree, tree, tree);
extern tree build_new				(location_t,
						 vec<tree, va_gc> **, tree,
						 tree, vec<tree, va_gc> **,
						 int, tsubst_flags_t);
extern tree get_temp_regvar			(tree, tree);
extern tree build_vec_init			(tree, tree, tree, bool, int,
						 tsubst_flags_t,
						 vec<tree, va_gc> ** = nullptr);
extern tree build_delete			(location_t, tree, tree,
						 special_function_kind,
						 int, int, tsubst_flags_t);
extern void push_base_cleanups			(void);
extern tree build_vec_delete			(location_t, tree, tree,
						 special_function_kind, int,
						 tsubst_flags_t);
extern tree create_temporary_var		(tree);
extern void initialize_vtbl_ptrs		(tree);
extern tree scalar_constant_value		(tree);
extern tree decl_constant_value			(tree, bool);
extern tree decl_really_constant_value		(tree, bool = true);
extern int diagnose_uninitialized_cst_or_ref_member (tree, bool, bool);
extern tree build_vtbl_address                  (tree);
extern bool maybe_reject_flexarray_init		(tree, tree);

/* in lex.cc */
extern void cxx_dup_lang_specific_decl		(tree);
extern tree unqualified_name_lookup_error	(tree,
						 location_t = UNKNOWN_LOCATION);
extern tree unqualified_fn_lookup_error		(cp_expr);
extern tree make_conv_op_name			(tree);
extern tree build_lang_decl			(enum tree_code, tree, tree);
extern tree build_lang_decl_loc			(location_t, enum tree_code, tree, tree);
extern bool maybe_add_lang_decl_raw		(tree, bool decomp_p);
extern bool maybe_add_lang_type_raw		(tree);
extern void retrofit_lang_decl			(tree);
extern void fit_decomposition_lang_decl		(tree, tree);
extern tree copy_decl				(tree CXX_MEM_STAT_INFO);
extern tree copy_type				(tree CXX_MEM_STAT_INFO);
extern tree cxx_make_type			(enum tree_code CXX_MEM_STAT_INFO);
extern tree make_class_type			(enum tree_code CXX_MEM_STAT_INFO);
extern const char *get_identifier_kind_name	(tree);
extern void set_identifier_kind			(tree, cp_identifier_kind);
extern bool cxx_init				(void);
extern void cxx_finish				(void);
extern bool in_main_input_context		(void);
extern uintptr_t module_token_pre (cpp_reader *, const cpp_token *, uintptr_t);
extern uintptr_t module_token_cdtor (cpp_reader *, uintptr_t);
extern uintptr_t module_token_lang (int type, int keyword, tree value,
				    location_t, uintptr_t);

/* in method.cc */
extern void init_method				(void);
extern tree make_thunk				(tree, bool, tree, tree);
extern void finish_thunk			(tree);
extern void use_thunk				(tree, bool);
extern bool trivial_fn_p			(tree);
extern tree forward_parm			(tree);
extern bool is_trivially_xible			(enum tree_code, tree, tree,
						 bool = false);
extern bool is_nothrow_xible			(enum tree_code, tree, tree,
						 bool = false);
extern bool is_xible				(enum tree_code, tree, tree,
						 bool = false);
extern bool is_convertible			(tree, tree, bool = false);
extern bool is_nothrow_convertible		(tree, tree, bool = false);
extern bool ref_xes_from_temporary		(tree, tree, bool);
extern tree get_defaulted_eh_spec		(tree, tsubst_flags_t = tf_warning_or_error);
extern bool maybe_explain_implicit_delete	(tree);
extern void explain_implicit_non_constexpr	(tree);
extern bool deduce_inheriting_ctor		(tree);
extern bool decl_remember_implicit_trigger_p	(tree);
extern void synthesize_method			(tree);
extern void maybe_synthesize_method		(tree);
extern tree lazily_declare_fn			(special_function_kind,
						 tree);
extern tree skip_artificial_parms_for		(const_tree, tree);
extern int num_artificial_parms_for		(const_tree);
extern tree make_alias_for			(tree, tree);
extern tree get_copy_ctor			(tree, tsubst_flags_t);
extern tree get_copy_assign			(tree);
extern tree get_default_ctor			(tree);
extern tree get_dtor				(tree, tsubst_flags_t);
extern tree build_stub_object			(tree);
extern bool is_stub_object			(tree);
extern tree build_invoke			(tree, const_tree,
						 tsubst_flags_t);
extern tree strip_inheriting_ctors		(tree);
extern tree inherited_ctor_binfo		(tree);
extern bool base_ctor_omit_inherited_parms	(tree);
extern bool ctor_omit_inherited_parms		(tree);
extern tree locate_ctor				(tree);
extern tree implicitly_declare_fn               (special_function_kind, tree,
						 bool, tree, tree);
extern tree type_order_value			(tree, tree);

/* In module.cc  */
class module_state; /* Forward declare.  */
inline bool modules_p () { return flag_modules != 0; }

/* The kind of module or part thereof that we're in.  */
enum module_kind_bits
{
  MK_NAMED = 1 << 0,	// TU is a named module
  MK_HEADER = 1 << 1,	// TU is a header unit
  MK_INTERFACE = 1 << 2,  // TU is an interface
  MK_PARTITION = 1 << 3,  // TU is a partition

  MK_PURVIEW = 1 << 4,	// In purview of current module
  MK_ATTACH = 1 << 5,	// Attaching to named module

  MK_EXPORTING = 1 << 6,  /* We are in an export region.  */
};

/* We do lots of bit-manipulation, so an unsigned is easier.  */
extern unsigned module_kind;

inline bool module_p ()
{ return module_kind & (MK_NAMED | MK_HEADER); }
inline bool named_module_p ()
{ return module_kind & MK_NAMED; }
inline bool header_module_p ()
{ return module_kind & MK_HEADER; }
inline bool module_interface_p ()
{ return module_kind & MK_INTERFACE; }
inline bool module_partition_p ()
{ return module_kind & MK_PARTITION; }
inline bool module_has_cmi_p ()
{ return module_kind & (MK_INTERFACE | MK_PARTITION | MK_HEADER); }

inline bool module_purview_p ()
{ return module_kind & MK_PURVIEW; }
inline bool module_attach_p ()
{ return module_kind & MK_ATTACH; }

inline bool named_module_purview_p ()
{ return named_module_p () && module_purview_p (); }
inline bool named_module_attach_p ()
{ return named_module_p () && module_attach_p (); }

/* Like module_has_cmi_p, but tentatively assumes that this TU may have a
   CMI if we haven't seen the module-declaration yet.  */
inline bool module_maybe_has_cmi_p ()
{ return module_has_cmi_p () || (named_module_p () && !module_purview_p ()); }

/* We're currently exporting declarations.  */
inline bool module_exporting_p ()
{ return module_kind & MK_EXPORTING; }

extern module_state *get_module (tree name, module_state *parent = NULL,
				 bool partition = false);
extern bool module_may_redeclare (tree olddecl, tree newdecl = NULL);

extern bool module_global_init_needed ();
extern bool module_determine_import_inits ();
extern void module_add_import_initializers ();

/* Where the namespace-scope decl was originally declared.  */
extern void set_originating_module (tree, bool friend_p = false);
extern tree get_originating_module_decl (tree) ATTRIBUTE_PURE;
extern int get_originating_module (tree, bool for_mangle = false) ATTRIBUTE_PURE;
extern unsigned get_importing_module (tree, bool = false) ATTRIBUTE_PURE;
extern void check_module_decl_linkage (tree);

/* Where current instance of the decl got declared/defined/instantiated.  */
extern void set_instantiating_module (tree);
extern void set_defining_module (tree);
extern void set_defining_module_for_partial_spec (tree);
extern void maybe_key_decl (tree ctx, tree decl);
extern void propagate_defining_module (tree decl, tree orig);
extern void remove_defining_module (tree decl);

extern void mangle_module (int m, bool include_partition);
extern void mangle_module_fini ();
extern void lazy_load_binding (unsigned mod, tree ns, tree id,
			       binding_slot *bslot);
extern void lazy_load_pendings (tree decl);
extern module_state *preprocess_module (module_state *, location_t,
					bool in_purview,
					bool is_import, bool export_p,
					cpp_reader *reader);
extern void preprocessed_module (cpp_reader *reader);
extern void import_module (module_state *, location_t, bool export_p,
			   tree attr, cpp_reader *);
extern void declare_module (module_state *, location_t, bool export_p,
			    tree attr, cpp_reader *);
extern void init_modules (cpp_reader *);
extern void fini_modules (cpp_reader *, void *cookie, bool);
extern void maybe_check_all_macros (cpp_reader *);
extern void *finish_module_processing (cpp_reader *);
extern char const *module_name (unsigned, bool header_ok);
extern bitmap get_import_bitmap ();
extern bitmap visible_instantiation_path (bitmap *);
extern void module_begin_main_file (cpp_reader *, line_maps *,
				    const line_map_ordinary *);
extern void module_preprocess_options (cpp_reader *);
extern bool handle_module_option (unsigned opt, const char *arg, int value);

/* In optimize.cc */
extern tree clone_attrs				(tree);
extern bool maybe_clone_body			(tree);

/* In parser.cc */
extern tree cp_convert_range_for (tree, tree, tree, cp_decomp *, bool,
				  tree, bool);
extern void cp_convert_omp_range_for (tree &, tree &, tree &,
				      tree &, tree &, tree &, tree &, tree &,
				      bool);
extern void cp_finish_omp_range_for (tree, tree);
extern bool cp_maybe_parse_omp_decl (tree, tree);
extern bool parsing_nsdmi (void);
extern bool parsing_function_declarator ();
extern void inject_this_parameter (tree, cp_cv_quals);
extern location_t defparse_location (tree);
extern void maybe_show_extern_c_location (void);
extern bool literal_integer_zerop (const_tree);
extern tree attr_chainon (tree, tree);

/* in pt.cc */
extern tree canonical_type_parameter		(tree);
extern void push_access_scope			(tree);
extern void pop_access_scope			(tree);
extern bool check_template_shadow		(tree);
extern tree get_innermost_template_args		(tree, int);
extern void maybe_begin_member_template_processing (tree);
extern void maybe_end_member_template_processing (void);
extern tree finish_member_template_decl		(tree);
extern void begin_template_parm_list		(void);
extern bool begin_specialization		(void);
extern void reset_specialization		(void);
extern void end_specialization			(void);
extern void begin_explicit_instantiation	(void);
extern void end_explicit_instantiation		(void);
extern void check_unqualified_spec_or_inst	(tree, location_t);
extern tree check_explicit_specialization	(tree, tree, int, int,
						 tree = NULL_TREE);
extern int num_template_headers_for_class	(tree);
extern void check_template_variable		(tree);
extern tree make_auto				(void);
extern tree make_decltype_auto			(void);
extern tree make_constrained_auto		(tree, tree);
extern tree make_constrained_decltype_auto	(tree, tree);
extern tree make_template_placeholder		(tree);
extern tree make_cast_auto			(void);
extern bool template_placeholder_p		(tree);
extern bool ctad_template_p			(tree);
extern bool unparenthesized_id_or_class_member_access_p (tree);
extern tree do_auto_deduction                   (tree, tree, tree,
                                                 tsubst_flags_t
						 = tf_warning_or_error,
                                                 auto_deduction_context
						 = adc_unspecified,
						 tree = NULL_TREE,
						 int = LOOKUP_NORMAL,
						 tree = NULL_TREE);
extern tree type_uses_auto			(tree);
extern tree convert_generic_types_to_packs	(tree, int, int);
extern tree splice_late_return_type		(tree, tree);
extern bool is_auto				(const_tree);
extern tree process_template_parm		(tree, location_t, tree,
						 bool, bool);
extern tree end_template_parm_list		(tree);
extern void end_template_parm_list		(void);
extern void end_template_decl			(void);
extern tree maybe_update_decl_type		(tree, tree);
extern bool check_default_tmpl_args             (tree, tree, bool, bool, int);
extern tree push_template_decl			(tree, bool is_friend = false);
extern tree add_inherited_template_parms	(tree, tree);
extern void template_parm_level_and_index	(tree, int*, int*);
extern bool redeclare_class_template		(tree, tree, tree);
extern tree adjust_type_for_entering_scope	(tree);
extern tree lookup_template_class		(tree, tree, tree, tree,
						 tsubst_flags_t);
extern tree lookup_template_function		(tree, tree);
extern tree lookup_template_variable		(tree, tree, tsubst_flags_t);
extern bool uses_template_parms			(tree);
extern bool uses_template_parms_level		(tree, int);
extern bool uses_outer_template_parms_in_constraints (tree, tree = NULL_TREE);
extern bool need_generic_capture		(void);
extern tree instantiate_class_template		(tree);
extern tree instantiate_template		(tree, tree, tsubst_flags_t);
extern tree fn_type_unification			(tree, tree, tree,
						 const tree *, unsigned int,
						 tree, unification_kind_t, int,
						 struct conversion **,
						 bool, bool);
extern void setup_explicit_instantiation_definition_linkage (tree);
extern void mark_decl_instantiated		(tree, int);
extern int more_specialized_fn			(tree, tree, int);
extern tree type_targs_deducible_from		(tree, tree);
extern void do_decl_instantiation		(tree, tree);
extern void do_type_instantiation		(tree, tree, tsubst_flags_t);
extern bool always_instantiate_p		(tree);
extern bool maybe_instantiate_noexcept		(tree, tsubst_flags_t = tf_warning_or_error);
extern tree instantiate_decl			(tree, bool, bool);
extern void maybe_instantiate_decl		(tree);
extern int comp_template_parms			(const_tree, const_tree);
extern bool template_heads_equivalent_p		(const_tree, const_tree);
extern bool builtin_pack_fn_p			(tree);
extern tree uses_parameter_packs                (tree);
extern bool template_parameter_pack_p           (const_tree);
extern bool function_parameter_pack_p		(const_tree);
extern bool function_parameter_expanded_from_pack_p (tree, tree);
extern tree make_pack_expansion                 (tree, tsubst_flags_t = tf_warning_or_error);
extern tree make_pack_index			(tree, tree);
extern bool check_for_bare_parameter_packs      (tree, location_t = UNKNOWN_LOCATION);
extern tree build_template_info			(tree, tree);
extern tree get_template_info			(const_tree);
extern int template_class_depth			(tree);
extern int is_specialization_of			(tree, tree);
extern bool is_specialization_of_friend		(tree, tree);
extern bool comp_template_args			(tree, tree, tree * = NULL,
						 tree * = NULL);
extern int template_args_equal                  (tree, tree);
extern tree maybe_process_partial_specialization (tree);
extern tree most_specialized_instantiation	(tree);
extern tree most_specialized_partial_spec       (tree, tsubst_flags_t, bool = false);
extern tree most_constrained_function		(tree);
extern void print_candidates			(tree);
extern void instantiate_pending_templates	(int);
extern tree tsubst_default_argument		(tree, int, tree, tree,
						 tsubst_flags_t);
extern tree tsubst (tree, tree, tsubst_flags_t, tree);
extern tree tsubst_expr                         (tree, tree, tsubst_flags_t, tree);
extern tree tsubst_pack_expansion		(tree, tree, tsubst_flags_t, tree);
extern tree tsubst_argument_pack		(tree, tree, tsubst_flags_t, tree);
extern tree tsubst_template_args		(tree, tree, tsubst_flags_t, tree);
extern tree tsubst_template_arg			(tree, tree, tsubst_flags_t, tree);
extern tree tsubst_function_parms		(tree, tree, tsubst_flags_t, tree);
extern tree most_general_template		(const_tree);
extern tree get_mostly_instantiated_function_type (tree);
extern bool problematic_instantiation_changed	(void);
extern void record_last_problematic_instantiation (void);
extern struct tinst_level *current_instantiation(void);
extern bool instantiating_current_function_p    (void);
extern tree maybe_get_template_decl_from_type_decl (tree);
extern int processing_template_parmlist;
extern bool dependent_type_p			(tree);
extern bool dependent_scope_p			(tree);
extern bool dependentish_scope_p		(tree);
extern bool any_dependent_template_arguments_p  (const_tree);
extern bool any_erroneous_template_args_p       (const_tree);
extern bool dependent_template_p		(tree);
extern bool dependent_template_id_p		(tree, tree);
extern bool type_dependent_expression_p		(tree);
extern bool type_dependent_object_expression_p	(tree);
extern bool any_type_dependent_arguments_p      (const vec<tree, va_gc> *);
extern bool any_type_dependent_elements_p       (const_tree);
extern bool type_dependent_expression_p_push	(tree);
extern bool value_dependent_expression_p	(tree);
extern bool instantiation_dependent_uneval_expression_p (tree);
extern bool any_value_dependent_elements_p      (const_tree);
extern bool dependent_template_arg_p		(tree);
extern bool dependent_omp_for_p			(tree, tree, tree, tree, tree);
extern tree resolve_typename_type		(tree, bool);
extern tree template_for_substitution		(tree);
extern bool reregister_specialization		(tree, tree, tree);
extern tree instantiate_non_dependent_expr	(tree, tsubst_flags_t = tf_error);
extern tree instantiate_non_dependent_expr_internal (tree, tsubst_flags_t);
extern tree instantiate_non_dependent_or_null   (tree);
extern bool variable_template_specialization_p  (tree);
extern bool alias_type_or_template_p            (tree);
enum { nt_opaque = false, nt_transparent = true };
extern tree alias_template_specialization_p     (const_tree, bool);
extern tree dependent_alias_template_spec_p     (const_tree, bool);
extern bool dependent_opaque_alias_p            (const_tree);
extern tree get_template_parm_object		(tree expr, tree mangle,
						 bool check_init = true);
extern tree tparm_object_argument		(tree);
extern bool explicit_class_specialization_p     (tree);
extern bool push_tinst_level                    (tree);
extern bool push_tinst_level                    (tree, tree);
extern bool push_tinst_level_loc                (tree, location_t);
extern bool push_tinst_level_loc                (tree, tree, location_t);
extern void pop_tinst_level                     (void);
extern struct tinst_level *outermost_tinst_level(void);
extern bool non_templated_friend_p		(tree);
extern void init_template_processing		(void);
extern void print_template_statistics		(void);
bool template_template_parameter_p		(const_tree);
bool template_type_parameter_p                  (const_tree);
extern bool primary_template_specialization_p   (const_tree);
extern tree get_primary_template_innermost_parameters	(const_tree);
extern tree get_template_innermost_arguments	(const_tree);
extern tree get_template_argument_pack_elems	(const_tree);
extern tree get_function_template_decl		(const_tree);
extern tree resolve_nondeduced_context		(tree, tsubst_flags_t);
extern tree resolve_nondeduced_context_or_error	(tree, tsubst_flags_t);
extern hashval_t iterative_hash_template_arg	(tree arg, hashval_t val);
extern tree coerce_template_parms		(tree, tree, tree, tsubst_flags_t,
						 bool = true);
extern tree canonicalize_type_argument		(tree, tsubst_flags_t);
extern void register_local_identity		(tree);
extern void register_local_specialization       (tree, tree);
extern tree retrieve_local_specialization       (tree);
extern void register_parameter_specializations	(tree, tree);
extern tree extract_fnparm_pack                 (tree, tree *);
extern tree template_parm_to_arg                (tree);
extern tree dguide_name				(tree);
extern bool dguide_name_p			(tree);
extern bool deduction_guide_p			(const_tree);
extern bool copy_guide_p			(const_tree);
extern bool template_guide_p			(const_tree);
extern bool builtin_guide_p			(const_tree);
extern bool inherited_guide_p			(const_tree);
extern void store_explicit_specifier		(tree, tree);
extern tree lookup_explicit_specifier		(tree);
extern tree lookup_imported_hidden_friend	(tree);
extern void walk_specializations		(bool,
						 void (*)(bool, spec_entry *,
							  void *),
						 void *);
extern tree match_mergeable_specialization	(bool is_decl, spec_entry *);
extern unsigned get_mergeable_specialization_flags (bool is_decl, tree tmpl,
						    tree spec);
extern void add_mergeable_specialization        (bool is_decl, spec_entry *,
						 tree outer, unsigned);
extern tree add_to_template_args		(tree, tree);
extern tree add_outermost_template_args		(tree, tree);
extern tree add_extra_args			(tree, tree, tsubst_flags_t, tree);
extern tree build_extra_args			(tree, tree, tsubst_flags_t);

/* in rtti.cc */
/* A vector of all tinfo decls that haven't been emitted yet.  */
extern GTY(()) vec<tree, va_gc> *unemitted_tinfo_decls;

extern void init_rtti_processing		(void);
extern tree build_typeid			(tree, tsubst_flags_t);
extern tree get_tinfo_decl_direct	        (tree, tree, int);
extern tree get_tinfo_decl			(tree);
extern tree get_typeid				(tree, tsubst_flags_t);
extern tree build_headof			(tree);
extern tree build_dynamic_cast			(location_t, tree, tree,
						 tsubst_flags_t);
extern void emit_support_tinfos			(void);
extern bool emit_tinfo_decl			(tree);
extern unsigned get_pseudo_tinfo_index		(tree);
extern tree get_pseudo_tinfo_type		(unsigned);
extern tree build_if_nonnull			(tree, tree, tsubst_flags_t);

/* in search.cc */
extern tree get_parent_with_private_access 	(tree decl, tree binfo);
extern bool accessible_base_p			(tree, tree, bool);
extern tree lookup_base                         (tree, tree, base_access,
						 base_kind *, tsubst_flags_t,
						 HOST_WIDE_INT = -1);
extern tree dcast_base_hint			(tree, tree);
extern int accessible_p				(tree, tree, bool);
extern int accessible_in_template_p		(tree, tree);
extern tree lookup_field			(tree, tree, int, bool);
extern tree lookup_fnfields			(tree, tree, int, tsubst_flags_t);
extern tree lookup_member			(tree, tree, int, bool,
						 tsubst_flags_t,
						 access_failure_info *afi = NULL);
extern tree lookup_member_fuzzy			(tree, tree, bool);
extern tree locate_field_accessor		(tree, tree, bool);
extern int look_for_overrides			(tree, tree);
extern void get_pure_virtuals			(tree);
extern void maybe_suppress_debug_info		(tree);
extern void note_debug_info_needed		(tree);
extern tree current_scope			(void);
extern int at_function_scope_p			(void);
extern bool at_class_scope_p			(void);
extern bool at_namespace_scope_p		(void);
extern tree context_for_name_lookup		(tree);
extern tree type_context_for_name_lookup	(tree);
extern tree lookup_conversions			(tree);
extern tree binfo_from_vbase			(tree);
extern tree binfo_for_vbase			(tree, tree);
extern tree look_for_overrides_here		(tree, tree);
#define dfs_skip_bases ((tree)1)
extern tree dfs_walk_all (tree, tree (*) (tree, void *),
			  tree (*) (tree, void *), void *);
extern tree dfs_walk_once (tree, tree (*) (tree, void *),
			   tree (*) (tree, void *), void *);
extern tree binfo_via_virtual			(tree, tree);
extern bool binfo_direct_p			(tree);
extern tree build_baselink			(tree, tree, tree, tree);
extern tree adjust_result_of_qualified_name_lookup
						(tree, tree, tree);
extern tree copied_binfo			(tree, tree);
extern tree original_binfo			(tree, tree);
extern bool shared_member_p			(tree);
extern bool any_dependent_bases_p (tree = current_nonlambda_class_type ());
extern bool maybe_check_overriding_exception_spec (tree, tree);

/* in semantics.cc */
extern void push_deferring_access_checks	(deferring_kind);
extern void resume_deferring_access_checks	(void);
extern void stop_deferring_access_checks	(void);
extern void pop_deferring_access_checks		(void);
extern vec<deferred_access_check, va_gc> *get_deferred_access_checks (void);
extern void reopen_deferring_access_checks (vec<deferred_access_check, va_gc> *);
extern void pop_to_parent_deferring_access_checks (void);
extern bool perform_access_checks (vec<deferred_access_check, va_gc> *,
				   tsubst_flags_t);
extern bool perform_deferred_access_checks	(tsubst_flags_t);
extern bool perform_or_defer_access_check	(tree, tree, tree,
						 tsubst_flags_t,
						 access_failure_info *afi = NULL);
extern tree maybe_convert_cond (tree);

/* RAII sentinel to ensures that deferred access checks are popped before
  a function returns.  */

class deferring_access_check_sentinel
{
public:
  deferring_access_check_sentinel (enum deferring_kind kind = dk_deferred)
  {
    push_deferring_access_checks (kind);
  }
  ~deferring_access_check_sentinel ()
  {
    pop_deferring_access_checks ();
  }
};

extern int stmts_are_full_exprs_p		(void);
extern void init_cp_semantics			(void);
extern tree do_poplevel				(tree);
extern void break_maybe_infinite_loop		(void);
extern void add_decl_expr			(tree);
extern tree maybe_cleanup_point_expr_void	(tree);
extern tree finish_expr_stmt			(tree);
extern tree begin_if_stmt			(void);
extern tree finish_if_stmt_cond			(tree, tree);
extern tree finish_then_clause			(tree);
extern void begin_else_clause			(tree);
extern void finish_else_clause			(tree);
extern void finish_if_stmt			(tree);
extern tree begin_while_stmt			(void);
extern void finish_while_stmt_cond	(tree, tree, bool, tree, bool);
extern void finish_while_stmt			(tree);
extern tree begin_do_stmt			(void);
extern void finish_do_body			(tree);
extern void finish_do_stmt		(tree, tree, bool, tree, bool);
extern tree finish_return_stmt			(tree);
extern tree begin_for_scope			(tree *);
extern tree begin_for_stmt			(tree, tree);
extern void finish_init_stmt			(tree);
extern void finish_for_cond		(tree, tree, bool, tree, bool);
extern void finish_for_expr			(tree, tree);
extern void find_range_for_decls		(tree[3]);
extern void finish_for_stmt			(tree);
extern tree begin_range_for_stmt		(tree, tree);
extern void finish_range_for_decl		(tree, tree, tree);
extern void finish_range_for_stmt		(tree);
extern tree finish_break_stmt			(void);
extern tree finish_continue_stmt		(void);
extern tree begin_switch_stmt			(void);
extern void finish_switch_cond			(tree, tree);
extern void finish_switch_stmt			(tree);
extern tree finish_goto_stmt			(tree);
extern tree begin_try_block			(void);
extern void finish_try_block			(tree);
extern void finish_handler_sequence		(tree);
extern tree begin_function_try_block		(tree *);
extern void finish_function_try_block		(tree);
extern void finish_function_handler_sequence    (tree, tree);
extern void finish_cleanup_try_block		(tree);
extern tree begin_handler			(void);
extern void finish_handler_parms		(tree, tree);
extern void finish_handler			(tree);
extern void finish_cleanup			(tree, tree);
extern bool is_this_parameter                   (tree);
extern bool is_object_parameter                 (tree);

enum {
  BCS_NORMAL = 0,
  BCS_NO_SCOPE = 1,
  BCS_TRY_BLOCK = 2,
  BCS_FN_BODY = 4,
  BCS_TRANSACTION = 8,
  BCS_STMT_EXPR = 16
};
extern tree begin_compound_stmt			(unsigned int);

extern void finish_compound_stmt		(tree);
extern tree finish_asm_string_expression	(location_t, tree);
extern tree finish_asm_stmt			(location_t, int, tree, tree,
						 tree, tree, tree, bool, bool);
extern tree finish_label_stmt			(tree);
extern void finish_label_decl			(tree);
extern cp_expr finish_parenthesized_expr	(cp_expr);
extern tree force_paren_expr			(tree, bool = false);
inline tree force_paren_expr_uneval 		(tree t)
{ return force_paren_expr (t, true); }
extern tree maybe_undo_parenthesized_ref	(tree);
extern tree finish_non_static_data_member       (tree, tree, tree,
						 tsubst_flags_t = tf_warning_or_error);
extern tree begin_stmt_expr			(void);
extern tree finish_stmt_expr_expr		(tree, tree);
extern tree finish_stmt_expr			(tree, bool);
extern tree stmt_expr_value_expr		(tree);
bool empty_expr_stmt_p				(tree);
extern cp_expr perform_koenig_lookup		(cp_expr, vec<tree, va_gc> *,
						 tsubst_flags_t);
extern tree finish_call_expr			(tree, vec<tree, va_gc> **, bool,
						 bool, tsubst_flags_t);
extern tree lookup_and_finish_template_variable (tree, tree, tsubst_flags_t = tf_warning_or_error);
extern tree finish_template_variable		(tree, tsubst_flags_t = tf_warning_or_error);
extern cp_expr finish_increment_expr		(cp_expr, enum tree_code);
extern tree finish_this_expr			(void);
extern tree finish_pseudo_destructor_expr       (tree, tree, tree, location_t, tsubst_flags_t);
extern cp_expr finish_unary_op_expr		(location_t, enum tree_code, cp_expr,
						 tsubst_flags_t);
/* Whether this call to finish_compound_literal represents a C++11 functional
   cast or a C99 compound literal.  */
enum fcl_t { fcl_functional, fcl_c99 };
extern tree finish_compound_literal		(tree, tree, tsubst_flags_t, fcl_t = fcl_functional);
extern tree finish_fname			(tree);
extern void finish_translation_unit		(void);
extern tree finish_template_type_parm		(tree, tree);
extern tree finish_template_template_parm       (tree, tree);
extern tree begin_class_definition		(tree);
extern void finish_template_decl		(tree);
extern tree finish_template_type		(tree, tree, int);
extern tree finish_base_specifier		(tree, tree, bool);
extern void finish_member_declaration		(tree);
extern bool outer_automatic_var_p		(tree);
extern tree process_outer_var_ref		(tree, tsubst_flags_t, bool force_use = false);
extern cp_expr finish_id_expression		(tree, tree, tree,
						 cp_id_kind *,
						 bool, bool, bool *,
						 bool, bool, bool, bool,
						 const char **,
                                                 location_t);
extern tree finish_typeof			(tree);
extern tree finish_underlying_type	        (tree);
extern tree calculate_bases                     (tree, tsubst_flags_t);
extern tree finish_bases                        (tree, bool);
extern tree calculate_direct_bases              (tree, tsubst_flags_t);
extern tree pack_index_element			(tree, tree, bool,
						 tsubst_flags_t);
extern tree finish_offsetof			(tree, tree, location_t);
extern void finish_decl_cleanup			(tree, tree);
extern void finish_eh_cleanup			(tree);
extern void emit_associated_thunks		(tree);
extern void finish_mem_initializers		(tree);
extern tree check_template_template_default_arg (tree);
extern bool expand_or_defer_fn_1		(tree);
extern void expand_or_defer_fn			(tree);
extern bool check_accessibility_of_qualified_id (tree, tree, tree, tsubst_flags_t);
extern tree finish_qualified_id_expr		(tree, tree, bool, bool,
						 bool, bool, tsubst_flags_t);
extern void simplify_aggr_init_expr		(tree *);
extern void finalize_nrv			(tree, tree);
extern tree omp_reduction_id			(enum tree_code, tree, tree);
extern tree omp_mapper_id			(tree, tree);
extern tree cp_remove_omp_priv_cleanup_stmt	(tree *, int *, void *);
extern bool cp_check_omp_declare_reduction	(tree);
extern bool cp_check_omp_declare_mapper		(tree);
extern void finish_omp_declare_simd_methods	(tree);
extern tree cp_finish_omp_init_prefer_type	(tree);
extern tree finish_omp_clauses			(tree, enum c_omp_region_type);
extern tree omp_instantiate_mappers		(tree);
extern tree push_omp_privatization_clauses	(bool);
extern void pop_omp_privatization_clauses	(tree);
extern void save_omp_privatization_clauses	(vec<tree> &);
extern void restore_omp_privatization_clauses	(vec<tree> &);
extern void finish_omp_threadprivate		(tree);
extern tree begin_omp_structured_block		(void);
extern tree finish_omp_structured_block		(tree);
extern tree finish_oacc_data			(tree, tree);
extern tree finish_oacc_host_data		(tree, tree);
extern tree finish_omp_construct		(enum tree_code, tree, tree);
extern tree begin_omp_parallel			(void);
extern tree finish_omp_parallel			(tree, tree);
extern tree begin_omp_task			(void);
extern tree finish_omp_task			(tree, tree);
extern tree finish_omp_for			(location_t, enum tree_code,
						 tree, tree, tree, tree, tree,
						 tree, tree, vec<tree> *, tree);
extern tree finish_omp_for_block		(tree, tree);
extern void finish_omp_atomic			(location_t, enum tree_code,
						 enum tree_code, tree, tree,
						 tree, tree, tree, tree, tree,
						 enum omp_memory_order, bool);
extern void finish_omp_barrier			(void);
extern void finish_omp_depobj			(location_t, tree,
						 enum omp_clause_depend_kind,
						 tree);
extern void finish_omp_flush			(int);
extern void finish_omp_taskwait			(void);
extern void finish_omp_taskyield		(void);
extern void finish_omp_cancel			(tree);
extern void finish_omp_cancellation_point	(tree);
extern tree omp_privatize_field			(tree, bool);
extern tree begin_transaction_stmt		(location_t, tree *, int);
extern void finish_transaction_stmt		(tree, tree, int, tree);
extern tree build_transaction_expr		(location_t, tree, int, tree);
extern bool cxx_omp_create_clause_info		(tree, tree, bool, bool,
						 bool, bool);
extern tree baselink_for_fns                    (tree);
extern void finish_static_assert                (tree, tree, location_t,
						 bool, bool, bool = false);
extern tree finish_decltype_type                (tree, bool, tsubst_flags_t);
extern tree fold_builtin_is_corresponding_member (location_t, int, tree *);
extern tree fold_builtin_is_pointer_inverconvertible_with_class (location_t, int, tree *);
extern tree finish_trait_expr			(location_t, enum cp_trait_kind, tree, tree);
extern tree finish_trait_type			(enum cp_trait_kind, tree, tree, tsubst_flags_t);
extern tree build_lambda_expr                   (void);
extern tree build_lambda_object			(tree);
extern tree begin_lambda_type                   (tree);
extern tree lambda_capture_field_type		(tree, bool, bool);
extern tree lambda_proxy_type			(tree);
extern tree lambda_function			(tree);
extern void apply_deduced_return_type           (tree, tree);
extern tree add_capture                         (tree, tree, tree, bool, bool, unsigned *);
extern tree add_default_capture                 (tree, tree, tree);
extern void insert_capture_proxy		(tree);
extern void insert_pending_capture_proxies	(void);
extern bool is_capture_proxy			(tree);
extern bool is_normal_capture_proxy             (tree);
extern tree strip_normal_capture_proxy		(tree);
extern bool is_constant_capture_proxy           (tree);
extern void register_capture_members		(tree);
extern tree lambda_expr_this_capture            (tree, int);
extern void maybe_generic_this_capture		(tree, tree);
extern tree maybe_resolve_dummy			(tree, bool);
extern tree current_nonlambda_function		(bool = false);
extern tree nonlambda_method_basetype		(void);
extern tree current_nonlambda_scope		(bool = false);
extern tree current_lambda_expr			(void);
extern bool generic_lambda_fn_p			(tree);
extern tree do_dependent_capture		(tree, bool = false);
extern bool lambda_fn_in_template_p		(tree);
extern void maybe_add_lambda_conv_op            (tree);
extern bool is_lambda_ignored_entity            (tree);
extern bool lambda_static_thunk_p		(tree);
extern bool call_from_lambda_thunk_p		(tree);
extern tree finish_builtin_launder		(location_t, tree,
						 tsubst_flags_t);
extern tree cp_build_vec_convert		(tree, location_t, tree,
						 tsubst_flags_t);
extern tree cp_build_bit_cast			(location_t, tree, tree,
						 tsubst_flags_t);
extern void start_lambda_scope			(tree decl);
extern void finish_lambda_scope			(void);
extern void record_lambda_scope			(tree lambda);
extern void record_lambda_scope_discriminator	(tree lambda);
extern void record_lambda_scope_sig_discriminator (tree lambda, tree fn);
extern tree start_lambda_function		(tree fn, tree lambda_expr);
extern void finish_lambda_function		(tree body);
extern bool regenerated_lambda_fn_p		(tree);
extern tree lambda_regenerating_args		(tree);
extern tree most_general_lambda			(tree);
extern tree finish_omp_target			(location_t, tree, tree, bool);
extern void finish_omp_target_clauses		(location_t, tree, tree *);
extern void maybe_warn_unparenthesized_assignment (tree, bool, tsubst_flags_t);
extern tree cp_check_pragma_unroll		(location_t, tree);

/* in tree.cc */
extern int cp_tree_operand_length		(const_tree);
extern int cp_tree_code_length			(enum tree_code);
extern void cp_free_lang_data 			(tree t);
extern tree force_target_expr			(tree, tree, tsubst_flags_t);
extern tree build_target_expr_with_type		(tree, tree, tsubst_flags_t);
extern void lang_check_failed			(const char *, int,
						 const char *) ATTRIBUTE_NORETURN
						 ATTRIBUTE_COLD;
extern tree stabilize_expr			(tree, tree *);
extern void stabilize_call			(tree, tree *);
extern bool stabilize_init			(tree, tree *);
extern tree add_stmt_to_compound		(tree, tree);
extern void init_tree				(void);
extern bool pod_type_p				(const_tree);
extern bool layout_pod_type_p			(const_tree);
extern bool std_layout_type_p			(const_tree);
extern bool trivial_type_p			(const_tree);
extern bool trivially_relocatable_type_p	(tree);
extern bool replaceable_type_p			(tree);
extern bool trivially_copyable_p		(const_tree);
extern bool type_has_unique_obj_representations (const_tree);
extern bool scalarish_type_p			(const_tree);
extern bool structural_type_p			(tree, bool = false);
extern bool type_has_nontrivial_default_init	(const_tree);
extern bool type_has_nontrivial_copy_init	(const_tree);
extern void maybe_warn_parm_abi			(tree, location_t);
extern bool class_tmpl_impl_spec_p		(const_tree);
extern int zero_init_p				(const_tree);
extern bool zero_init_expr_p			(tree);
extern bool check_abi_tag_redeclaration		(const_tree, const_tree,
						 const_tree);
extern bool check_abi_tag_args			(tree, tree);
extern tree strip_typedefs			(tree, bool * = NULL,
						 unsigned int = 0);
extern tree strip_typedefs_expr			(tree, bool * = NULL,
						 unsigned int = 0);
extern tree copy_binfo				(tree, tree, tree,
						 tree *, int);
extern int member_p				(const_tree);
extern cp_lvalue_kind real_lvalue_p		(const_tree);
extern cp_lvalue_kind lvalue_kind		(const_tree);
extern bool glvalue_p				(const_tree);
extern bool obvalue_p				(const_tree);
extern bool xvalue_p	                        (const_tree);
extern bool bitfield_p				(const_tree);
extern bool non_mergeable_glvalue_p		(const_tree);
extern tree cp_stabilize_reference		(tree);
extern bool builtin_valid_in_constant_expr_p    (const_tree);
extern tree build_min				(enum tree_code, tree, ...);
extern tree build_min_nt_loc			(location_t, enum tree_code,
						 ...);
extern tree build_min_non_dep			(enum tree_code, tree, ...);
extern tree build_min_non_dep_op_overload	(enum tree_code, tree, tree, ...);
extern tree build_min_non_dep_op_overload	(tree, tree, tree,
						 vec<tree, va_gc> *);
extern tree build_min_nt_call_vec (tree, vec<tree, va_gc> *);
extern tree build_min_non_dep_call_vec		(tree, tree, vec<tree, va_gc> *);
extern vec<tree, va_gc>* vec_copy_and_insert    (vec<tree, va_gc>*, tree, unsigned);
extern tree build_cplus_new			(tree, tree, tsubst_flags_t);
extern tree build_local_temp			(tree);
extern bool is_local_temp			(tree);
extern tree build_aggr_init_expr		(tree, tree);
extern tree get_target_expr			(tree,
						 tsubst_flags_t = tf_warning_or_error);
extern tree get_internal_target_expr		(tree);
extern tree build_cplus_array_type		(tree, tree, int is_dep = -1);
extern tree build_array_of_n_type		(tree, unsigned HOST_WIDE_INT);
extern bool array_of_runtime_bound_p		(tree);
extern bool vla_type_p				(tree);
extern tree build_array_copy			(tree);
extern tree build_vec_init_expr			(tree, tree, tsubst_flags_t);
extern tree expand_vec_init_expr		(tree, tree, tsubst_flags_t,
						 vec<tree,va_gc>** = nullptr);
extern void diagnose_non_constexpr_vec_init	(tree);
extern tree hash_tree_cons			(tree, tree, tree);
extern tree hash_tree_chain			(tree, tree);
extern tree build_qualified_name		(tree, tree, tree, bool);
extern tree build_ref_qualified_type		(tree, cp_ref_qualifier);
extern tree make_binding_vec			(tree, unsigned clusters CXX_MEM_STAT_INFO);
inline tree ovl_first				(tree) ATTRIBUTE_PURE;
extern tree ovl_make				(tree fn,
						 tree next = NULL_TREE);
extern tree ovl_insert				(tree fn, tree maybe_ovl,
						 int using_or_hidden = 0);
extern tree ovl_skip_hidden			(tree) ATTRIBUTE_PURE;
extern void lookup_mark				(tree lookup, bool val);
extern tree lookup_add				(tree fns, tree lookup);
extern tree lookup_maybe_add			(tree fns, tree lookup,
						 bool deduping);
extern int is_overloaded_fn			(tree) ATTRIBUTE_PURE;
extern bool really_overloaded_fn		(tree) ATTRIBUTE_PURE;
extern tree dependent_name			(tree);
extern tree call_expr_dependent_name		(tree);
extern tree maybe_get_fns			(tree) ATTRIBUTE_PURE;
extern tree get_fns				(tree) ATTRIBUTE_PURE;
extern tree get_first_fn			(tree) ATTRIBUTE_PURE;
extern tree ovl_scope				(tree);
extern const char *cxx_printable_name		(tree, int);
extern const char *cxx_printable_name_translate	(tree, int);
extern tree canonical_eh_spec			(tree);
extern tree build_cp_fntype_variant		(tree, cp_ref_qualifier, tree, bool);
extern tree build_exception_variant		(tree, tree);
extern void fixup_deferred_exception_variants   (tree, tree);
extern tree bind_template_template_parm		(tree, tree);
extern tree array_type_nelts_total		(tree);
extern bool array_of_unknown_bound_p		(const_tree);
extern tree break_out_target_exprs		(tree, bool = false);
extern tree build_ctor_subob_ref		(tree, tree, tree);
extern tree replace_placeholders		(tree, tree, bool * = NULL);
extern bool find_placeholders			(tree);
extern tree get_type_decl			(tree);
extern tree decl_namespace_context		(tree);
extern bool decl_anon_ns_mem_p			(tree);
extern bool decl_internal_context_p		(const_tree);
extern tree lvalue_type				(tree);
extern tree error_type				(tree);
extern int varargs_function_p			(const_tree);
extern bool cp_tree_equal			(tree, tree);
extern tree no_linkage_check			(tree, bool);
extern void debug_binfo				(tree);
extern tree build_dummy_object			(tree);
extern tree maybe_dummy_object			(tree, tree *);
extern bool is_dummy_object			(const_tree);
extern bool is_byte_access_type			(tree);
extern bool is_byte_access_type_not_plain_char	(tree);
extern const struct scoped_attribute_specs cxx_gnu_attribute_table;
extern const struct scoped_attribute_specs std_attribute_table;
extern const struct scoped_attribute_specs internal_attribute_table;
extern tree make_ptrmem_cst			(tree, tree);
extern tree cp_build_type_attribute_variant     (tree, tree);
extern tree cp_build_reference_type		(tree, bool);
extern tree move				(tree);
extern tree cp_build_qualified_type		(tree, int,
						 tsubst_flags_t = tf_warning_or_error);
extern bool cv_qualified_p			(const_tree);
extern tree cv_unqualified			(tree);
extern special_function_kind special_function_p (const_tree);
extern special_function_kind special_memfn_p	(const_tree);
extern int count_trees				(tree);
extern int char_type_p				(tree);
extern void verify_stmt_tree			(tree);
extern linkage_kind decl_linkage		(tree);
extern duration_kind decl_storage_duration	(tree);
extern tree cp_walk_subtrees (tree*, int*, walk_tree_fn,
			      void*, hash_set<tree> *);
#define cp_walk_tree(tp,func,data,pset) \
	walk_tree_1 (tp, func, data, pset, cp_walk_subtrees)
#define cp_walk_tree_without_duplicates(tp,func,data) \
	walk_tree_without_duplicates_1 (tp, func, data, cp_walk_subtrees)
extern tree rvalue				(tree);
extern tree convert_bitfield_to_declared_type   (tree);
extern tree cp_save_expr			(tree);
extern bool cast_valid_in_integral_constant_expression_p (tree);
extern bool cxx_type_hash_eq			(const_tree, const_tree);
extern tree cxx_copy_lang_qualifiers		(const_tree, const_tree);

extern void cxx_print_statistics		(void);
extern bool maybe_warn_zero_as_null_pointer_constant (tree, location_t);

/* in ptree.cc */
extern void cxx_print_xnode			(FILE *, tree, int);
extern void cxx_print_decl			(FILE *, tree, int);
extern void cxx_print_type			(FILE *, tree, int);
extern void cxx_print_identifier		(FILE *, tree, int);
extern void cxx_print_error_function		(diagnostics::text_sink &,
						 const char *,
						 const diagnostics::diagnostic_info *);

/* in typeck.cc */
/* Says how we should behave when comparing two arrays one of which
   has unknown bounds.  */
enum compare_bounds_t { bounds_none, bounds_either, bounds_first };

extern bool cxx_mark_addressable		(tree, bool = false);
extern int string_conv_p			(const_tree, const_tree, int);
extern tree cp_truthvalue_conversion		(tree, tsubst_flags_t);
extern tree contextual_conv_bool		(tree, tsubst_flags_t);
extern tree condition_conversion		(tree);
extern tree require_complete_type		(tree,
						 tsubst_flags_t = tf_warning_or_error);
extern tree complete_type			(tree);
extern tree complete_type_or_else		(tree, tree);
extern tree complete_type_or_maybe_complain	(tree, tree, tsubst_flags_t);
extern int cp_compare_floating_point_conversion_ranks (tree, tree);
inline bool type_unknown_p			(const_tree);
enum { ce_derived, ce_type, ce_normal, ce_exact };
extern bool comp_except_specs			(const_tree, const_tree, int);
extern bool comptypes				(tree, tree, int);
extern bool same_type_ignoring_top_level_qualifiers_p (tree, tree);
extern bool similar_type_p			(tree, tree);
extern bool cp_comp_parm_types			(tree, tree);
extern bool next_common_initial_sequence	(tree &, tree &);
extern bool layout_compatible_type_p		(tree, tree);
extern bool compparms				(const_tree, const_tree);
extern int comp_cv_qualification		(const_tree, const_tree);
extern int comp_cv_qualification		(int, int);
extern int comp_cv_qual_signature		(tree, tree);
extern tree cxx_sizeof_or_alignof_expr		(location_t, tree,
						 enum tree_code, bool, bool);
extern tree cxx_sizeof_or_alignof_type		(location_t, tree,
						 enum tree_code, bool, bool);
extern tree cxx_alignas_expr                    (tree);
extern tree cxx_sizeof_nowarn                   (tree);
extern tree is_bitfield_expr_with_lowered_type  (const_tree);
extern tree unlowered_expr_type                 (const_tree);
extern tree decay_conversion			(tree,
                                                 tsubst_flags_t,
                                                 bool = true);
extern tree build_class_member_access_expr      (cp_expr, tree, tree, bool,
						 tsubst_flags_t);
extern tree finish_class_member_access_expr     (cp_expr, tree, bool,
						 tsubst_flags_t);
extern tree lookup_destructor			(tree, tree, tree, tsubst_flags_t);
extern tree build_dependent_operator_type	(tree, enum tree_code, bool);
extern tree build_x_indirect_ref		(location_t, tree,
						 ref_operator, tree,
						 tsubst_flags_t);
extern tree cp_build_indirect_ref		(location_t, tree,
						 ref_operator,
						 tsubst_flags_t);
extern tree cp_build_fold_indirect_ref		(tree);
extern tree build_array_ref			(location_t, tree, tree);
extern tree cp_build_array_ref			(location_t, tree, tree,
						 tsubst_flags_t);
extern tree get_member_function_from_ptrfunc	(tree *, tree, tsubst_flags_t);
extern tree cp_build_function_call_nary         (tree, tsubst_flags_t, ...)
						ATTRIBUTE_SENTINEL;
extern tree cp_build_function_call_vec		(tree, vec<tree, va_gc> **,
						 tsubst_flags_t,
						 tree = NULL_TREE);
extern tree build_x_binary_op			(const op_location_t &,
						 enum tree_code, tree,
						 enum tree_code, tree,
						 enum tree_code, tree,
						 tree *, tsubst_flags_t);
inline tree build_x_binary_op (const op_location_t &loc,
			       enum tree_code code, tree arg1, tree arg2,
			       tsubst_flags_t complain)
{
  return build_x_binary_op (loc, code, arg1, TREE_CODE (arg1), arg2,
			    TREE_CODE (arg2), NULL_TREE, NULL, complain);
}
extern tree build_x_array_ref			(location_t, tree, tree,
						 tsubst_flags_t);
extern tree build_omp_array_section		(location_t, tree, tree, tree);
extern tree build_x_unary_op			(location_t,
						 enum tree_code, cp_expr,
						 tree, tsubst_flags_t);
extern tree cp_build_addressof			(location_t, tree,
						 tsubst_flags_t);
extern tree cp_build_addr_expr			(tree, tsubst_flags_t);
extern tree cp_build_unary_op                   (enum tree_code, tree, bool,
                                                 tsubst_flags_t);
extern tree genericize_compound_lvalue		(tree);
extern tree unary_complex_lvalue		(enum tree_code, tree);
extern tree build_x_conditional_expr		(location_t, tree, tree, tree,
                                                 tsubst_flags_t);
extern tree build_x_compound_expr_from_list	(tree, expr_list_kind,
						 tsubst_flags_t);
extern tree build_x_compound_expr_from_vec	(vec<tree, va_gc> *,
						 const char *, tsubst_flags_t);
extern tree build_x_compound_expr		(location_t, tree, tree,
						 tree, tsubst_flags_t);
extern tree build_compound_expr                 (location_t, tree, tree);
extern tree cp_build_compound_expr		(tree, tree, tsubst_flags_t);
extern tree build_static_cast			(location_t, tree, tree,
						 tsubst_flags_t);
extern tree build_reinterpret_cast		(location_t, tree, tree,
						 tsubst_flags_t);
extern tree build_const_cast			(location_t, tree, tree,
						 tsubst_flags_t);
extern tree build_c_cast			(location_t, tree, tree);
extern cp_expr build_c_cast			(location_t loc, tree type,
						 cp_expr expr);
extern tree cp_build_c_cast			(location_t, tree, tree,
						 tsubst_flags_t);
extern bool maybe_warn_self_move		(location_t, tree, tree);
extern cp_expr build_x_modify_expr		(location_t, tree,
						 enum tree_code, tree,
						 tree, tsubst_flags_t);
extern tree cp_build_modify_expr		(location_t, tree,
						 enum tree_code, tree,
						 tsubst_flags_t);
extern tree convert_for_initialization		(tree, tree, tree, int,
						 impl_conv_rhs, tree, int,
                                                 tsubst_flags_t);
extern int comp_ptr_ttypes			(tree, tree);
extern bool comp_ptr_ttypes_const		(tree, tree, compare_bounds_t);
extern bool error_type_p			(const_tree);
extern bool ptr_reasonably_similar		(const_tree, const_tree);
extern tree build_ptrmemfunc			(tree, tree, int, bool,
						 tsubst_flags_t);
extern int cp_type_quals			(const_tree);
extern int type_memfn_quals			(const_tree);
extern cp_ref_qualifier type_memfn_rqual	(const_tree);
extern tree apply_memfn_quals			(tree, cp_cv_quals,
						 cp_ref_qualifier = REF_QUAL_NONE);
extern bool cp_has_mutable_p			(const_tree);
extern bool at_least_as_qualified_p		(const_tree, const_tree);
extern void cp_apply_type_quals_to_decl		(int, tree);
extern tree build_ptrmemfunc1			(tree, tree, tree);
extern void expand_ptrmemfunc_cst		(tree, tree *, tree *);
extern tree type_after_usual_arithmetic_conversions (tree, tree);
extern tree common_pointer_type                 (tree, tree);
extern tree composite_pointer_type		(const op_location_t &,
						 tree, tree, tree, tree,
						 composite_pointer_operation,
						 tsubst_flags_t);
extern tree merge_types				(tree, tree);
extern tree strip_array_domain			(tree);
extern tree check_return_expr			(tree, bool *, bool *);
extern tree spaceship_type			(tree, tsubst_flags_t = tf_warning_or_error);
extern tree genericize_spaceship		(location_t, tree, tree, tree);
extern tree cp_build_binary_op                  (const op_location_t &,
						 enum tree_code, tree, tree,
						 tsubst_flags_t);
extern tree build_x_vec_perm_expr               (location_t,
						 tree, tree, tree,
						 tsubst_flags_t);
extern tree build_x_shufflevector               (location_t,
						 vec<tree, va_gc> *,
						 tsubst_flags_t);
#define cxx_sizeof(T)  cxx_sizeof_or_alignof_type (input_location, T, SIZEOF_EXPR, false, true)
extern tree build_simple_component_ref		(tree, tree);
extern tree build_ptrmemfunc_access_expr	(tree, tree);
extern tree build_address			(tree);
extern tree build_nop				(tree, tree CXX_MEM_STAT_INFO);
extern tree non_reference			(tree);
extern tree lookup_anon_field			(tree, tree);
extern bool invalid_nonstatic_memfn_p		(location_t, tree,
						 tsubst_flags_t);
extern tree convert_member_func_to_ptr		(tree, tree, tsubst_flags_t);
extern tree convert_ptrmem			(tree, tree, bool, bool,
						 tsubst_flags_t);
extern int lvalue_or_else			(tree, enum lvalue_use,
                                                 tsubst_flags_t);
extern void check_template_keyword		(tree);
extern bool check_raw_literal_operator		(const_tree decl);
extern bool check_literal_operator_args		(const_tree, bool *, bool *);
extern void maybe_warn_about_useless_cast       (location_t, tree, tree,
						 tsubst_flags_t);
extern tree cp_perform_integral_promotions      (tree, tsubst_flags_t);

extern tree finish_left_unary_fold_expr      (location_t, tree, int);
extern tree finish_right_unary_fold_expr     (location_t, tree, int);
extern tree finish_binary_fold_expr          (location_t, tree, tree, int);
extern tree treat_lvalue_as_rvalue_p	     (tree, bool);
extern bool decl_in_std_namespace_p	     (tree);
extern void maybe_warn_pessimizing_move	     (tree, tree, bool);

/* in typeck2.cc */
extern void require_complete_eh_spec_types	(tree, tree);
extern bool cxx_incomplete_type_diagnostic	(location_t, const_tree,
						 const_tree, enum diagnostics::kind);
inline location_t
loc_or_input_loc (location_t loc)
{
  return loc == UNKNOWN_LOCATION ? input_location : loc;
}

/* Like EXPR_LOCATION, but also handle some tcc_exceptional that have
   locations.  */

inline location_t
cp_expr_location (const_tree t_)
{
  tree t = CONST_CAST_TREE (t_);
  if (t == NULL_TREE)
    return UNKNOWN_LOCATION;
  switch (TREE_CODE (t))
    {
    case LAMBDA_EXPR:
      return LAMBDA_EXPR_LOCATION (t);
    case STATIC_ASSERT:
      return STATIC_ASSERT_SOURCE_LOCATION (t);
    case TRAIT_EXPR:
      return TRAIT_EXPR_LOCATION (t);
    case PTRMEM_CST:
      return PTRMEM_CST_LOCATION (t);
    default:
      return EXPR_LOCATION (t);
    }
}

inline location_t
cp_expr_loc_or_loc (const_tree t, location_t or_loc)
{
  location_t loc = cp_expr_location (t);
  if (loc == UNKNOWN_LOCATION)
    loc = or_loc;
  return loc;
}

inline location_t
cp_expr_loc_or_input_loc (const_tree t)
{
  return cp_expr_loc_or_loc (t, input_location);
}

inline bool
cxx_incomplete_type_diagnostic (const_tree value, const_tree type,
				enum diagnostics::kind diag_kind)
{
  return cxx_incomplete_type_diagnostic (cp_expr_loc_or_input_loc (value),
					 value, type, diag_kind);
}

extern void cxx_incomplete_type_error		(location_t, const_tree,
						 const_tree);
inline void
cxx_incomplete_type_error (const_tree value, const_tree type)
{
  cxx_incomplete_type_diagnostic (value, type, diagnostics::kind::error);
}

extern void cxx_incomplete_type_inform 	        (const_tree);
extern tree error_not_base_type			(tree, tree);
extern tree binfo_or_else			(tree, tree);
extern void cxx_readonly_error			(location_t, tree,
						 enum lvalue_use);
extern void complete_type_check_abstract	(tree);
extern int abstract_virtuals_error		(tree, tree,
						 tsubst_flags_t = tf_warning_or_error);
extern int abstract_virtuals_error		(abstract_class_use, tree,
						 tsubst_flags_t = tf_warning_or_error);

extern tree store_init_value			(tree, tree, vec<tree, va_gc>**, int);
extern tree build_disable_temp_cleanup		(tree);
extern tree split_nonconstant_init		(tree, tree);
extern bool check_narrowing			(tree, tree, tsubst_flags_t,
						 bool = false);
extern bool ordinary_char_type_p		(tree);
extern bool array_string_literal_compatible_p	(tree, tree);
extern tree digest_init				(tree, tree, tsubst_flags_t);
extern tree digest_init_flags			(tree, tree, int, tsubst_flags_t);
extern tree digest_nsdmi_init		        (tree, tree, tsubst_flags_t);
extern tree build_scoped_ref			(tree, tree, tree *);
extern tree build_x_arrow			(location_t, tree,
						 tsubst_flags_t);
extern tree build_m_component_ref		(tree, tree, tsubst_flags_t);
extern tree build_functional_cast		(location_t, tree, tree,
						 tsubst_flags_t);
extern tree add_exception_specifier		(tree, tree, tsubst_flags_t);
extern tree merge_exception_specifiers		(tree, tree);
extern void set_target_expr_eliding		(tree);
extern tree cp_build_init_expr			(location_t, tree, tree);
inline tree cp_build_init_expr (tree t, tree i)
{ return cp_build_init_expr (input_location, t, i); }

/* in mangle.cc */
extern void init_mangle				(void);
extern void mangle_decl				(tree);
extern const char *mangle_type_string		(tree);
extern tree mangle_typeinfo_for_type		(tree);
extern tree mangle_typeinfo_string_for_type	(tree);
extern tree mangle_vtbl_for_type		(tree);
extern tree mangle_vtt_for_type			(tree);
extern tree mangle_ctor_vtbl_for_type		(tree, tree);
extern tree mangle_thunk			(tree, int, tree, tree, tree);
extern tree mangle_guard_variable		(tree);
extern tree mangle_tls_init_fn			(tree);
extern tree mangle_tls_wrapper_fn		(tree);
extern bool decl_tls_wrapper_p			(tree);
extern tree mangle_ref_init_variable		(tree);
extern tree mangle_template_parm_object		(tree);
extern char *get_mangled_vtable_map_var_name    (tree);
extern bool mangle_return_type_p		(tree);
extern tree mangle_decomp			(tree, vec<tree> &);
extern void mangle_module_substitution		(int);
extern int mangle_module_component		(tree id, bool partition);
extern tree mangle_module_global_init		(int);
extern unsigned HOST_WIDE_INT range_expr_nelts	(tree);
extern bool equal_abi_tags			(tree, tree);

/* in dump.cc */
extern bool cp_dump_tree			(void *, tree);

/* In cp/cp-objcp-common.cc.  */

extern alias_set_type cxx_get_alias_set		(tree);
extern bool cxx_warn_unused_global_decl		(const_tree);
extern size_t cp_tree_size			(enum tree_code);
extern bool cp_var_mod_type_p			(tree, tree);
extern void cxx_initialize_diagnostics		(diagnostics::context *);
extern int cxx_types_compatible_p		(tree, tree);
extern bool cxx_block_may_fallthru		(const_tree);

/* in cp-gimplify.cc */
extern int cp_gimplify_expr			(tree *, gimple_seq *,
						 gimple_seq *);
extern void cp_genericize			(tree);
extern bool cxx_omp_const_qual_no_mutable	(tree);
extern enum omp_clause_default_kind cxx_omp_predetermined_sharing_1 (tree);
extern enum omp_clause_default_kind cxx_omp_predetermined_sharing (tree);
extern enum omp_clause_defaultmap_kind cxx_omp_predetermined_mapping (tree);
extern tree cxx_omp_clause_default_ctor		(tree, tree, tree);
extern tree cxx_omp_clause_copy_ctor		(tree, tree, tree);
extern tree cxx_omp_clause_assign_op		(tree, tree, tree);
extern tree cxx_omp_clause_dtor			(tree, tree);
extern void cxx_omp_finish_clause		(tree, gimple_seq *, bool);
extern tree cxx_omp_finish_mapper_clauses	(tree);
extern tree cxx_omp_mapper_lookup		(tree, tree);
extern tree cxx_omp_extract_mapper_directive	(tree);
extern tree cxx_omp_map_array_section		(location_t, tree);
extern bool cxx_omp_privatize_by_reference	(const_tree);
extern bool cxx_omp_disregard_value_expr	(tree, bool);
extern void cp_fold_function			(tree);
extern tree cp_fold_maybe_rvalue		(tree, bool);
extern tree cp_fold_rvalue			(tree);
extern tree cp_fully_fold			(tree);
extern tree cp_fully_fold_init			(tree);
extern tree predeclare_vla			(tree);
extern void clear_fold_cache			(void);
extern tree lookup_hotness_attribute		(tree);
extern tree process_stmt_hotness_attribute	(tree, location_t);
extern tree build_assume_call			(location_t, tree);
extern tree process_stmt_assume_attribute	(tree, tree, location_t);
extern bool simple_empty_class_p		(tree, tree, tree_code);
extern tree fold_builtin_source_location	(const_tree);
extern tree get_source_location_impl_type	();
extern tree cp_fold_immediate			(tree *, mce_value,
						 tree = current_function_decl);
extern void process_and_check_pending_immediate_escalating_fns ();

/* in name-lookup.cc */
extern tree strip_using_decl                    (tree);
extern void diagnose_name_conflict		(tree, tree);
extern bool dependent_local_decl_p		(tree);

/* Tell the binding oracle what kind of binding we are looking for.  */

enum cp_oracle_request
{
  CP_ORACLE_IDENTIFIER
};

/* If this is non-NULL, then it is a "binding oracle" which can lazily
   create bindings when needed by the C compiler.  The oracle is told
   the name and type of the binding to create.  It can call pushdecl
   or the like to ensure the binding is visible; or do nothing,
   leaving the binding untouched.  c-decl.cc takes note of when the
   oracle has been called and will not call it again if it fails to
   create a given binding.  */

typedef void cp_binding_oracle_function (enum cp_oracle_request, tree identifier);

extern cp_binding_oracle_function *cp_binding_oracle;

/* Set during diagnostics to record the failed constraint. This is a
   TREE_LIST whose VALUE is the constraint and whose PURPOSE are the
   instantiation arguments Defined in pt.cc.  */

extern tree current_failed_constraint;

/* An RAII class to manage the failed constraint.  */

struct diagnosing_failed_constraint
{
  diagnosing_failed_constraint (tree, tree, bool);
  ~diagnosing_failed_constraint ();
  static bool replay_errors_p ();

  bool diagnosing_error;
};

/* in constraint.cc */

extern cp_expr finish_constraint_or_expr	(location_t, cp_expr, cp_expr);
extern cp_expr finish_constraint_and_expr	(location_t, cp_expr, cp_expr);
extern cp_expr finish_constraint_primary_expr	(cp_expr);
extern tree start_concept_definition		(cp_expr);
extern tree finish_concept_definition		(tree, tree, tree);
extern tree combine_constraint_expressions      (tree, tree);
extern tree append_constraint			(tree, tree);
extern tree get_constraints                     (const_tree);
extern void set_constraints                     (tree, tree);
extern void remove_constraints                  (tree);
extern tree current_template_constraints	(void);
extern tree associate_classtype_constraints     (tree);
extern tree build_constraints                   (tree, tree);
extern tree maybe_substitute_reqs_for		(tree, const_tree);
extern tree get_trailing_function_requirements	(tree);
extern tree get_shorthand_constraints           (tree);

extern tree build_concept_id			(tree);
extern tree build_type_constraint		(tree, tree, tsubst_flags_t);
extern tree build_concept_check                 (tree, tree, tsubst_flags_t);
extern tree build_concept_check                 (tree, tree, tree, tsubst_flags_t);

extern tree build_constrained_parameter         (tree, tree, tree = NULL_TREE);
extern bool equivalent_placeholder_constraints  (tree, tree);
extern hashval_t iterative_hash_placeholder_constraint	(tree, hashval_t);
extern tree finish_shorthand_constraint         (tree, tree);
extern tree finish_requires_expr                (location_t, tree, tree);
extern tree finish_simple_requirement           (location_t, tree);
extern tree finish_type_requirement             (location_t, tree);
extern tree finish_compound_requirement         (location_t, tree, tree, bool);
extern tree finish_nested_requirement           (location_t, tree);
extern tree tsubst_requires_expr                (tree, tree, tsubst_flags_t, tree);
extern tree evaluate_requires_expr		(tree);
extern tree tsubst_constraint                   (tree, tree, tsubst_flags_t, tree);
extern tree tsubst_constraint_info              (tree, tree, tsubst_flags_t, tree);
extern tree tsubst_parameter_mapping		(tree, tree, tsubst_flags_t, tree);

struct processing_constraint_expression_sentinel
{
  processing_constraint_expression_sentinel ();
  ~processing_constraint_expression_sentinel ();
};

extern bool processing_constraint_expression_p	();

extern tree get_concept_check_template		(tree);
extern tree evaluate_concept_check              (tree);
extern bool constraints_satisfied_p		(tree, tree = NULL_TREE);
extern bool* lookup_subsumption_result          (tree, tree);
extern bool save_subsumption_result             (tree, tree, bool);
extern tree find_template_parameters		(tree, tree);
extern bool equivalent_constraints              (tree, tree);
extern bool equivalently_constrained            (tree, tree);
extern bool strictly_subsumes			(tree, tree);
extern bool ttp_subsumes			(tree, tree);
extern int more_constrained                     (tree, tree);
extern bool at_least_as_constrained             (tree, tree);
extern bool constraints_equivalent_p            (tree, tree);
extern bool atomic_constraints_identical_p	(tree, tree);
extern hashval_t iterative_hash_constraint      (tree, hashval_t);
extern hashval_t hash_atomic_constraint         (tree);
extern void diagnose_trait_expr			(location_t, tree, tree);
extern bool maybe_diagnose_standard_trait	(location_t, tree);
extern void diagnose_constraints                (location_t, tree, tree);

extern void note_failed_type_completion		(tree, tsubst_flags_t);
extern location_t failed_completion_location	(tree);

/* in logic.cc */
extern bool subsumes                            (tree, tree);

/* In class.cc */
extern void set_current_access_from_decl (tree);
extern void cp_finish_injected_record_type (tree);

/* in vtable-class-hierarchy.cc */
extern void vtv_compute_class_hierarchy_transitive_closure (void);
extern void vtv_generate_init_routine           (void);
extern void vtv_save_class_info                 (tree);
extern void vtv_recover_class_info              (void);
extern void vtv_build_vtable_verify_fndecl      (void);

/* In constexpr.cc */
/* Representation of entries in the constexpr function definition table.  */

struct GTY((for_user)) constexpr_fundef {
  tree decl;
  tree body;
  tree parms;
  tree result;
};

extern void fini_constexpr			(void);
extern bool literal_type_p                      (tree);
extern void maybe_save_constexpr_fundef		(tree);
extern void register_constexpr_fundef		(const constexpr_fundef &);
extern constexpr_fundef *retrieve_constexpr_fundef	(tree);
extern bool is_valid_constexpr_fn		(tree, bool);
extern bool check_constexpr_ctor_body           (tree, tree, bool);
extern tree constexpr_fn_retval		(tree);
extern tree ensure_literal_type_for_constexpr_object (tree);
extern bool potential_constant_expression       (tree);
extern bool is_constant_expression (tree);
extern bool is_rvalue_constant_expression (tree);
extern bool is_nondependent_constant_expression (tree);
extern bool is_nondependent_static_init_expression (tree);
extern bool is_static_init_expression    (tree);
extern bool is_std_class (tree, const char *);
extern bool is_std_allocator (tree);
extern bool potential_rvalue_constant_expression (tree);
extern bool require_potential_constant_expression (tree);
extern bool require_constant_expression (tree);
extern bool require_rvalue_constant_expression (tree);
extern bool require_potential_rvalue_constant_expression (tree);
extern bool require_potential_rvalue_constant_expression_fncheck (tree);
extern tree cxx_constant_value			(tree, tree = NULL_TREE,
						 tsubst_flags_t = tf_error);
inline tree cxx_constant_value (tree t, tsubst_flags_t complain)
{ return cxx_constant_value (t, NULL_TREE, complain); }
extern void cxx_constant_dtor			(tree, tree);
extern tree cxx_constant_init			(tree, tree = NULL_TREE);
extern tree maybe_constant_value		(tree, tree = NULL_TREE, mce_value = mce_unknown);
extern tree maybe_constant_init			(tree, tree = NULL_TREE, bool = false);
extern tree maybe_constant_init			(tree, tree, mce_value);
extern tree fold_non_dependent_expr		(tree,
						 tsubst_flags_t = tf_warning_or_error,
						 bool = false, tree = NULL_TREE);
extern tree maybe_fold_non_dependent_expr	(tree,
						 tsubst_flags_t = tf_warning_or_error);
extern tree fold_non_dependent_init		(tree,
						 tsubst_flags_t = tf_warning_or_error,
						 bool = false, tree = NULL_TREE);
extern tree fold_simple				(tree);
extern tree fold_to_constant			(tree);
extern bool reduced_constant_expression_p       (tree, tree = NULL_TREE);
extern bool is_instantiation_of_constexpr       (tree);
extern bool var_in_constexpr_fn                 (tree);
extern bool var_in_maybe_constexpr_fn           (tree);
extern bool maybe_constexpr_fn			(tree);
extern void explain_invalid_constexpr_fn        (tree);
extern vec<tree> cx_error_context               (void);
extern tree fold_sizeof_expr			(tree);
extern void clear_cv_and_fold_caches		(void);
extern tree unshare_constructor			(tree CXX_MEM_STAT_INFO);
extern bool decl_implicit_constexpr_p		(tree);
struct constexpr_ctx;
extern tree find_failing_clause			(const constexpr_ctx *ctx, tree);
extern void diagnose_failing_condition		(tree, location_t, bool,
						 const constexpr_ctx * = nullptr);
extern bool replace_decl			(tree *, tree, tree);

/* An RAII sentinel used to restrict constexpr evaluation so that it
   doesn't do anything that causes extra DECL_UID generation.  */

struct uid_sensitive_constexpr_evaluation_sentinel
{
  temp_override<bool> ovr;
  uid_sensitive_constexpr_evaluation_sentinel ();
};

/* Used to determine whether uid_sensitive_constexpr_evaluation_p was
   called and returned true, indicating that we've restricted constexpr
   evaluation in order to avoid UID generation.  We use this to control
   updates to the fold_cache and cv_cache.  */

struct uid_sensitive_constexpr_evaluation_checker
{
  const unsigned saved_counter;
  uid_sensitive_constexpr_evaluation_checker ();
  bool evaluation_restricted_p () const;
};

void cp_tree_c_finish_parsing ();

/* In cp-ubsan.cc */
extern void cp_ubsan_maybe_instrument_member_call (tree);
extern void cp_ubsan_instrument_member_accesses (tree *);
extern tree cp_ubsan_maybe_instrument_downcast	(location_t, tree, tree, tree);
extern tree cp_ubsan_maybe_instrument_cast_to_vbase (location_t, tree, tree);
extern void cp_ubsan_maybe_initialize_vtbl_ptrs (tree);

/* In coroutines.cc */
extern tree finish_co_return_stmt		(location_t, tree);
extern tree finish_co_await_expr		(location_t, tree);
extern tree finish_co_yield_expr		(location_t, tree);
extern tree coro_validate_builtin_call		(tree,
						 tsubst_flags_t = tf_warning_or_error);
extern tree coro_get_actor_function		(tree);
extern tree coro_get_destroy_function		(tree);
extern tree coro_get_ramp_function		(tree);

extern tree co_await_get_resume_call		(tree await_expr);


/* contracts.cc */
extern tree make_postcondition_variable		(cp_expr);
extern tree make_postcondition_variable		(cp_expr, tree);
extern tree grok_contract			(tree, tree, tree, cp_expr, location_t);
extern tree finish_contract_condition		(cp_expr);

/* Return the first contract in ATTRS, or NULL_TREE if there are none.  */

inline tree
find_contract (tree attrs)
{
  while (attrs && !cxx_contract_attribute_p (attrs))
    attrs = TREE_CHAIN (attrs);
  return attrs;
}

inline void
set_decl_contracts (tree decl, tree contract_attrs)
{
  remove_contract_attributes (decl);
  DECL_ATTRIBUTES (decl) = chainon (DECL_ATTRIBUTES (decl), contract_attrs);
}

/* Returns the computed semantic of the node.  */

inline contract_semantic
get_contract_semantic (const_tree t)
{
  return (contract_semantic) (TREE_LANG_FLAG_3 (CONTRACT_CHECK (t))
      | (TREE_LANG_FLAG_2 (t) << 1)
      | (TREE_LANG_FLAG_0 ((t)) << 2));
}

/* Sets the computed semantic of the node.  */

inline void
set_contract_semantic (tree t, contract_semantic semantic)
{
  TREE_LANG_FLAG_3 (CONTRACT_CHECK (t)) = semantic & 0x01;
  TREE_LANG_FLAG_2 (t) = (semantic & 0x02) >> 1;
  TREE_LANG_FLAG_0 (t) = (semantic & 0x04) >> 2;
}

/* Inline bodies.  */

inline tree
ovl_first (tree node)
{
  while (TREE_CODE (node) == OVERLOAD)
    node = OVL_FUNCTION (node);
  return node;
}

inline bool
type_unknown_p (const_tree expr)
{
  return TREE_TYPE (expr) == unknown_type_node;
}

inline hashval_t
named_decl_hash::hash (const value_type decl)
{
  tree name = (TREE_CODE (decl) == BINDING_VECTOR
	       ? BINDING_VECTOR_NAME (decl) : OVL_NAME (decl));
  return name ? IDENTIFIER_HASH_VALUE (name) : 0;
}

inline bool
named_decl_hash::equal (const value_type existing, compare_type candidate)
{
  tree name = (TREE_CODE (existing) == BINDING_VECTOR
	       ? BINDING_VECTOR_NAME (existing) : OVL_NAME (existing));
  return candidate == name;
}

inline bool
null_node_p (const_tree expr)
{
  STRIP_ANY_LOCATION_WRAPPER (expr);
  return expr == null_node;
}

/* True iff T is a variable template declaration.  */
inline bool
variable_template_p (tree t)
{
  if (TREE_CODE (t) != TEMPLATE_DECL)
    return false;
  if (!PRIMARY_TEMPLATE_P (t))
    return false;
  if (tree r = DECL_TEMPLATE_RESULT (t))
    return VAR_P (r);
  return false;
}

/* True iff T is a concept.  */

inline bool
concept_definition_p (const_tree t)
{
  return TREE_CODE (STRIP_TEMPLATE (t)) == CONCEPT_DECL;
}

/* True if t is an expression that checks a concept.  */

inline bool
concept_check_p (const_tree t)
{
  if (t && TREE_CODE (t) == TEMPLATE_ID_EXPR)
    return concept_definition_p (TREE_OPERAND (t, 0));
  return false;
}

/* Return the prototype parameter of the concept T,
   i.e. its first declared template parameter.  */

inline tree
concept_prototype_parameter (const_tree t)
{
  gcc_checking_assert (concept_definition_p (t));
  if (TREE_CODE (t) == CONCEPT_DECL)
    t = DECL_TI_TEMPLATE (t);
  tree parms = DECL_INNERMOST_TEMPLATE_PARMS (t);
  return TREE_VALUE (TREE_VEC_ELT (parms, 0));
}

/* Helpers for IMPLICIT_RVALUE_P to look through automatic dereference.  */

inline bool
implicit_rvalue_p (const_tree t)
{
  if (REFERENCE_REF_P (t))
    t = TREE_OPERAND (t, 0);
  return ((TREE_CODE (t) == NON_LVALUE_EXPR
	   || TREE_CODE (t) == STATIC_CAST_EXPR)
	  && IMPLICIT_RVALUE_P (t));
}
inline tree
set_implicit_rvalue_p (tree ot)
{
  tree t = ot;
  if (REFERENCE_REF_P (t))
    t = TREE_OPERAND (t, 0);
  IMPLICIT_RVALUE_P (t) = 1;
  return ot;
}

/* True if t is a "constrained auto" type-specifier.  */

inline bool
is_constrained_auto (const_tree t)
{
  return is_auto (t) && PLACEHOLDER_TYPE_CONSTRAINTS_INFO (t);
}

/* True if CODE, a tree code, denotes a tree whose operand is not evaluated
   as per [expr.context], i.e., an operand to sizeof, typeof, decltype, or
   alignof.  */

inline bool
unevaluated_p (tree_code code)
{
  return (code == DECLTYPE_TYPE
	  || code == ALIGNOF_EXPR
	  || code == SIZEOF_EXPR
	  || code == NOEXCEPT_EXPR
	  || code == REQUIRES_EXPR);
}

/* RAII class to push/pop the access scope for T.  */

struct push_access_scope_guard
{
  tree decl;
  push_access_scope_guard (tree t)
    : decl (t)
  {
    if (VAR_OR_FUNCTION_DECL_P (decl)
	|| TREE_CODE (decl) == TYPE_DECL)
      push_access_scope (decl);
    else
      decl = NULL_TREE;
  }
  ~push_access_scope_guard ()
  {
    if (decl)
      pop_access_scope (decl);
  }
};

/* Extracting strings from constexpr.  */

class cexpr_str
{
public:
  cexpr_str (tree message) : message (message) {}
  cexpr_str (const cexpr_str &) = delete;
  ~cexpr_str () { XDELETEVEC (buf); }

  bool type_check (location_t location);
  bool extract (location_t location, const char * & msg, int &len);
  bool extract (location_t location, tree &str);
  tree message;
private:
  tree message_data = NULL_TREE;
  tree message_sz = NULL_TREE;
  char *buf = nullptr;
};

/* True if TYPE is an extended floating-point type.  */

inline bool
extended_float_type_p (tree type)
{
  type = TYPE_MAIN_VARIANT (type);
  for (int i = 0; i < NUM_FLOATN_NX_TYPES; ++i)
    if (type == FLOATN_TYPE_NODE (i))
      return true;
  if (type == bfloat16_type_node)
    return true;
  return false;
}

/* True if DECL is name-independent declaration.  */

inline bool
name_independent_decl_p (tree decl)
{
  return ((VAR_P (decl) || TREE_CODE (decl) == FIELD_DECL)
	  && DECL_NAME (decl)
	  && id_equal (DECL_NAME (decl), "_")
	  && !TREE_STATIC (decl)
	  && !DECL_EXTERNAL (decl));
}

namespace highlight_colors {

/* Color names for highlighting "%qH" vs "%qI" values,
   and ranges corresponding to them.  */
extern const char *const percent_h;
extern const char *const percent_i;

} // namespace highlight_colors

#if CHECKING_P
namespace selftest {
  extern void run_cp_tests (void);

  /* Declarations for specific families of tests within cp,
     by source file, in alphabetical order.  */
  extern void cp_pt_cc_tests ();
  extern void cp_tree_cc_tests (void);
} // namespace selftest
#endif /* #if CHECKING_P */

/* -- end of C++ */

#endif /* ! GCC_CP_TREE_H */
