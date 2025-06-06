/* Supporting functions for resolving DATA statement.
   Copyright (C) 2002-2025 Free Software Foundation, Inc.
   Contributed by Lifang Zeng <zlf605@hotmail.com>

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


/* Notes for DATA statement implementation:

   We first assign initial value to each symbol by gfc_assign_data_value
   during resolving DATA statement. Refer to check_data_variable and
   traverse_data_list in resolve.cc.

   The complexity exists in the handling of array section, implied do
   and array of struct appeared in DATA statement.

   We call gfc_conv_structure, gfc_con_array_array_initializer,
   etc., to convert the initial value. Refer to trans-expr.cc and
   trans-array.cc.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "gfortran.h"
#include "data.h"
#include "constructor.h"

static void formalize_init_expr (gfc_expr *);

/* Calculate the array element offset.  */

static bool
get_array_index (gfc_array_ref *ar, mpz_t *offset)
{
  gfc_expr *e;
  int i;
  mpz_t delta;
  mpz_t tmp;
  bool ok = true;

  mpz_init (tmp);
  mpz_set_si (*offset, 0);
  mpz_init_set_si (delta, 1);
  for (i = 0; i < ar->dimen; i++)
    {
      e = gfc_copy_expr (ar->start[i]);
      gfc_simplify_expr (e, 1);

      if (!gfc_is_constant_expr (ar->as->lower[i])
	  || !gfc_is_constant_expr (ar->as->upper[i])
	  || !gfc_is_constant_expr (e))
	{
	  gfc_error ("non-constant array in DATA statement %L", &ar->where);
	  ok = false;
	  break;
	}

      mpz_set (tmp, e->value.integer);
      gfc_free_expr (e);

      /* Overindexing is only allowed as a legacy extension.  */
      if (mpz_cmp (tmp, ar->as->lower[i]->value.integer) < 0
	  && !gfc_notify_std (GFC_STD_LEGACY,
			      "Subscript at %L below array lower bound "
			      "(%ld < %ld) in dimension %d", &ar->c_where[i],
			      mpz_get_si (tmp),
			      mpz_get_si (ar->as->lower[i]->value.integer),
			      i+1))
	{
	  ok = false;
	  break;
	}
      if (mpz_cmp (tmp, ar->as->upper[i]->value.integer) > 0
	  && !gfc_notify_std (GFC_STD_LEGACY,
			      "Subscript at %L above array upper bound "
			      "(%ld > %ld) in dimension %d", &ar->c_where[i],
			      mpz_get_si (tmp),
			      mpz_get_si (ar->as->upper[i]->value.integer),
			      i+1))
	{
	  ok = false;
	  break;
	}

      mpz_sub (tmp, tmp, ar->as->lower[i]->value.integer);
      mpz_mul (tmp, tmp, delta);
      mpz_add (*offset, tmp, *offset);

      mpz_sub (tmp, ar->as->upper[i]->value.integer,
	       ar->as->lower[i]->value.integer);
      mpz_add_ui (tmp, tmp, 1);
      mpz_mul (delta, tmp, delta);
    }
  mpz_clear (delta);
  mpz_clear (tmp);

  return ok;
}

/* Find if there is a constructor which component is equal to COM.
   TODO: remove this, use symbol.cc(gfc_find_component) instead.  */

static gfc_constructor *
find_con_by_component (gfc_component *com, gfc_constructor_base base)
{
  gfc_constructor *c;

  for (c = gfc_constructor_first (base); c; c = gfc_constructor_next (c))
    if (com == c->n.component)
      return c;

  return NULL;
}


/* Create a character type initialization expression from RVALUE.
   TS [and REF] describe [the substring of] the variable being initialized.
   INIT is the existing initializer, not NULL.  Initialization is performed
   according to normal assignment rules.  */

static gfc_expr *
create_character_initializer (gfc_expr *init, gfc_typespec *ts,
			      gfc_ref *ref, gfc_expr *rvalue)
{
  HOST_WIDE_INT len, start, end, tlen;
  gfc_char_t *dest;
  bool alloced_init = false;

  if (init && init->ts.type != BT_CHARACTER)
    return NULL;

  gfc_extract_hwi (ts->u.cl->length, &len);

  if (init == NULL)
    {
      /* Create a new initializer.  */
      init = gfc_get_character_expr (ts->kind, NULL, NULL, len);
      init->ts = *ts;
      alloced_init = true;
    }

  dest = init->value.character.string;

  if (ref)
    {
      gfc_expr *start_expr, *end_expr;

      gcc_assert (ref->type == REF_SUBSTRING);

      /* Only set a substring of the destination.  Fortran substring bounds
	 are one-based [start, end], we want zero based [start, end).  */
      start_expr = gfc_copy_expr (ref->u.ss.start);
      end_expr = gfc_copy_expr (ref->u.ss.end);

      if ((!gfc_simplify_expr(start_expr, 1))
	  || !(gfc_simplify_expr(end_expr, 1)))
	{
	  gfc_error ("failure to simplify substring reference in DATA "
		     "statement at %L", &ref->u.ss.start->where);
	  gfc_free_expr (start_expr);
	  gfc_free_expr (end_expr);
	  if (alloced_init)
	    gfc_free_expr (init);
	  return NULL;
	}

      gfc_extract_hwi (start_expr, &start);
      gfc_free_expr (start_expr);
      start--;
      gfc_extract_hwi (end_expr, &end);
      gfc_free_expr (end_expr);
    }
  else
    {
      /* Set the whole string.  */
      start = 0;
      end = len;
    }

  /* Copy the initial value.  */
  if (rvalue->ts.type == BT_HOLLERITH)
    len = rvalue->representation.length - rvalue->ts.u.pad;
  else
    len = rvalue->value.character.length;

  tlen = end - start;
  if (len > tlen)
    {
      if (tlen < 0)
	{
	  gfc_warning_now (0, "Unused initialization string at %L because "
			   "variable has zero length", &rvalue->where);
	  len = 0;
	}
      else
	{
	  gfc_warning_now (0, "Initialization string at %L was truncated to "
			   "fit the variable (%wd/%wd)", &rvalue->where,
			   tlen, len);
	  len = tlen;
	}
    }

  if (start < 0)
    {
      gfc_error ("Substring start index at %L is less than one",
		 &ref->u.ss.start->where);
      return NULL;
    }
  if (end > init->value.character.length)
    {
      gfc_error ("Substring end index at %L exceeds the string length",
		 &ref->u.ss.end->where);
      return NULL;
    }

  if (rvalue->ts.type == BT_HOLLERITH)
    {
      for (size_t i = 0; i < (size_t) len; i++)
	dest[start+i] = rvalue->representation.string[i];
    }
  else
    memcpy (&dest[start], rvalue->value.character.string,
	    len * sizeof (gfc_char_t));

  /* Pad with spaces.  Substrings will already be blanked.  */
  if (len < tlen && ref == NULL)
    gfc_wide_memset (&dest[start + len], ' ', end - (start + len));

  if (rvalue->ts.type == BT_HOLLERITH)
    {
      init->representation.length = init->value.character.length;
      init->representation.string
	= gfc_widechar_to_char (init->value.character.string,
				init->value.character.length);
    }

  return init;
}


/* Assign the initial value RVALUE to  LVALUE's symbol->value. If the
   LVALUE already has an initialization, we extend this, otherwise we
   create a new one.  If REPEAT is non-NULL, initialize *REPEAT
   consecutive values in LVALUE the same value in RVALUE.  In that case,
   LVALUE must refer to a full array, not an array section.  */

bool
gfc_assign_data_value (gfc_expr *lvalue, gfc_expr *rvalue, mpz_t index,
		       mpz_t *repeat)
{
  gfc_ref *ref;
  gfc_expr *init;
  gfc_expr *expr = NULL;
  gfc_expr *rexpr;
  gfc_constructor *con;
  gfc_constructor *last_con;
  gfc_symbol *symbol;
  gfc_typespec *last_ts;
  mpz_t offset;

  symbol = lvalue->symtree->n.sym;
  init = symbol->value;
  last_ts = &symbol->ts;
  last_con = NULL;
  mpz_init_set_si (offset, 0);

  /* Find/create the parent expressions for subobject references.  */
  for (ref = lvalue->ref; ref; ref = ref->next)
    {
      /* Break out of the loop if we find a substring.  */
      if (ref->type == REF_SUBSTRING)
	{
	  /* A substring should always be the last subobject reference.  */
	  gcc_assert (ref->next == NULL);
	  break;
	}

      /* Use the existing initializer expression if it exists.  Otherwise
	 create a new one.  */
      if (init == NULL)
	expr = gfc_get_expr ();
      else
	expr = init;

      /* Find or create this element.  */
      switch (ref->type)
	{
	case REF_ARRAY:
	  if (ref->u.ar.as->rank == 0)
	    {
	      gcc_assert (ref->u.ar.as->corank > 0);
	      if (init == NULL)
		free (expr);
	      continue;
	    }

	  if (init && expr->expr_type != EXPR_ARRAY)
	    {
	      gfc_error ("%qs at %L already is initialized at %L",
			 lvalue->symtree->n.sym->name, &lvalue->where,
			 &init->where);
	      goto abort;
	    }

	  if (init == NULL)
	    {
	      /* The element typespec will be the same as the array
		 typespec.  */
	      expr->ts = *last_ts;
	      /* Setup the expression to hold the constructor.  */
	      expr->expr_type = EXPR_ARRAY;
	      expr->rank = ref->u.ar.as->rank;
	      expr->corank = ref->u.ar.as->corank;
	    }

	  if (ref->u.ar.type == AR_ELEMENT)
	    {
	      if (!get_array_index (&ref->u.ar, &offset))
		goto abort;
	    }
	  else
	    mpz_set (offset, index);

	  /* Check the bounds.  */
	  if (mpz_cmp_si (offset, 0) < 0)
	    {
	      gfc_error ("Data element below array lower bound at %L",
			 &lvalue->where);
	      goto abort;
	    }
	  else if (repeat != NULL
		   && ref->u.ar.type != AR_ELEMENT)
	    {
	      mpz_t size, end;
	      gcc_assert (ref->u.ar.type == AR_FULL
			  && ref->next == NULL);
	      mpz_init_set (end, offset);
	      mpz_add (end, end, *repeat);
	      if (spec_size (ref->u.ar.as, &size))
		{
		  if (mpz_cmp (end, size) > 0)
		    {
		      mpz_clear (size);
		      gfc_error ("Data element above array upper bound at %L",
				 &lvalue->where);
		      goto abort;
		    }
		  mpz_clear (size);
		}

	      con = gfc_constructor_lookup (expr->value.constructor,
					    mpz_get_si (offset));
	      if (!con)
		{
		  con = gfc_constructor_lookup_next (expr->value.constructor,
						     mpz_get_si (offset));
		  if (con != NULL && mpz_cmp (con->offset, end) >= 0)
		    con = NULL;
		}

	      /* Overwriting an existing initializer is non-standard but
		 usually only provokes a warning from other compilers.  */
	      if (con != NULL && con->expr != NULL)
		{
		  /* Order in which the expressions arrive here depends on
		     whether they are from data statements or F95 style
		     declarations.  Therefore, check which is the most
		     recent.  */
		  gfc_expr *exprd;
		  exprd = (linemap_location_before_p (line_table,
					 gfc_get_location (&con->expr->where),
					 gfc_get_location (&rvalue->where))
			   ? rvalue : con->expr);
		  if (gfc_notify_std (GFC_STD_GNU,
				      "re-initialization of %qs at %L",
				      symbol->name, &exprd->where) == false)
		    return false;
		}

	      while (con != NULL)
		{
		  gfc_constructor *next_con = gfc_constructor_next (con);

		  if (mpz_cmp (con->offset, end) >= 0)
		    break;
		  if (mpz_cmp (con->offset, offset) < 0)
		    {
		      gcc_assert (mpz_cmp_si (con->repeat, 1) > 0);
		      mpz_sub (con->repeat, offset, con->offset);
		    }
		  else if (mpz_cmp_si (con->repeat, 1) > 0
			   && mpz_get_si (con->offset)
			      + mpz_get_si (con->repeat) > mpz_get_si (end))
		    {
		      int endi;
		      splay_tree_node node
			= splay_tree_lookup (con->base,
					     mpz_get_si (con->offset));
		      gcc_assert (node
				  && con == (gfc_constructor *) node->value
				  && node->key == (splay_tree_key)
						  mpz_get_si (con->offset));
		      endi = mpz_get_si (con->offset)
			     + mpz_get_si (con->repeat);
		      if (endi > mpz_get_si (end) + 1)
			mpz_set_si (con->repeat, endi - mpz_get_si (end));
		      else
			mpz_set_si (con->repeat, 1);
		      mpz_set (con->offset, end);
		      node->key = (splay_tree_key) mpz_get_si (end);
		      break;
		    }
		  else
		    gfc_constructor_remove (con);
		  con = next_con;
		}

	      con = gfc_constructor_insert_expr (&expr->value.constructor,
						 NULL, &rvalue->where,
						 mpz_get_si (offset));
	      mpz_set (con->repeat, *repeat);
	      repeat = NULL;
	      mpz_clear (end);
	      break;
	    }
	  else
	    {
	      mpz_t size;
	      if (spec_size (ref->u.ar.as, &size))
		{
		  if (mpz_cmp (offset, size) >= 0)
		    {
		      mpz_clear (size);
		      gfc_error ("Data element above array upper bound at %L",
		                 &lvalue->where);
		      goto abort;
		    }
		  mpz_clear (size);
		}
	    }

	  con = gfc_constructor_lookup (expr->value.constructor,
					mpz_get_si (offset));
	  if (!con)
	    {
	      con = gfc_constructor_insert_expr (&expr->value.constructor,
						 NULL, &rvalue->where,
						 mpz_get_si (offset));
	    }
	  else if (mpz_cmp_si (con->repeat, 1) > 0)
	    {
	      /* Need to split a range.  */
	      if (mpz_cmp (con->offset, offset) < 0)
		{
		  gfc_constructor *pred_con = con;
		  con = gfc_constructor_insert_expr (&expr->value.constructor,
						     NULL, &con->where,
						     mpz_get_si (offset));
		  con->expr = gfc_copy_expr (pred_con->expr);
		  mpz_add (con->repeat, pred_con->offset, pred_con->repeat);
		  mpz_sub (con->repeat, con->repeat, offset);
		  mpz_sub (pred_con->repeat, offset, pred_con->offset);
		}
	      if (mpz_cmp_si (con->repeat, 1) > 0)
		{
		  gfc_constructor *succ_con;
		  succ_con
		    = gfc_constructor_insert_expr (&expr->value.constructor,
						   NULL, &con->where,
						   mpz_get_si (offset) + 1);
		  succ_con->expr = gfc_copy_expr (con->expr);
		  mpz_sub_ui (succ_con->repeat, con->repeat, 1);
		  mpz_set_si (con->repeat, 1);
		}
	    }
	  break;

	case REF_COMPONENT:
	  if (init == NULL)
	    {
	      /* Setup the expression to hold the constructor.  */
	      expr->expr_type = EXPR_STRUCTURE;
	      expr->ts.type = BT_DERIVED;
	      expr->ts.u.derived = ref->u.c.sym;
	    }
	  else
	    gcc_assert (expr->expr_type == EXPR_STRUCTURE);
	  last_ts = &ref->u.c.component->ts;

	  /* Find the same element in the existing constructor.  */
	  con = find_con_by_component (ref->u.c.component,
				       expr->value.constructor);

	  if (con == NULL)
	    {
	      /* Create a new constructor.  */
	      con = gfc_constructor_append_expr (&expr->value.constructor,
						 NULL, NULL);
	      con->n.component = ref->u.c.component;
	    }
	  break;

	case REF_INQUIRY:

	  /* After some discussion on clf it was determined that the following
	     violates F18(R841). If the error is removed, the expected result
	     is obtained. Leaving the code in place ensures a clean error
	     recovery.  */
	  gfc_error ("data-implied-do object at %L is neither an array-element "
		     "nor a scalar-structure-component (F2018: R841)",
		     &lvalue->where);

	  /* This breaks with the other reference types in that the output
	     constructor has to be of type COMPLEX, whereas the lvalue is
	     of type REAL.  The rvalue is copied to the real or imaginary
	     part as appropriate.  In addition, for all except scalar
	     complex variables, a complex expression has to provided, where
	     the constructor does not have it, and the expression modified
	     with a new value for the real or imaginary part.  */
	  gcc_assert (ref->next == NULL && last_ts->type == BT_COMPLEX);
	  rexpr = gfc_copy_expr (rvalue);
	  if (!gfc_compare_types (&lvalue->ts, &rexpr->ts))
	    gfc_convert_type (rexpr, &lvalue->ts, 0);

	  /* This is the scalar, complex case, where an initializer exists.  */
	  if (init && ref == lvalue->ref)
	    expr = symbol->value;
	  /* Then all cases, where a complex expression does not exist.  */
	  else if (!last_con || !last_con->expr)
	    {
	      expr = gfc_get_constant_expr (BT_COMPLEX, lvalue->ts.kind,
					    &lvalue->where);
	      if (last_con)
		last_con->expr = expr;
	    }
	  else
	    /* Finally, and existing constructor expression to be modified.  */
	    expr = last_con->expr;

	  /* Rejection of LEN and KIND inquiry references is handled
	     elsewhere. The error here is added as backup. The assertion
	     of F2008 for RE and IM is also done elsewhere.  */
	  switch (ref->u.i)
	    {
	    case INQUIRY_LEN:
	    case INQUIRY_KIND:
	      gfc_error ("LEN or KIND inquiry ref in DATA statement at %L",
			 &lvalue->where);
	      goto abort;
	    case INQUIRY_RE:
	      mpfr_set (mpc_realref (expr->value.complex),
			rexpr->value.real,
			GFC_RND_MODE);
	      break;
	    case INQUIRY_IM:
	      mpfr_set (mpc_imagref (expr->value.complex),
			rexpr->value.real,
			GFC_RND_MODE);
	      break;
	    }

	  /* Only the scalar, complex expression needs to be saved as the
	     symbol value since the last constructor expression is already
	     provided as the initializer in the code after the reference
	     cases.  */
	  if (ref == lvalue->ref)
	    symbol->value = expr;

	  gfc_free_expr (rexpr);
	  mpz_clear (offset);
	  return true;

	default:
	  gcc_unreachable ();
	}

      if (init == NULL)
	{
	  /* Point the container at the new expression.  */
	  if (last_con == NULL)
	    {
	      symbol->value = expr;
	      /* For a new initializer use the location from the
		 constructor as fallback.  */
	      if (!GFC_LOCUS_IS_SET(expr->where) && con != NULL)
		symbol->value->where = con->where;
	    }
	  else
	    last_con->expr = expr;
	}
      init = con->expr;
      last_con = con;
    }

  mpz_clear (offset);
  gcc_assert (repeat == NULL);

  /* Overwriting an existing initializer is non-standard but usually only
     provokes a warning from other compilers.  */
  if (init != NULL
      && GFC_LOCUS_IS_SET (init->where)
      && GFC_LOCUS_IS_SET (rvalue->where))
    {
      /* Order in which the expressions arrive here depends on whether
	 they are from data statements or F95 style declarations.
	 Therefore, check which is the most recent.  */
      expr = (linemap_location_before_p (line_table,
					 gfc_get_location (&init->where),
					 gfc_get_location (&rvalue->where))
	      ? rvalue : init);
      if (gfc_notify_std (GFC_STD_GNU, "re-initialization of %qs at %L",
			  symbol->name, &expr->where) == false)
	return false;
    }

  if (ref || (last_ts->type == BT_CHARACTER
	      && rvalue->expr_type == EXPR_CONSTANT))
    {
      /* An initializer has to be constant.  */
      if (lvalue->ts.u.cl->length == NULL && !(ref && ref->u.ss.length != NULL))
	return false;
      if (lvalue->ts.u.cl->length
	  && lvalue->ts.u.cl->length->expr_type != EXPR_CONSTANT)
	return false;
      expr = create_character_initializer (init, last_ts, ref, rvalue);
      if (!expr)
	return false;
    }
  else
    {
      if (lvalue->ts.type == BT_DERIVED
	  && gfc_has_default_initializer (lvalue->ts.u.derived))
	{
	  gfc_error ("Nonpointer object %qs with default initialization "
		     "shall not appear in a DATA statement at %L",
		     symbol->name, &lvalue->where);
	  return false;
	}

      expr = gfc_copy_expr (rvalue);
      if (!gfc_compare_types (&lvalue->ts, &expr->ts))
	gfc_convert_type (expr, &lvalue->ts, 0);
    }

  if (IS_POINTER (symbol)
      && !gfc_check_pointer_assign (lvalue, rvalue, false, true))
    return false;

  if (last_con == NULL)
    symbol->value = expr;
  else
    last_con->expr = expr;

  return true;

abort:
  if (!init)
    gfc_free_expr (expr);
  mpz_clear (offset);
  return false;
}


/* Modify the index of array section and re-calculate the array offset.  */

void
gfc_advance_section (mpz_t *section_index, gfc_array_ref *ar,
		     mpz_t *offset_ret, int *vector_offset)
{
  int i;
  mpz_t delta;
  mpz_t tmp;
  bool forwards;
  int cmp;
  gfc_expr *start, *end, *stride, *elem;
  gfc_constructor_base base;

  for (i = 0; i < ar->dimen; i++)
    {
      bool advance = false;

      switch (ar->dimen_type[i])
	{
	case DIMEN_ELEMENT:
	  /* Loop to advance the next index.  */
	  advance = true;
	  break;

	case DIMEN_RANGE:
	  if (ar->stride[i])
	    {
	      stride = gfc_copy_expr(ar->stride[i]);
	      if(!gfc_simplify_expr(stride, 1))
		gfc_internal_error("Simplification error");
	      mpz_add (section_index[i], section_index[i],
		       stride->value.integer);
	      if (mpz_cmp_si (stride->value.integer, 0) >= 0)
		forwards = true;
	      else
		forwards = false;
	      gfc_free_expr(stride);
	    }
	  else
	    {
	      mpz_add_ui (section_index[i], section_index[i], 1);
	      forwards = true;
	    }

	  if (ar->end[i])
	    {
	      end = gfc_copy_expr(ar->end[i]);
	      if(!gfc_simplify_expr(end, 1))
		gfc_internal_error("Simplification error");
	      cmp = mpz_cmp (section_index[i], end->value.integer);
	      gfc_free_expr(end);
	    }
	  else
	    cmp = mpz_cmp (section_index[i], ar->as->upper[i]->value.integer);

	  if ((cmp > 0 && forwards) || (cmp < 0 && !forwards))
	    {
	      /* Reset index to start, then loop to advance the next index.  */
	      if (ar->start[i])
		{
		  start = gfc_copy_expr(ar->start[i]);
		  if(!gfc_simplify_expr(start, 1))
		    gfc_internal_error("Simplification error");
		  mpz_set (section_index[i], start->value.integer);
		  gfc_free_expr(start);
		}
	      else
		mpz_set (section_index[i], ar->as->lower[i]->value.integer);
	      advance = true;
	    }
	  break;

	case DIMEN_VECTOR:
	  vector_offset[i]++;
	  base = ar->start[i]->value.constructor;
	  elem = gfc_constructor_lookup_expr (base, vector_offset[i]);

	  if (elem == NULL)
	    {
	      /* Reset to first vector element and advance the next index.  */
	      vector_offset[i] = 0;
	      elem = gfc_constructor_lookup_expr (base, 0);
	      advance = true;
	    }
	  if (elem)
	    {
	      start = gfc_copy_expr (elem);
	      if (!gfc_simplify_expr (start, 1))
		gfc_internal_error ("Simplification error");
	      mpz_set (section_index[i], start->value.integer);
	      gfc_free_expr (start);
	    }
	  break;

	default:
	  gcc_unreachable ();
	}

      if (!advance)
	break;
    }

  mpz_set_si (*offset_ret, 0);
  mpz_init_set_si (delta, 1);
  mpz_init (tmp);
  for (i = 0; i < ar->dimen; i++)
    {
      mpz_sub (tmp, section_index[i], ar->as->lower[i]->value.integer);
      mpz_mul (tmp, tmp, delta);
      mpz_add (*offset_ret, tmp, *offset_ret);

      mpz_sub (tmp, ar->as->upper[i]->value.integer,
	       ar->as->lower[i]->value.integer);
      mpz_add_ui (tmp, tmp, 1);
      mpz_mul (delta, tmp, delta);
    }
  mpz_clear (tmp);
  mpz_clear (delta);
}


/* Rearrange a structure constructor so the elements are in the specified
   order.  Also insert NULL entries if necessary.  */

static void
formalize_structure_cons (gfc_expr *expr)
{
  gfc_constructor_base base = NULL;
  gfc_constructor *cur;
  gfc_component *order;

  /* Constructor is already formalized.  */
  cur = gfc_constructor_first (expr->value.constructor);
  if (!cur || cur->n.component == NULL)
    return;

  for (order = expr->ts.u.derived->components; order; order = order->next)
    {
      cur = find_con_by_component (order, expr->value.constructor);
      if (cur)
	gfc_constructor_append_expr (&base, cur->expr, &cur->expr->where);
      else
	gfc_constructor_append_expr (&base, NULL, NULL);
    }

  /* For all what it's worth, one would expect
       gfc_constructor_free (expr->value.constructor);
     here. However, if the constructor is actually free'd,
     hell breaks loose in the testsuite?!  */

  expr->value.constructor = base;
}


/* Make sure an initialization expression is in normalized form, i.e., all
   elements of the constructors are in the correct order.  */

static void
formalize_init_expr (gfc_expr *expr)
{
  expr_t type;
  gfc_constructor *c;

  if (expr == NULL)
    return;

  type = expr->expr_type;
  switch (type)
    {
    case EXPR_ARRAY:
      for (c = gfc_constructor_first (expr->value.constructor);
	   c; c = gfc_constructor_next (c))
	formalize_init_expr (c->expr);

    break;

    case EXPR_STRUCTURE:
      formalize_structure_cons (expr);
      break;

    default:
      break;
    }
}


/* Resolve symbol's initial value after all data statement.  */

void
gfc_formalize_init_value (gfc_symbol *sym)
{
  formalize_init_expr (sym->value);
}


/* Get the integer value into RET_AS and SECTION from AS and AR, and return
   offset.  */

void
gfc_get_section_index (gfc_array_ref *ar, mpz_t *section_index, mpz_t *offset,
		       int *vector_offset)
{
  int i;
  mpz_t delta;
  mpz_t tmp;
  gfc_expr *start, *elem;
  gfc_constructor_base base;

  mpz_set_si (*offset, 0);
  mpz_init (tmp);
  mpz_init_set_si (delta, 1);
  for (i = 0; i < ar->dimen; i++)
    {
      mpz_init (section_index[i]);
      switch (ar->dimen_type[i])
	{
	case DIMEN_ELEMENT:
	case DIMEN_RANGE:
	  elem = ar->start[i];
	  break;

	case DIMEN_VECTOR:
	  vector_offset[i] = 0;
	  base = ar->start[i]->value.constructor;
	  elem = gfc_constructor_lookup_expr (base, vector_offset[i]);
	  break;

	default:
	  gcc_unreachable ();
	}

      if (elem)
	{
	  start = gfc_copy_expr (elem);
	  if (!gfc_simplify_expr (start, 1))
	    gfc_internal_error ("Simplification error");
	  mpz_sub (tmp, start->value.integer,
		   ar->as->lower[i]->value.integer);
	  mpz_mul (tmp, tmp, delta);
	  mpz_add (*offset, tmp, *offset);
	  mpz_set (section_index[i], start->value.integer);
	  gfc_free_expr (start);
	}
      else
	/* Fallback for empty section or constructor.  */
	mpz_set (section_index[i], ar->as->lower[i]->value.integer);

      mpz_sub (tmp, ar->as->upper[i]->value.integer,
	       ar->as->lower[i]->value.integer);
      mpz_add_ui (tmp, tmp, 1);
      mpz_mul (delta, tmp, delta);
    }

  mpz_clear (tmp);
  mpz_clear (delta);
}

