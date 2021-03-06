/******************************************************************************
  Copyright (c) 1996 Xerox Corporation.  All rights reserved.
  Portions of this code were written by Stephen White, aka ghond.
  Use and copying of this software and preparation of derivative works based
  upon this software are permitted.  Any distribution of this software or
  derivative works must comply with all applicable United States export
  control laws.  This software is made available AS IS, and Xerox Corporation
  makes no warranty about the software, its performance or its conformity to
  any specification.  Any person obtaining a copy of this software is requested
  to send their name and post office or electronic mail address to:
    Pavel Curtis
    Xerox PARC
    3333 Coyote Hill Rd.
    Palo Alto, CA 94304
    Pavel@Xerox.Com
 *****************************************************************************/

#include "structures.h"

extern Var		new_float(double);
extern enum error	become_integer(Var, int *, int);

extern int		do_equals(Var, Var);
extern int		compare_integers(int, int);
extern Var		compare_numbers(Var, Var);

extern Var		do_add(Var, Var);
extern Var		do_subtract(Var, Var);
extern Var		do_multiply(Var, Var);
extern Var		do_divide(Var, Var);
extern Var		do_modulus(Var, Var);
extern Var		do_power(Var, Var);

/*
 * $Log: numbers.h,v $
 * Revision 1.1  2021/07/15 19:45:10  bruce
 * Updated with 64-bit compile
 *
 * Revision 1.1  1996/02/08  07:28:25  pavel
 * Initial revision
 *
 */
