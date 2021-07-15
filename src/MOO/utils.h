/******************************************************************************
  Copyright (c) 1992, 1995, 1996 Xerox Corporation.  All rights reserved.
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

#ifndef Utils_h
#define Utils_h 1

#include "my-stdio.h"

#include "config.h"
#include "execute.h"

#define Arraysize(x) (sizeof(x) / sizeof(*x))

extern int		mystrcasecmp(const char *, const char *);
extern int		mystrncasecmp(const char *, const char *, int);

extern int		verbcasecmp(const char *verb, const char *word);

extern unsigned		str_hash(const char *);

extern void		free_var(Var);
extern Var		var_dup(Var);
extern Var      	var_ref(Var);

extern int		equality(Var lhs, Var rhs, int case_matters);
extern int 		is_true(Var v);

extern char    	       *strsub(const char *, const char *, const char *, int);
extern int		strindex(const char *, const char *, int);
extern int		strrindex(const char *, const char *, int);

extern Var		get_system_property(const char *);
extern Objid		get_system_object(const char *);

extern int		value_bytes(Var);

extern const char      *raw_bytes_to_binary(const char *buffer, int buflen);
extern const char      *binary_to_raw_bytes(const char *binary, int *rawlen);

#endif

/* $Log: utils.h,v $
 * Revision 2.4  1996/02/08  06:08:07  pavel
 * Moved become_number and compare_ints to number.h.  Updated copyright notice
 * for 1996.  Release 1.8.0beta1.
 *
 * Revision 2.3  1996/01/11  07:46:18  pavel
 * Added raw_bytes_to_binary() and binary_to_raw_bytes(), in support of binary
 * I/O facilities.  Release 1.8.0alpha5.
 *
 * Revision 2.2  1995/12/28  00:39:38  pavel
 * Removed declaration for private variable `cmap'.  Release 1.8.0alpha3.
 *
 * Revision 2.1  1995/12/11  08:09:56  pavel
 * Add `value_bytes()'.  Release 1.8.0alpha2.
 *
 * Revision 2.0  1995/11/30  04:56:31  pavel
 * New baseline version, corresponding to release 1.8.0alpha1.
 *
 * Revision 1.10  1992/10/23  23:03:47  pavel
 * Added copyright notice.
 *
 * Revision 1.9  1992/10/21  03:02:35  pavel
 * Converted to use new automatic configuration system.
 *
 * Revision 1.8  1992/10/17  20:57:30  pavel
 * Global rename of strdup->str_dup, strref->str_ref, vardup->var_dup, and
 * varref->var_ref.
 * Removed useless #ifdef of STRCASE.
 *
 * Revision 1.7  1992/09/08  21:55:12  pjames
 * Added `become_number()' (from bf_num.c).
 *
 * Revision 1.6  1992/09/03  16:23:16  pjames
 * Make cmap[] visible.
 *
 * Revision 1.5  1992/08/31  22:22:05  pjames
 * Changed some `char *'s to `const char *'
 *
 * Revision 1.4  1992/08/28  16:24:15  pjames
 * Added `varref()'.  Removed `copy_pi()'.
 *
 * Revision 1.3  1992/08/10  16:38:28  pjames
 * Added is_true (from execute.h) and *_activ_as_pi routines.
 *
 * Revision 1.2  1992/07/27  18:03:48  pjames
 * Changed name of ct_env to var_names
 *
 * Revision 1.1  1992/07/20  23:23:12  pavel
 * Initial RCS-controlled version.
 */
