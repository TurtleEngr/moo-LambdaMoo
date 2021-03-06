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

#include "config.h"
#include "eval_env.h"
#include "storage.h"
#include "structures.h"
#include "sym_table.h"
#include "utils.h"

Var *
new_rt_env(unsigned size)
{
    Var *ret = mymalloc(size * sizeof(Var), M_RT_ENV);
    unsigned i;
    
    for (i = 0; i < size; i++)
	ret[i].type = TYPE_NONE;

    return ret;
}    

void 
free_rt_env(Var *rt_env, unsigned size)
{
    register unsigned i;

    for (i = 0; i < size; i++)
	free_var(rt_env[i]);
    myfree((void *) rt_env, M_RT_ENV);
}

Var *
copy_rt_env(Var *from, unsigned size)
{
    unsigned i;

    Var *ret = new_rt_env(size);
    for (i = 0; i < size; i++) 
	ret[i] = var_ref(from[i]);
    return ret;
}

void
fill_in_rt_consts(Var *env, DB_Version version)
{
    Var v;

    v.type = TYPE_INT;
    v.v.num = (int) TYPE_ERR;     env[SLOT_ERR] = var_ref(v);
    v.v.num = (int) TYPE_INT;     env[SLOT_NUM] = var_ref(v);
    v.v.num = (int) TYPE_STR;     env[SLOT_STR] = var_ref(v);
    v.v.num = (int) TYPE_OBJ;     env[SLOT_OBJ] = var_ref(v);
    v.v.num = (int) TYPE_LIST;    env[SLOT_LIST] = var_ref(v);

    if (version >= DBV_Float) {
	v.v.num = (int) TYPE_INT;   env[SLOT_INT] = var_ref(v);
	v.v.num = (int) TYPE_FLOAT; env[SLOT_FLOAT] = var_ref(v);
    }
}

void
set_rt_env_obj(Var *env, int slot, Objid o)
{
    Var v;
    v.type = TYPE_OBJ;
    v.v.obj = o;
    env[slot] = var_ref(v);
}

void
set_rt_env_str(Var *env, int slot, const char *s)
{
    Var v;
    v.type = TYPE_STR;
    v.v.str = s;
    env[slot] = v;
}

void
set_rt_env_var(Var *env, int slot, Var v)
{
    env[slot] = v;
}

char rcsid_rt_env[] = "$Id: eval_env.c,v 1.1 2021/07/15 19:45:06 bruce Exp $";

/*
 * $Log: eval_env.c,v $
 * Revision 1.1  2021/07/15 19:45:06  bruce
 * Updated with 64-bit compile
 *
 * Revision 2.1  1996/02/08  07:13:03  pavel
 * Renamed TYPE_NUM to TYPE_INT.  Made fill_in_rt_consts() version-dependent.
 * Updated copyright notice for 1996.  Release 1.8.0beta1.
 *
 * Revision 2.0  1995/11/30  04:21:51  pavel
 * New baseline version, corresponding to release 1.8.0alpha1.
 *
 * Revision 1.6  1992/10/23  23:03:47  pavel
 * Added copyright notice.
 *
 * Revision 1.5  1992/10/21  03:02:35  pavel
 * Converted to use new automatic configuration system.
 *
 * Revision 1.4  1992/10/17  20:24:21  pavel
 * Global rename of strdup->str_dup, strref->str_ref, vardup->var_dup, and
 * varref->var_ref.
 *
 * Revision 1.3  1992/08/31  22:23:15  pjames
 * Changed some `char *'s to `const char *'
 *
 * Revision 1.2  1992/08/28  15:59:54  pjames
 * Changed vardup to varref.
 *
 * Revision 1.1  1992/08/10  16:20:00  pjames
 * Initial RCS-controlled version
 */
