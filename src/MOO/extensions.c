/******************************************************************************
  Copyright (c) 1995, 1996 Xerox Corporation.  All rights reserved.
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

/* Extensions to the MOO server
 *
 * This module contains some examples of how to extend the MOO server using
 * some of the interfaces exported by various other modules.  The examples are
 * all commented out, since they're really not all that useful in general; they
 * were written primarily to test out the interfaces they use.
 *
 * The uncommented parts of this module provide a skeleton for any module that
 * implements new MOO built-in functions.  Feel free to replace the
 * commented-out bits with your own extensions; in future releases, you can
 * just replace the distributed version of this file (which will never contain
 * any actually useful code) with your own edited version as a simple way to
 * link in your extensions.
 */

#define EXAMPLE 0

#include "bf_register.h"
#include "functions.h"

#if EXAMPLE

#include "my-unistd.h"

#include "exceptions.h"
#include "log.h"
#include "net_multi.h"
#include "storage.h"
#include "tasks.h"

typedef struct stdin_waiter {
    struct stdin_waiter *next;
    vm			 the_vm;
} stdin_waiter;

static stdin_waiter    *waiters = 0;

static task_enum_action
stdin_enumerator(task_closure closure, void *data)
{
    stdin_waiter      **ww;

    for (ww = &waiters; *ww; ww = &((*ww)->next)) {
	stdin_waiter	       *w = *ww;
	const char	       *status = (w->the_vm->task_id & 1
					  ? "stdin-waiting"
					  : "stdin-weighting");
	task_enum_action	tea = (*closure)(w->the_vm, status, data);

	if (tea == TEA_KILL) {
	    *ww = w->next;
	    myfree(w, M_TASK);
	    if (!waiters)
		network_unregister_fd(0);
	}

	if (tea != TEA_CONTINUE)
	    return tea;
    }

    return TEA_CONTINUE;
}

static void
stdin_readable(int fd, void *data)
{
    char		buffer[1000];
    int			n;
    Var			v;
    stdin_waiter       *w;

    if (data != &waiters)
	panic("STDIN_READABLE: Bad data!");

    if (!waiters) {
	errlog("STDIN_READABLE: Nobody cares!\n");
	return;
    }

    n = read(0, buffer, sizeof(buffer));
    buffer[n] = '\0';
    while (n)
	if (buffer[--n] == '\n')
	    buffer[n] = 'X';

    if (buffer[0] == 'a') {
	v.type = TYPE_ERR;
	v.v.err = E_NACC;
    } else {
	v.type = TYPE_STR;
	v.v.str = str_dup(buffer);
    }

    resume_task(waiters->the_vm, v);
    w = waiters->next;
    myfree(waiters, M_TASK);
    waiters = w;
    if (!waiters)
	network_unregister_fd(0);
}

static enum error
stdin_suspender(vm the_vm, void *data)
{
    stdin_waiter       *w = data;

    if (!waiters)
	network_register_fd(0, stdin_readable, 0, &waiters);

    w->the_vm = the_vm;
    w->next = waiters;
    waiters = w;

    return E_NONE;
}

static package
bf_read_stdin(Var arglist, Byte next, void *vdata, Objid progr)
{
    stdin_waiter       *w = mymalloc(sizeof(stdin_waiter), M_TASK);

    return make_suspend_pack(stdin_suspender, w);
}
#endif /* EXAMPLE */

void
register_extensions()
{
#if EXAMPLE
    register_task_queue(stdin_enumerator);
    register_function("read_stdin", 0, 0, bf_read_stdin);
#endif
}

char rcsid_extensions[] = "$Id: extensions.c,v 1.1 2021/07/15 19:45:06 bruce Exp $";

/*
 * $Log: extensions.c,v $
 * Revision 1.1  2021/07/15 19:45:06  bruce
 * Updated with 64-bit compile
 *
 * Revision 2.1  1996/02/08  07:03:47  pavel
 * Renamed err/logf() to errlog/oklog().  Updated copyright notice for 1996.
 * Release 1.8.0beta1.
 *
 * Revision 2.0  1995/11/30  04:26:34  pavel
 * New baseline version, corresponding to release 1.8.0alpha1.
 *
 * Revision 1.1  1995/11/30  04:26:21  pavel
 * Initial revision
 */
