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
#include <sys/wait.h>

#if NDECL_WAITPID
#include "my-types.h"

extern pid_t	waitpid(pid_t, int *, int);
#endif

/*
 * $Log: my-wait.h,v $
 * Revision 1.1  2021/07/15 19:45:09  bruce
 * Updated with 64-bit compile
 *
 * Revision 2.1  1996/02/08  05:58:27  pavel
 * Updated copyright notice for 1996.  Release 1.8.0beta1.
 *
 * Revision 2.0  1995/11/30  05:00:04  pavel
 * New baseline version, corresponding to release 1.8.0alpha1.
 *
 * Revision 1.2  1992/10/23  23:03:47  pavel
 * Added copyright notice.
 *
 * Revision 1.1  1992/10/23  22:25:00  pavel
 * Initial RCS-controlled version.
 */
