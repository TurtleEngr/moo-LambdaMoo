/******************************************************************************
  Copyright (c) 1994, 1995, 1996 Xerox Corporation.  All rights reserved.
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

#include "options.h"

#  if MPLEX_STYLE == MP_SELECT
#    include "net_mp_selct.c"
#  endif

#  if MPLEX_STYLE == MP_POLL
#    include "net_mp_poll.c"
#  endif

#  if MPLEX_STYLE == MP_FAKE
#    include "net_mp_fake.c"
#  endif

char rcsid_net_mplex[] = "$Id: net_mplex.c,v 1.1 2021/07/15 19:45:10 bruce Exp $";

/*
 * $Log: net_mplex.c,v $
 * Revision 1.1  2021/07/15 19:45:10  bruce
 * Updated with 64-bit compile
 *
 * Revision 2.1  1996/02/08  06:58:20  pavel
 * Updated copyright notice for 1996.  Release 1.8.0beta1.
 *
 * Revision 2.0  1995/11/30  04:28:28  pavel
 * New baseline version, corresponding to release 1.8.0alpha1.
 *
 * Revision 1.1  1995/11/30  04:28:19  pavel
 * Initial revision
 */
