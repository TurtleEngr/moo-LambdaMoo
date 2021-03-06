/******************************************************************************
  The following was written by Alex Stewart and is released into the public
  domain.  Anyone may copy, modify, or completely ignore it.  The author makes
  no warranty of any kind and accepts no responsibility for this software or
  any results of its use.

  Note:  Portions of the following code were copied from the LambdaMOO server
  source, and thus other copyright requirements and conditions may apply.
 -----------------------------------------------------------------------------
  The following is part of a patch to the LambdaMOO (1.7.7 and 1.7.8 (maybe
  others, not tested)) server.  The following routines are for two additional
  builtin functions for the MOO language: ctime_tz(tzspec [,time]) and
  mktime_tz(tzspec, year, month, mday, hour, min, sec).  The former acts just
  like ctime(), but allows one to specify the unix timezone information to
  use when performing the conversion (instead of only supporting the machine's
  local timezone).  The latter is a function which will take a broken-down
  time (easily extractable from ctime output) and a time zone and provide the
  corresponding seconds-since-1970 value, or *returns* E_INVARG if the 
  specified time is out of range or doesn't exist (falls within a 
  DST-conversion hour or some such so that there wasn't actually any time 
  at which the clocks displayed the specified time).

  Both functions take _unix_ timezone specs, of the form used for the TZ 
  environment variable (see 'man tzset' for more info), which are not 
  the same as the "PST/PDT" type things that ctime reports.  This 
  makes them much more flexible, powerful, and customizable, but will 
  probably require some in-DB checking and conversion for a lot of things.
  Also note that the timezone id on the end of the ctime string isn't
  guaranteed to be exactly 3 characters long for all timezones.

  The timezone handling is done by temporarily pretending (by setting the 
  TZ variable in the program's environment area) that the machine is in 
  the given timezone instead of its actual zone, then setting things 
  back to normal afterwards.  It's a hack, but what can I say, standard unix 
  time routines suck.  (speaking of which, anyone who thinks "unix is unix"
  should try looking at the time handling functions available on various 
  systems and writing something that's supposed to work properly on them 
  all.. yikes.)
 *****************************************************************************/
/* Following is a "TZ" environment variable setting to use as the default 
 * timezone that the MOO runs under.  "TZ=localtime" should be sufficient 
 * for most situations.
 */

static char tzdefault[] = "TZ=localtime";

/*****************************************************************************/

#include "my-stdlib.h"
#include "my-string.h"
#include "my-time.h"
#include "structures.h"
#include "utils.h"
#include "storage.h"
#include "functions.h"
#include "server.h"

#define TZBUFLEN 100

/* Some systems (aka SunOS) don't seem to define these in their header files
   even though they do apparently support them.. if you get warnings, you
   can uncomment these lines */
/* int putenv(const char *string); */
/* time_t mktime(struct tm *timeptr); */

static package
bf_ctime_tz(Var arglist, Byte next, void *vdata, Objid progr)
{ /* (tzstr [,time]) */
    Var         r;
    time_t      c;
    char        buffer[50];
    char        tzbuffer[4+TZBUFLEN] = "TZ=";

    strncpy(&tzbuffer[3], arglist.v.list[1].v.str, TZBUFLEN);
    tzbuffer[3+TZBUFLEN] = 0;
    if (arglist.v.list[0].v.num == 2) {
        c = arglist.v.list[2].v.num;
    } else {
        c = time(0);
    }

    /* Pretend we're in the specified time zone */
    putenv(tzbuffer);
    tzset();

    { /* Format the time, including a timezone name */
#if HAVE_STRFTIME
        strftime(buffer, 50, "%a %b %d %H:%M:%S %Y %Z", localtime(&c));
#else
#  if HAVE_TM_ZONE
        struct tm      *t = localtime(&c);
        char           *tzname = t->tm_zone;
#  else
#    if !HAVE_TZNAME
        const char     *tzname = "XXX";
#    endif
#  endif

        strcpy(buffer, ctime(&c));
        buffer[24] = ' ';
        strncpy(buffer + 25, tzname, 3);
        buffer[28] = '\0';
#endif
    }

    if (buffer[8] == '0')
        buffer[8] = ' ';
    r.type = TYPE_STR;
    r.v.str = str_dup(buffer);

    /* Set the timezone back to normal */
    putenv(tzdefault);
    tzset();

    free_var(arglist);
    return make_var_pack(r);
}

static package
bf_mktime_tz(Var arglist, Byte next, void *vdata, Objid progr)
{ /* (tzstr, year, month, day, hour, min, sec) */
    Var         r;
    char        tzbuffer[4+TZBUFLEN] = "TZ=";
    struct tm   tmstruct;
    struct tm  *pltstruct;
    time_t      t;

    strncpy(&tzbuffer[3], arglist.v.list[1].v.str, TZBUFLEN);
    tzbuffer[3+TZBUFLEN] = 0;

    tmstruct.tm_year  = arglist.v.list[2].v.num - 1900;
    tmstruct.tm_mon   = arglist.v.list[3].v.num - 1;
    tmstruct.tm_mday  = arglist.v.list[4].v.num;
    tmstruct.tm_hour  = arglist.v.list[5].v.num;
    tmstruct.tm_min   = arglist.v.list[6].v.num;
    tmstruct.tm_sec   = arglist.v.list[7].v.num;
    tmstruct.tm_wday  = 0;
    tmstruct.tm_yday  = 0;
    tmstruct.tm_isdst = -1;

    /* Pretend we're in the specified time zone */
    putenv(tzbuffer);
    tzset();

    t = mktime(&tmstruct);

    /* The setting of tm_isdst to -1 above tells it to "autosense" the 
       appropriate value to use on its own (this is necessary for Linux 
       because otherwise it won't convert properly for DST times, but some 
       systems don't support that and will see the nonzero value as saying 
       "DST on" even if it's not a time of the year that's DST, so just in
       case we'll check to see whether it's actually a non-dst time we 
       came up with and if so, do it again with tm_isdst = 0.  
       (it sounds overly complicated but it should work for the widest 
       range of OSes, I think.  It wouldn't be nearly so complicated if 
       certain OSes were more up to date (supporting the "autosense" 
       setting) or if I wasn't trying to get around a bug in Linux (not 
       handling positive tm_isdst values properly) too.. sigh) */
    tmstruct.tm_isdst = localtime(&t)->tm_isdst;
    if (!tmstruct.tm_isdst)
        t = mktime(&tmstruct);

    r.type = TYPE_INT;
    r.v.num = t;

    /* mktime returns -1 on error, but -1 is also a valid time value, so 
       if we got -1 back, check to see whether the tm struct for a time 
       of -1 corresponds to the tm struct we gave it.  If not, return an 
       error instead. */
    if (t == -1) {
        pltstruct = localtime(&t);
        if ((tmstruct.tm_year != pltstruct->tm_year) ||
            (tmstruct.tm_mon  != pltstruct->tm_mon)  ||
            (tmstruct.tm_mday != pltstruct->tm_mday) ||
            (tmstruct.tm_hour != pltstruct->tm_hour) ||
            (tmstruct.tm_min  != pltstruct->tm_min)  ||
            (tmstruct.tm_sec  != pltstruct->tm_sec)) {
            r.type = TYPE_ERR;
            r.v.err = E_INVARG;
        }
    }

    /* Set the timezone back to normal */
    putenv(tzdefault);
    tzset();

    free_var(arglist);
    return make_var_pack(r);
}

void
register_pAS5(void)
{
    (void) register_function("ctime_tz", 1, 2, bf_ctime_tz, TYPE_STR, TYPE_INT);
    (void) register_function("mktime_tz", 7, 7, bf_mktime_tz,
        TYPE_STR, TYPE_INT, TYPE_INT, TYPE_INT, TYPE_INT, TYPE_INT, TYPE_INT);

    /* (make sure things start out the same as our routines set for the default) */
    putenv(tzdefault);
    tzset();
}

/* (end) */
