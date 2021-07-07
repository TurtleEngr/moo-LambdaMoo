#!/usr/local/bin/perl
# $Header: /repo/public.cvs/app/lambda/src/script/statmoo.cgi,v 1.2 2001/06/24 02:14:15 bruce Exp $
#
# statmoo - A PERL script to report the status of a MOO server, related
# processes, and general state of the machine.
#
# Note: Though this script tries its best to include all MOO-related processes
# in its output listings, due to the way the MOO forks its name-lookup
# processes, the MOO must be started from a shell which properly supports
# process groups (i.e. csh or some derivative) for statmoo to be able to find
# and report info on the name-lookup processes.
#
###############################################################################
#                           CONFIGURATION SECTION                             #
###############################################################################

# This is just for convenience in defining the following config variables.
# It's not used by the rest of the script, so you can ignore it if you want..

#$MOOHOME = "/usr/moo";
$MOOHOME = "/usr/du/world/moo";

# The following is the file that contains the PID number for the running MOO
# process to be monitored:

$moo_pid_file = "$MOOHOME/moo.pid";

# And the file that contains the PID of a MOO-related backup/copy/FTP/whatever
# process that might run regularly.  If you don't have one, just leave it
# blank:

#$backup_pid_file = "";
$backup_pid_file = "$MOOHOME/du_backup.pid";

# This is the disk ID (as reported by vmstat) for the disk on which the MOO
# resides (writes checkpoints, stores FUP files, etc):

#$moodisk = "s1";
$moodisk = "s3";

# This is a list of other disks (separated by spaces) you are interested in
# monitoring.  Note that only 4 total (including the MOO disk) will be used:

#$otherdisks = "";
$otherdisks = "s1";

# Threshold values for disk IO (KB/sec) for what should be considered low,
# moderate, or high.  You may need to tweak these depending on what you see
# on your own system:

$disk_lo_threshold = 20;
$disk_hi_threshold = 45;

# Directory (or regexp pattern) to remove from all output (ps) listings,
# either for security (in the case that this output will be made available to
# the general public and you don't want them having access to too much specific
# info about your system), or just for general readability:

#$filter_pattern = "$MOOHOME/";
$filter_pattern = "/usr/du/world/";

###############################################################################
#                         END OF CONFIGURATION SECTION                        #
###############################################################################

# (needed for running via CGI under some setups)
$> = $<;

# I'll make it do fancy HTML some day, but for now this suffices for CGI output
print "Content-Type: text/plain\n\n" if $ENV{REQUEST_METHOD};

open(PID, $moo_pid_file);
chop($moopid = <PID>);
if ($backup_pid_file) {
  open(PID, $backup_pid_file);
  chop($backpid = <PID>);
} else {
  $backpid = "xxx";
}

$pspat = "^ *[^ ]+ +[^ ]+ +[0-9]+ +([0-9]+) +";
$psmoopat = "^ *[^ ]+ +[^ ]+ +[0-9]+ +$moopid +([0-9]+ +){6}[0-9a-f]{8} *([0-9]+) ";
$moopidlist = $moopid;
$cppid = "xxx";
$backpidlist = $backpid;
open(PSLIST, "/bin/ps -elj |");
while (<PSLIST>) {
  if    (m/$psmoopat/o)                { $moopagesz   = $2; }
  elsif (m/${pspat}$moopid /o)         { $cppid       = $1; }
  elsif (m/${pspat}[0-9]+ +$moopid /o) { $moopidlist  = "$moopidlist|$1"; }
  elsif (m/${pspat}($backpid|[0-9]+ +$backpid) /o)
                                       { $backpidlist = "$backpidlist|$1"; }
}

$pspid = open(PSLIST, "/usr/ucb/ps avgxww |");
while (<PSLIST>) {
  # filter out anything that shouldn't be shown...
  s/$filter_pattern//go;

  if    (m/^ *PID/o)             { $psheader   = $_; }
  elsif (m/^ *$moopid /o)        { $mooinfo    = $_; }
  elsif (m/^ *$cppid /o)         { $cpinfo     = $_; }
  elsif (m/^ *($moopidlist) /o)  { @otherlines = (@otherlines, $_); }
  elsif (m/^ *($backpidlist) /o) { @backlines  = (@backlines, $_); }
  elsif (m/^ *([0-9]*) [^.]* ([0-9]+\.[0-9]) +([0-9]+\.[0-9])/
        && ($2 >= 5 || $3 >= 5) && $1 != $pspid && $1 != $$)
                                 { @bigprocs   = (@bigprocs, $_); }
}

if ($mooinfo) {
  print "The MOO server is running.\n";
  print "MOO-Server processes:\n";
  print $psheader; $psheader = "";
  print $mooinfo, $cpinfo, @otherlines;
} else {
  print "The MOO server is not running.\n";
}
if (@backlines) {
  print "MOO Backup System processes:\n";
  print $psheader; $psheader = "";
  print @backlines;
}
if (@bigprocs) {
  print "Other High-CPU processes:\n";
  print $psheader; $psheader = "";
  print @bigprocs;
}

open (VMSTAT, "vmstat $moodisk $otherdisks 5 2 |");
print "\nVMstat info:\n";
print scalar(<VMSTAT>);
print $vmheader = <VMSTAT>;
print $vmavg = <VMSTAT>;
print $vmcur = <VMSTAT>;

if ($mooinfo) {
  $moorunning = "Yes";
  $mooinfo =~ m/:[0-9][0-9] *([0-9]+ *[0-9]+) +([0-9]*\.[0-9]) +([0-9]*.[0-9])/;
  $moomemmess = $1;
  $moocpup = "$2%";
  $moomemp = "$3%";
  if ($moomemmess =~ m/^([0-9]*) +([0-9]*)$/) {
    $moosz  = $1;
    $moorss = $2;
  } else {
    # (this is the tricky bit.. ps has run the SZ and RSS together so
    #  they're practically unintelligible.  We'll have to use $moopagesz
    #  gleaned from the first PS listing, above, and multiply it by
    #  `pagesize` to figure out how many digits should be allocated to each
    #  part)
    $szlen  = length(($moopagesz * `pagesize`) / 1024);
    $moosz  = substr($moomemmess,0,$szlen);
    $moorss = substr($moomemmess,$szlen);
  }
  if ($cpinfo) {
    $cprunning = "Yes";
    $cpinfo =~ m/:[0-9][0-9]([0-9]+ *[0-9]+) +([0-9]*\.[0-9]) +([0-9]*.[0-9])/;
    $cpcpup = "$2%";
    $cpmemp = "$3%";
    # (Here things are somewhat easier than figring out the $moomemmess above,
    #  since the Checkpointer's SZ should be exactly the same as the MOO
    #  process', so we already know how many digits it will be)
    $szlen = length($moosz);
    $cpsz  = substr($1,0,$szlen);
    $cprss = substr($1,$szlen);
    $cprss =~ s/ //g;
  } else {
    $cprunning = "No";
    $cpcpup = "N/A";
    $cpmemp = "N/A";
    $cpsz   = "N/A";
    $cprss  = "N/A";
  }
} else {
  $moorunning = "No";
  $moocpup = "N/A";
  $moomemp = "N/A";
  $moosz   = "N/A";
  $moorss  = "N/A";
  $cprunning = "No";
  $cpcpup = "N/A";
  $cpmemp = "N/A";
  $cpsz   = "N/A";
  $cprss  = "N/A";
}
if (@backlines) {
  $backrunning = "Yes";
  for (@backlines) {
    # (we're assuming here that none of the backup system's processes will
    #  ever get so big that it mushes the SZ and RSS together..  It's a
    #  pretty safe bet that if this ever happens people have bigger problems
    #  than a broken stats reporting program :)
    m/:[0-9][0-9] *([0-9]+) *([0-9]+) +([0-9]*\.[0-9]) +([0-9]*.[0-9])/;
    $backsz   += $1;
    $backrss  += $2;
    $backcpup += $3;
    $backmemp += $4;
  }
  $backcpup =~ s/([0-9]*\.[0-9]).*/$1%/;
  $backmemp =~ s/([0-9]*\.[0-9]).*/$1%/;
} else {
  $backrunning = "No";
  $backcpup = "N/A";
  $backmemp = "N/A";
  $backsz   = "N/A";
  $backrss  = "N/A";
}

@vmheader = split(' ',$vmheader);
@vmcur = split(' ',$vmcur);
@vmavg = split(' ',$vmavg);

$pagercur = $vmcur[7]+$vmcur[8];
$pagerstat = ($vmcur[8] > $vmavg[8]/2) ? 2
           : ($vmcur[7] > $vmavg[7])   ? 1
           :                             0;
$pagerstat += 1 if ($vmcur[4] < $vmavg[4]/2);
$pagernote = ("", "(moderate)", "(high)", "(extreme)")[$pagerstat];

foreach $i (12..15) {
  if ($vmheader[$i] eq $moodisk) {
    $moodiskio = $vmcur[$i];
  } elsif ($vmheader[$i] ne "--") {
    $otherdiskio += $vmcur[$i];
    $odiskstat = &max($odiskstat, ($vmcur[$i] < $disk_lo_threshold) ? 0
                                : ($vmcur[$i] < $disk_hi_threshold) ? 1
                                :                     2 );
  }
}
$moodisknote = ($moodiskio < $disk_lo_threshold) ? ""
             : ($moodiskio < $disk_hi_threshold) ? "(moderate)"
             : "(high)";
$otherdisknote = ("","(moderate)","(high)")[$odiskstat];

$moosz    = &commify($moosz);
$moorss   = &commify($moorss);
$cpsz     = &commify($cpsz);
$cprss    = &commify($cprss);
$backsz   = &commify($backsz);
$backrss  = &commify($backrss);

print "\nBreakdown:\n";
printf("%19s  %10s  %10s  %10s\n", ("","MOO","Checkpoint","Backup"));
printf("%19s  %10s  %10s  %10s\n", ("Running:",$moorunning,$cprunning,$backrunning));
printf("%19s  %10s  %10s  %10s\n", ("Process Size (KB):",$moosz,$cpsz,$backsz));
printf("%19s  %10s  %10s  %10s\n", ("Amount in RAM (KB):",$moorss,$cprss,$backrss));
printf("%19s  %10s  %10s  %10s\n", ("%RAM Used:",$moomemp,$cpmemp,$backmemp));
printf("%19s  %10s  %10s  %10s\n", ("%CPU Time:",$moocpup,$cpcpup,$backcpup));

print "
Idle CPU time:      $vmcur[21]%  (average $vmavg[21]%)
Total Free Memory:  ",&commify($vmcur[3]+$vmcur[4])," KB
Pager Activity:     $pagercur KB/sec $pagernote
Disk IO:
          MOO Disk: $moodiskio KB/sec $moodisknote
 Total Other Disks: $otherdiskio KB/sec $otherdisknote
";

print "\nAnalysis:\n";
$normal = 1;
if ($pagernote eq "(extreme)") {
  print "* Extreme paging activity.  VM thrashing is likely.\n";
  $normal = 0;
}
if ($moodisknote eq "(heavy)" || $otherdisknote eq "(heavy)") {
  print "- Heavy disk IO";
  if ($pagernote eq "(extreme)")
    { print " (probably due to paging activity)"; }
  elsif (@bigprocs)
    { print ", possibly due to monopolization by another process"; }
  else
    { print ".  Possible disk bottleneck"; }
  print ".\n";
  $normal = 0;
}
if ($vmcur[21] < 5) {
  print "* Low CPU idle reading";
  if (@bigprocs)
    { print ", possibly due to monopolization by another process"; }
  else
    { print ".  Possible CPU bottleneck"; }
  print ".\n";
  $normal = 0;
}
if ($normal) { print "  System functioning normally.\n"; }

exit;
#-----------------------------------------------------------------------------#

sub max {
  local($max) = pop(@_);
  foreach $i (@_) { $max = $i if ($i > $max); }
  $max;
}

sub commify {
  local($result) = @_;
  while ($result =~ s/([0-9]+)([0-9]{3}(,[0-9]{3})*)/$1,$2/) {};
  $result;
}
