#!/usr/bin/perl
#
# For test:
# 	perf script -s fsperf.pl -i perf-block.data
# 	perf script -s fsperf.pl -i perf-sched.data
#

# For debugging
#use Carp;
#$SIG{ __DIE__ } = sub { Carp::confess( @_ ) };

BEGIN {
    use Config;

    # use bignum if IV or UV is small for this program.
    my $too_small = ($Config{'ivsize'} < 8 or $Config{'uvsize'} < 8);
    if ($too_small) {
	if (not $ENV{'PERF_EXEC_PATH'}) {
	    warn "Warning: Perl IV/UV size may be too small to run.\n";
	}
	require bignum;
	import bignum;
    }
}

if ($ENV{'PERF_EXEC_PATH'}) {
    no warnings;
    use lib "$ENV{'PERF_EXEC_PATH'}/scripts/perl/Perf-Trace-Util/lib";
    use lib "./Perf-Trace-Util/lib";

    require Perf::Trace::Core;
    import Perf::Trace::Core;
    require Perf::Trace::Context;
    import Perf::Trace::Context;
    require Perf::Trace::Util;
    import Perf::Trace::Util;
}

# For debugging
#use Carp 'verbose';
#$SIG{ __DIE__ } = sub { Carp::confess( @_ ) };

#sub dump_stack
#{
#    my $i = 1;
#    print STDERR "Stack Trace:\n";
#    while ((my @t = (caller($i++)))) {
#	print STDERR "  $t[1]: $t[3] at $t[2]\n";
#    }
#}

use strict;
use warnings;
use Math::BigInt try => 'GMP';
use Getopt::Long;
use Cwd;
use File::Copy;
use POSIX ();

# If there are many processes, we open sched_*.dat for each process.
# So, this can be the cause of EMFILE.
#
# To fix it, use "FileCache" module. It makes LRU cache of open FHs,
# and when re-open expired cache, it opens by append mode.
no strict "refs";
use FileCache;

my (%block_s, %sched_s, %switch_state, %cpu_state, %wq_state, %irq_s,
    %syscall_s);

my $perf_sched_data = "perf-sched.data";
my $perf_sched_kallsyms = "perf-sched.kallsyms";
my $perf_block_data = "perf-block.data";
my $perf_block_map = "perf-block.map";
my $output_dir = "fsperf-output";
my $debug_event_fname = "fsperf-debug.pl";

#
# Options inherit to child process
#
# Don't exit even if sanity check found error
my $opt_no_error = 0;
# less than this seek distance will be ignore
my $opt_seek_threshold = 0;
# 0: absolute distance, 1: relative distance
my $opt_seek_relative = 0;
# Read kallsyms from specified file
my $opt_kallsyms = $perf_sched_kallsyms;
# Use sched_switch instead of sched_stat_*
my $opt_use_sched_switch = 0;
# Debugging for perf event
my $opt_debug_event = 0;

#
# Options not inherit to child process
#
# --call-graph option for sched events
my $opt_call_graph = undef;
# Disable to collect irq events
my $opt_no_irq = 0;
# Disable to collect syscall events
my $opt_no_syscall = 0;
# Don't run block events
my $opt_no_block = 0;
# Don't run sched events
my $opt_no_sched = 0;
# Only re-plot graph
my $opt_graph_only = 0;
# Print kallsyms
my $opt_print_kallsyms = 0;

my @opt_target_pid = ();
my @opt_target_wid = ();
my ($cur_time, $perf_start, $perf_end, $perf_xstart, $perf_xend);
my %devmap;

##################################
#
# Utility functions
#

use constant MINORBITS => 20;
use constant MINORMASK => ((1 << MINORBITS) - 1);

sub kmajor($)
{
    my $dev = shift;
    return $dev >> MINORBITS;
}

sub kminor($)
{
    my $dev = shift;
    return $dev & MINORMASK;
}

sub kmakedev($$)
{
    my $major = shift;
    my $minor = shift;
    return $major << MINORBITS | $minor;
}

sub kdevname($)
{
    my $dev = shift;
    return sprintf("%u,%u", kmajor($dev), kminor($dev))
}

sub min($$)
{
    my $a = shift;
    my $b = shift;
    return $a if (!defined($b));
    return $b if (!defined($a));
    return $b if ($a > $b);
    return $a;
}

sub max($$)
{
    my $a = shift;
    my $b = shift;
    return $a if (!defined($b));
    return $b if (!defined($a));
    return $b if ($a < $b);
    return $a;
}

# bignum in some version can't handle undef as 0. So, this checks
# undef, then initialize or add.
sub num_add($$)
{
    if (defined($_[0])) {
	$_[0] += $_[1];
    } else {
	$_[0] = $_[1];
    }
}

sub num_sub($$)
{
    if (defined($_[0])) {
	$_[0] -= $_[1];
    } else {
	$_[0] = -$_[1];
    }
}

# helper for max/min
sub num_max($$)
{
    $_[0] = max($_[0], $_[1]);
}

sub num_min($$)
{
    $_[0] = min($_[0], $_[1]);
}

use constant NSEC_PER_SEC => 1000000000;

sub to_tv64($$)
{
    my ($secs, $nsecs) = @_;
    return $secs * NSEC_PER_SEC + $nsecs;
}

sub to_sec($)
{
    my $tv64 = shift;
    return int($tv64 / NSEC_PER_SEC);
}

sub to_nsec($)
{
    my $tv64 = shift;
    return int($tv64 % NSEC_PER_SEC);
}

sub to_float_tv64($)
{
    my $tv64 = shift;
    return to_sec($tv64) + to_nsec($tv64) / NSEC_PER_SEC;
}

sub time_str($$)
{
    my ($secs, $nsecs) = @_;
    return sprintf("%u.%09u", $secs, $nsecs);
}

sub tv64_str($)
{
    my $tv64 = shift;
    return time_str(to_sec($tv64), to_nsec($tv64));
}

sub pr_warn(@)
{
    warn "Warning: @_\n";
}

sub safe_system_with_output
{
    my $output = shift;
    my @cmd = @_;

    if ($output) {
	# Redirect output to $output
	@cmd = join(" ", @cmd, "> $output 2>&1");
    }

    print STDERR "Run command: @cmd\n";
    if (system(@cmd) != 0) {
	if ($output) {
	    # Print error output
	    if (open(my $out, "<", "$output")) {
		while (<$out>) {
		    print STDERR;
		}
		close($out);
	    }
	}

	die "Exited abnormally with code $?: `@cmd'";
    }
}

sub safe_system
{
    safe_system_with_output(undef, @_);
}

# Older perf's perl didn't support callchain, and newer one
# supports. But it was introduced with incompatible way.
#
# So this function make args normalize to newer format.
sub perf_args_normalize
{
    my $args = shift;

    # If $common_callchain is not array ref, assumes as older perf
    # without callchain. So adds dummy array ref as callchain.
    if (ref($args->[7]) ne "ARRAY") {
	my @dummy_callchain = ();
	splice(@{$args}, 7, 0, \@dummy_callchain);
    }

    # For debugging
    debug_event(@{$args}) if ($opt_debug_event);
}

# perf script event handlers, generated by perf script -g perl
# Licensed under the terms of the GNU GPL License version 2

# The common_* event handler fields are the most useful fields common to
# all events.  They don't necessarily correspond to the 'common_*' fields
# in the format files.  Those fields not available as handler params can
# be retrieved using Perl functions of the form common_*($context).
# See Context.pm for the list of available functions.

sub update_cur_time($$)
{
    my ($secs, $nsecs) = @_;
    my $time = to_tv64($secs, $nsecs);

    $cur_time = $secs;
    num_min($perf_start, $time);
    num_max($perf_end, $time);
}

sub calc_start_end_time
{
    # Adjust range of start and end
    $perf_xstart = to_sec($perf_start) - 1;
    $perf_xend = to_sec($perf_end) + 2;
}

# Remember cache FH to close before fork()
my %cache_fh;

sub open_file($;$$)
{
    my $name = shift;
    my $mode = shift;
    my $curdir = shift;
    my $path;

    if ($curdir) {
	$path = $name;
    } else {
	if (! -d $output_dir) {
	    mkdir($output_dir);
	}
	$path = "$output_dir/$name";
    }

    my $fh = cacheout($path) or die "Couldn't create file: $path: $!";
    if ($mode) {
	chmod($mode, $fh);
    }
    $cache_fh{$fh} = 1;

    return $fh;
}

sub close_file($)
{
    my $fh = shift;
    delete($cache_fh{$fh});
    cacheout_close($fh);
}

sub close_file_all()
{
    foreach my $fh (keys(%cache_fh)) {
	close_file($fh);
    }
}

sub open_datfile($)
{
    my $base = shift;
    my $name = sprintf("%s.dat", $base);
    return open_file($name);
}

##################################
#
# Hack to resolve symbol
#
# FIXME: maybe, we should add new functionality to perf to resolve
# symbol from script
#

my @kallsyms;
my $kallsyms_loaded = 0;

# Load kallsyms in perf.data
sub kallsyms_load($)
{
    my $data = shift;

    my $re_addr = "[0-9a-fA-F]";
    my $re_addr8 = $re_addr . "{8}";
    my $re_addr16 = $re_addr . "{16}";

    # No way to dump kallsyms by perf, so use "strings"
    open(my $fh, "-|", "strings $data") or die "Couldn't run `strings': $!";

    my $last = -1;
    my $need_sort = 0;
    while (<$fh>) {
	if (m!^($re_addr8|$re_addr16) (\S) (\S+)(\s+\[(.*)\])?$!) {
	    my $addr = Math::BigInt->from_hex($1);
	    my $sym_type = $2;
	    my $sym = $3;
	    my $mod = $5 || "";

	    print "[$data]" . $_ if ($opt_print_kallsyms);

	    my %m = (
		     addr => $addr,
		     type => $sym_type,
		     sym => $sym,
		     mod => $mod,
		    );

	    push(@kallsyms, \%m);

	    if ($last > $addr) {
		$need_sort = 1;
	    }
	    $last = $addr;
	}
    }

    close($fh);

    $need_sort = 1;
    if ($need_sort) {
	@kallsyms = sort { $a->{addr} <=> $b->{addr} } @kallsyms;
    }
#    foreach my $m (@kallsyms) {
#	printf "%#x, %s, %s, %s\n",
#	    $m->{addr}, $m->{type}, $m->{sym}, $m->{mod};
#    }
}

sub kallsyms_load_all()
{
    if ($kallsyms_loaded == 0) {
	# Read from kallsyms embedded in perf.data (that older perf had)
	kallsyms_load($perf_sched_data);

	# If no embedded kallsyms, try to read from $opt_kallsyms
	if (!scalar(@kallsyms)) {
	    print "Trying read symbols from \"$opt_kallsyms\"\n";
	    kallsyms_load($opt_kallsyms);
	}
    }

    $kallsyms_loaded = 1;
}

# Find symbol by addr
sub kallsyms_find_by_addr($)
{
    my $addr = shift;

    kallsyms_load_all();

    if (!scalar(@kallsyms)) {
	# If not found, return address
	return sprintf("%#x", $addr);
    }

    # bsearch symbol address
    my $min_idx = 0;
    my $max_idx = $#kallsyms;

    my ($idx, $sym, $diff);
    while ($min_idx <= $max_idx) {
	$idx = int(($min_idx + $max_idx) / 2);

	if ($kallsyms[$idx]->{addr} < $addr) {
	    $min_idx = $idx;
	} elsif ($kallsyms[$idx]->{addr} > $addr) {
	    $max_idx = $idx - 1;
	} else {
	    last;
	}

	# linear search nearby symbol less than $addr
	if ($max_idx - $min_idx <= 5) {
	    $idx = $max_idx;
	    while ($kallsyms[$idx]->{addr} > $addr) {
		$idx--;
	    }
	    $diff = $addr - $kallsyms[$idx]->{addr};
	    last;
	}
    }

    if ($diff) {
	return sprintf("%s+%#x", $kallsyms[$idx]->{sym}, $diff);
    }
    return $kallsyms[$idx]->{sym};
}

##################################
#
# plot
#

# Using linetype color for reqsz
my $R_Q_COLOR	= 1;	# read queue
my $W_Q_COLOR	= 2;	# write queue
my $R_C_COLOR	= 10;	# read complete
my $W_C_COLOR	= 11;	# write complete

# Using linetype color for state
my $R_COLOR	= 30;	# TASK_RUNNING color
my $W_COLOR	= 31;	# cpu wait color
my $S_COLOR	= 32;	# TASK_INTERRUPTIBLE color
my $D_COLOR	= 33;	# TASK_UNINTERRUPTIBLE color
my $IRQ_COLOR	= 34;	# IRQ color
my $SIRQ_COLOR	= 35;	# Softirq color
my $SYS_COLOR	= 36;	# syscall color

my $plot_missing_char = "-";

my @sched_types = ("R", "W", "S", "D", "irq", "sirq", "sys");
my %sched_graph_info =
    (
     "R"    => {col => 2,height => 1,  name => "Running", color => $R_COLOR},
     "W"    => {col => 3,height => 1,  name => "CPU wait",color => $W_COLOR},
     "S"    => {col => 4,height => 1,  name => "Sleep",   color => $S_COLOR},
     "D"    => {col => 5,height => 1,  name => "Block",   color => $D_COLOR},
     "irq"  => {col => 6,height => 0.5,name => "IRQ",     color => $IRQ_COLOR},
     "sirq" => {col => 7,height => 0.5,name => "Softirq", color => $SIRQ_COLOR},
     "sys"  => {col => 8,height => 0.5,name => "Syscall", color => $SYS_COLOR},
    );
my $sched_total_height = 0;
foreach (@sched_types) { $sched_total_height += $sched_graph_info{$_}{height}; }

sub fname_plot_color()
{
    return ".color.gp";
}

sub output_plot_color($)
{
    my $fname = shift;

    # If not exists, write color script
    if (-f "$output_dir/$fname") {
	return;
    }

    my $fh = open_file($fname);

    print $fh <<"EOF";
if ((GPVAL_VERSION < 4.5) || \\
   (!strstrt(GPVAL_COMPILE_OPTIONS,"+USER_LINETYPES"))) \\
   exit

# For block and reqsz
set linetype  1 linecolor rgb "royalblue" pointtype 1
set linetype  2 linecolor rgb "dark-cyan" pointtype 1
set linetype  3 linecolor rgb "dark-turquoise" pointtype 1
set linetype  4 linecolor rgb "dark-pink" pointtype 1
set linetype  5 linecolor rgb "dark-salmon" pointtype 1
set linetype  6 linecolor rgb "dark-violet" pointtype 1

set linetype  7 linecolor rgb "web-blue" pointtype 2
set linetype  8 linecolor rgb "cyan" pointtype 2
set linetype  9 linecolor rgb "turquoise" pointtype 2
set linetype 10 linecolor rgb "salmon" pointtype 2
set linetype 11 linecolor rgb "pink" pointtype 2
set linetype 12 linecolor rgb "violet" pointtype 2

set linetype 13 linecolor rgb "forest-green" pointtype 3
set linetype 14 linecolor rgb "web-green" pointtype 3
set linetype 15 linecolor rgb "dark-green" pointtype 3
set linetype 16 linecolor rgb "dark-orange" pointtype 3
set linetype 17 linecolor rgb "dark-khaki" pointtype 3
set linetype 18 linecolor rgb "dark-goldenrod" pointtype 3

set linetype 19 linecolor rgb "spring-green" pointtype 4
set linetype 20 linecolor rgb "chartreuse" pointtype 4
set linetype 21 linecolor rgb "green" pointtype 4
set linetype 22 linecolor rgb "orange" pointtype 4
set linetype 23 linecolor rgb "khaki" pointtype 4
set linetype 24 linecolor rgb "goldenrod" pointtype 4

# For schedule
set linetype $R_COLOR linecolor rgb "forest-green"
set linetype $W_COLOR linecolor rgb "red"
set linetype $S_COLOR linecolor rgb "dark-gray"
set linetype $D_COLOR linecolor rgb "skyblue"
set linetype $IRQ_COLOR linecolor rgb "web-blue"
set linetype $SIRQ_COLOR linecolor rgb "skyblue"
set linetype $SYS_COLOR linecolor rgb "light-green"
EOF

    close_file($fh);
}

sub output_plot_pre($$$$@)
{
    my $fh = shift;
    my $title = shift;
    my $xlabel = shift;
    my $ylabel = shift;
    my @configs = @_;

    my $fname_color = fname_plot_color();
    output_plot_color($fname_color);

    print $fh <<"EOF";
#!/usr/bin/gnuplot

#set term dumb
#set term svg
#set term png truecolor
#set output 'example.svg'

unset format
load "${fname_color}"

EOF
    print $fh "set title '${title}'\n" if ($title);
    print $fh "set xrange [${perf_xstart}:${perf_xend}]\n";
    print $fh "set xlabel '${xlabel}'\n" if ($xlabel);
    print $fh "set ylabel '${ylabel}'\n" if ($ylabel);
    print $fh "set grid\n";
    print $fh "set termoption noenhanced\n";

    foreach my $conf (@configs) {
	if (defined($conf)) {
	    print $fh "$conf\n";
	}
    }

    print $fh <<"EOF";

plot \\
EOF
}

sub output_plot_post($)
{
    my $fh = shift;

    print $fh <<"EOF";

pause -1 "Hit return to continue"
EOF
}

sub output_plot_summary($$)
{
    my $dev = shift;
    my $need_sched = shift;

    my $devname = kdevname($dev);
    my $fname = sprintf("%s_summary.gp", $devname);

    my $fh = open_file($fname, 0755);

    my @plot_gp = (
		   fname_plot_bno($dev),
		   fname_plot_mbps($dev),
		   fname_plot_iops($dev),
		   fname_plot_reqsz($dev),
		   fname_plot_qdepth($dev),
		   fname_plot_lat_q2c($dev, "c"),
		   fname_plot_lat_d2c($dev, "c"),
		   fname_plot_seek_nr($dev, "c"),
		   fname_plot_seek_step($dev, "c")
		  );
    # Add schedule plot if need
    if ($need_sched and (scalar(@opt_target_pid) || scalar(@opt_target_wid))) {
	my @sched_gp = map {
	    fname_plot_sched($_);
	} (@opt_target_pid, @opt_target_wid);

	push(@plot_gp, @sched_gp);
    }

    my $nr_plots = scalar(@plot_gp);
    my $height = 400 * $nr_plots;
    my $width = 800;
    my $fontscale = 0.8;

    print $fh <<"EOF";
#!/usr/bin/gnuplot

set term png truecolor size ${width},${height} fontscale ${fontscale}
set output '${devname}_summary.png'
set lmargin 15
set multiplot layout ${nr_plots},1 columnsfirst scale 1.0,0.9 offset 0.0,0.0

EOF
    close_file($fh);

    # Close all cached FH
    close_file_all();

    my $oldpwd = getcwd();
    chdir($output_dir);

    my $cmd = "cat " . join(" ", @plot_gp) . " | grep -v ^pause >> $fname";
    safe_system($cmd);

    # Run summary.gp
    $cmd = "./$fname";
    safe_system_with_output("$fname.output", $cmd);

    chdir($oldpwd);

    # Move .png to current dir
    $cmd = "mv $output_dir/*.png .";
    safe_system($cmd);
}

sub graph_main
{
    # Create summary plot
    foreach my $dev (split(/,/, $ENV{FSPERF_DEV})) {
	output_plot_summary($dev, 1);
    }
}

##################################
#
# block events
#

my %EVENT_LONGNAME = (
		      "block::block_bio_queue"		=> "Queue",
		      "block::block_bio_frontmerge"	=> "FrontMerge",
		      "block::block_bio_backmerge"	=> "BackMerge",
		      "block::block_rq_issue"		=> "Issue",
		      "block::block_rq_complete"	=> "Complete"
		     );
my %EVENT_SHORTNAME = (
		       "block::block_bio_queue"		=> "Q",
		       "block::block_bio_frontmerge"	=> "F",
		       "block::block_bio_backmerge"	=> "M",
		       "block::block_rq_issue"		=> "D",
		       "block::block_rq_complete"	=> "C"
		      );
my %DIR_LONGNAME = (
		    "r" => "Read",
		    "rs" => "Read Sync",
		    "rm" => "Read META",
		    "rsm" => "Read Sync META",
		    "w" => "Write",
		    "ws" => "Write Sync",
		    "wm" => "Write META",
		    "wsm" => "Write Sync META",
		    "c" => "Combined"
		   );

use constant RWBS_FLUSH		=> (1 << 1);
use constant RWBS_WRITE		=> (1 << 2);
use constant RWBS_DISCARD	=> (1 << 3);
use constant RWBS_READ		=> (1 << 4);
use constant RWBS_NODIR		=> (1 << 5);
use constant RWBS_FUA		=> (1 << 6);
use constant RWBS_RAHEAD	=> (1 << 7);
use constant RWBS_SYNC		=> (1 << 8);
use constant RWBS_META		=> (1 << 9);
use constant RWBS_SECURE	=> (1 << 10);

# Exclude FLUSH (FLUSH is conflicting with FUA)
my %RWBS_FLAG = ("W" => RWBS_WRITE, "D" => RWBS_DISCARD, "R" => RWBS_READ,
		 "N" => RWBS_NODIR, "F" => RWBS_FUA,     "A" => RWBS_RAHEAD,
		 "S" => RWBS_SYNC,  "M" => RWBS_META,    "E" => RWBS_SECURE);
my %RWBS_CHAR = (RWBS_FLUSH() => "F",
		 RWBS_WRITE() => "W", RWBS_DISCARD() => "D", RWBS_READ() => "R",
		 RWBS_NODIR() => "N", RWBS_FUA() => "F", RWBS_RAHEAD() => "A",
		 RWBS_SYNC() => "S", RWBS_META() => "M", RWBS_SECURE() => "E");

sub parse_rwbs(@)
{
    my ($event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my @chars = split("", $rwbs);
    my $rwbs_flags = 0;

    if ($chars[0] eq "F") {
	$rwbs_flags |= RWBS_FLUSH;
	shift(@chars);
    }

    foreach my $c (@chars) {
	if ($RWBS_FLAG{$c}) {
	    $rwbs_flags |= $RWBS_FLAG{$c};
	} else {
	    my $devname = kdevname($dev);
	    my $tv64 = to_tv64($common_secs, $common_nsecs);

	    pr_warn("Unknown rwbs flag: ($devname): ",
		    make_io_str($tv64, $rwbs, $sector, $nr_sector));
	}
    }

    return $rwbs_flags;
}

sub rwbs_str($)
{
    my $rwbs_flags = shift;
    my $str = "";

    my $flag = RWBS_FLUSH;
    while ($rwbs_flags) {
	if ($rwbs_flags & $flag) {
	    $str .= $RWBS_CHAR{$flag};
	    $rwbs_flags &= ~$flag;
	}
	$flag <<= 1;
    }

    return lc($str);
}

sub make_io_str($$$$)
{
    my ($tv64, $dir, $sector, $nr_sector) = @_;
    my $time_str = tv64_str($tv64);
    return "$time_str, $dir, $sector + $nr_sector";
}

sub fname_plot_bno($)
{
    my $dev = shift;
    return sprintf("%s_plot_bno.gp", kdevname($dev));
}

sub output_plot_bno($)
{
    my $dev = shift;
    my $fname = fname_plot_bno($dev);

    my $fh = open_file($fname, 0755);

    output_plot_pre($fh, "I/O Position",
		    "Time (secs)", "Disk offset (byte)",
		    "set format y '%.1s %cB'",
		    "set pointsize 0.5",
		    "set bars 0.0",
		    "",
		    "to_byte(blk) = (blk * 512)");

    my $first = 1;
    foreach my $dir ("r", "w", "rs", "ws", "rm", "wm", "rsm", "wsm") {
	foreach my $type ("Queue", "Issue", "Complete") {
	    my $datafile = sprintf("%s_bno_%s_%s.dat", kdevname($dev),
				   lc($type), $dir);

	    print $fh ", \\\n" if (not $first);
	    $first = 0;

	    # Set point at middle of range on a I/O request
#	    print $fh
#		"'$datafile' using 1:(to_byte(\$3 + \$2) / 2) title \"$DIR_LONGNAME{$dir} $type\" with points pointtype 7";
	    # Set a line of range on a I/O request
	    print $fh
		"'$datafile' using 1:(to_byte(\$2)):(to_byte(\$2)):(to_byte(\$3)) title \"$DIR_LONGNAME{$dir} $type\" with yerrorbars";
	}
    }
    print $fh "\n";

    output_plot_post($fh);

    close_file($fh);
}

sub fname_plot_mbps($)
{
    my $dev = shift;
    return sprintf("%s_plot_mbps.gp", kdevname($dev));
}

sub output_plot_mbps($)
{
    my $dev = shift;
    my $fname = fname_plot_mbps($dev);
    my $datafile = sprintf("%s_blkps_c.dat", kdevname($dev));

    my $fh = open_file($fname, 0755);

    output_plot_pre($fh, "Throughput",
		    "Time (secs)", "MB/s",
		    "set yrange [0:]",
		    "",
		    "to_mb(blk) = (blk * 512) / (1024 * 1024)");

    print $fh <<"EOF";
'$datafile' using 1:(to_mb(\$2)) title "I/O" with lines
EOF

    output_plot_post($fh);

    close_file($fh);
}

sub fname_plot_iops($)
{
    my $dev = shift;
    return sprintf("%s_plot_iops.gp", kdevname($dev));
}

sub output_plot_iops($)
{
    my $dev = shift;
    my $fname = fname_plot_iops($dev);
    my $datafile = sprintf("%s_iops_c.dat", kdevname($dev));

    my $fh = open_file($fname, 0755);

    output_plot_pre($fh, "IO/s",
		    "Time (secs)", "IO/s",
		    "set yrange [0:]");

    print $fh <<"EOF";
'$datafile' using 1:2 title "I/O" with lines
EOF

    output_plot_post($fh);

    close_file($fh);
}

sub fname_plot_reqsz($)
{
    my $dev = shift;
    return sprintf("%s_plot_reqsz.gp", kdevname($dev));
}

sub output_plot_reqsz($)
{
    my $dev = shift;
    my $fname = fname_plot_reqsz($dev);

    my $fh = open_file($fname, 0755);

    output_plot_pre($fh, "Request Size",
		    "Time (secs)", "Request size (byte)",
		    "set format y '%.1s %cB'",
		    "set pointsize 0.5",
		    "",
		    "to_byte(blk) = (blk * 512)");

    my @info_reqsz =
	({ type => "queue", dir => "r", color => $R_Q_COLOR },
	 { type => "queue", dir => "w", color => $W_Q_COLOR },
	 { type => "complete", dir => "r", color => $R_C_COLOR },
	 { type => "complete", dir => "w", color => $W_C_COLOR },);
    my $first = 1;
    foreach my $i (@info_reqsz) {
	my $type = $i->{type};
	my $dir = $i->{dir};
	my $color = $i->{color};
	my $datafile = sprintf("%s_reqsz_%s_%s.dat", kdevname($dev),
			       lc($type), $dir);

	print $fh ", \\\n" if (not $first);
	$first = 0;

	print $fh
	    "'$datafile' using 1:(to_byte(\$2)) title \"$DIR_LONGNAME{$dir} $type\" with points linetype ${color}";
    }
    print $fh "\n";

    output_plot_post($fh);

    close_file($fh);
}

sub fname_plot_qdepth($)
{
    my $dev = shift;
    return sprintf("%s_plot_qdepth_c.gp", kdevname($dev));
}

sub output_plot_qdepth($)
{
    my $dev = shift;
    my $fname = fname_plot_qdepth($dev);
    my $datafile = sprintf("%s_qdepth_c.dat", kdevname($dev));

    my $fh = open_file($fname, 0755);

    output_plot_pre($fh, "Queue Depth",
		    "Time (secs)", "Queue Depth (Number of BIOs)",
		    "set yrange [0:]");

    print $fh <<"EOF";
'$datafile' using 1:2 title \"Depth\" with lines
EOF

    output_plot_post($fh);

    close_file($fh);
}

sub fname_plot_lat_x2c($$$)
{
    my $event_name = shift;
    my $dev = shift;
    my $dir = shift;
    my $shortname = $EVENT_SHORTNAME{$event_name};
    return sprintf("%s_plot_lat_%s2c_%s.gp", kdevname($dev),
		   lc($shortname), $dir);
}

sub output_plot_lat_x2c($$$)
{
    my $event_name = shift;
    my $dev = shift;
    my $dir = shift;
    my $shortname = $EVENT_SHORTNAME{$event_name};
    my $fname = fname_plot_lat_x2c($event_name, $dev, $dir);
    my $datafile = sprintf("%s_lat_%s2c_%s.dat", kdevname($dev),
			   lc($shortname), $dir);

    my $fh = open_file($fname, 0755);

    my $title = sprintf("%s to Complete Latency time - (%s)",
			$EVENT_LONGNAME{$event_name} , $DIR_LONGNAME{$dir});
    my $label = sprintf("%s2C", $shortname);

    output_plot_pre($fh, $title,
		    "Time (secs)", "Latency time (secs)",
		    "set yrange [0:]");

    print $fh <<"EOF";
'$datafile' using 1:2 title \"${label}\" with impulses
EOF

    output_plot_post($fh);

    close_file($fh);
}

sub fname_plot_lat_q2c($$)
{
    my $dev = shift;
    my $dir = shift;
    return fname_plot_lat_x2c("block::block_bio_queue", $dev, $dir);
}

sub output_plot_lat_q2c($$)
{
    my $dev = shift;
    my $dir = shift;
    output_plot_lat_x2c("block::block_bio_queue", $dev, $dir);
}

sub fname_plot_lat_d2c($$)
{
    my $dev = shift;
    my $dir = shift;
    return fname_plot_lat_x2c("block::block_rq_issue", $dev, $dir);
}

sub output_plot_lat_d2c($$)
{
    my $dev = shift;
    my $dir = shift;
    output_plot_lat_x2c("block::block_rq_issue", $dev, $dir);
}

sub fname_plot_seek_nr($$)
{
    my $dev = shift;
    my $dir = shift;
    return sprintf("%s_plot_seek_nr_%s.gp", kdevname($dev), $dir);
}

sub output_plot_seek_nr($$)
{
    my $dev = shift;
    my $dir = shift;
    my $fname = fname_plot_seek_nr($dev, $dir);
    my $datafile = sprintf("%s_seek_nr_%s.dat", kdevname($dev), $dir);

    my $fh = open_file($fname, 0755);

    output_plot_pre($fh, "Number of Seeks - ($DIR_LONGNAME{$dir})",
		    "Time (secs)", "Number of Seeks",
		    "set yrange [0:]");

    print $fh <<"EOF";
'$datafile' using 1:2 title \"Seeks\" with lines
EOF

    output_plot_post($fh);

    close_file($fh);
}

sub fname_plot_seek_step($$)
{
    my $dev = shift;
    my $dir = shift;
    return sprintf("%s_plot_seek_step_%s.gp", kdevname($dev), $dir);
}

sub output_plot_seek_step($$)
{
    my $dev = shift;
    my $dir = shift;
    my $fname = fname_plot_seek_step($dev, $dir);
    my $datafile = sprintf("%s_seek_step_%s.dat", kdevname($dev), $dir);

    my $fh = open_file($fname, 0755);

    output_plot_pre($fh, "Seek Steps - ($DIR_LONGNAME{$dir})",
		    "Time (secs)", "Disk offset (byte)",
		    "set format y '%.1s %cB'",
		    "set pointsize 0.5",
		    "",
		    "to_byte(blk) = (blk * 512)");

    print $fh <<"EOF";
'$datafile' using 1:(to_byte(\$2)) title \"D2D seek\" with linespoints
EOF

    output_plot_post($fh);

    close_file($fh);
}

sub open_dev_datfile($$)
{
    my $dev = shift;
    my $postfix = shift;
    my $base = sprintf("%s_%s", kdevname($dev), $postfix);
    return open_datfile($base);
}

# The position after common arguments
use constant DEV_POS => 8;

# FLUSH handling
sub pr_flush_debug(@)
{
#    my ($rwbs_flags,
#	$event_name, $context, $common_cpu, $common_secs,
#	$common_nsecs, $common_pid, $common_comm, $common_callchain,
#	$dev, $sector, $nr_sector, $rwbs, $comm, $msg) = @_;
#    my $time_str = tv64_str(to_tv64($common_secs, $common_nsecs));
#    my $idx = flush_pending_idx($dev);
#
#    print "$event_name: $time_str, $rwbs, $sector, $nr_sector: $idx: $msg\n";
}

#
# FLUSH sequence was changed at kernel-v3.18.
# (block::block_rq_complete() was supported for FLUSH command too)
my $flush_has_complete = 0;
#
# FLUSH only request uses sector==0, so we can't match queue and
# complete. To identify roughly, use unique dummy sector number.
my $pending_sector = -1;
my $complete_sector = -1;
sub get_unique_sector($)
{
    if ($_[0] < -1000000000) {
	$_[0] = -1;
    } else {
	$_[0]--;
    }
    return $_[0];
}

sub is_unique_sector($)
{
    my $sector = shift;
    return $sector < 0;
}

sub unique_pending_sector()
{
    return get_unique_sector($pending_sector);
}

sub unique_complete_sector()
{
    return get_unique_sector($complete_sector);
}

sub get_flush_idx($)
{
    my $dev = shift;
    if (!defined($block_s{$dev}{"flush_idx"})) {
	$block_s{$dev}{"flush_idx"} = 0;
    }
    return $block_s{$dev}{"flush_idx"};
}

sub flush_pending_idx($)
{
    my $dev = shift;
    return get_flush_idx($dev);
}

sub flush_running_idx($)
{
    my $dev = shift;
    return get_flush_idx($dev) ? 0 : 1;
}

sub toggle_flush_idx($)
{
    my $dev = shift;
    $block_s{$dev}{"flush_idx"} = (get_flush_idx($dev) ? 0 : 1);
}

sub flush_add_pending($$)
{
    my $dev = shift;
    my $sector = shift;
    my $idx = flush_pending_idx($dev);

    $block_s{$dev}{"flush_pending_$idx"}{$sector} = 1;
}

sub flush_issue($$)
{
    my $time = shift;
    my $dev = shift;

    my $requeued = $block_s{$dev}{"flush_requeued"};
    delete($block_s{$dev}{"flush_requeued"});

    # If FLUSH command was requeued, update requests on previous pending idx.
    my $idx = $requeued ? flush_running_idx($dev) : flush_pending_idx($dev);

    if (!defined($block_s{$dev}{"flush_pending_$idx"})) {
	# If there is no pending, it's strange. So ignore to avoid to
	# call toggle_flush_idx(). Maybe, missed some block::block_rq_requeue
	# events?
	$requeued = 0 unless ($requeued);
	pr_warn("FLUSH issue without pending: requeue event was missed? ($requeued, $idx)");
	return;
    }

    foreach my $s (keys(%{$block_s{$dev}{"flush_pending_$idx"}})) {
	# If data part was completed, use F2
	my $part = $block_s{$dev}{"pending_w"}{$s}{"D"} ? "F2D" : "F1D";
	$block_s{$dev}{"pending_w"}{$s}{$part} = $time;
    }

    if ($requeued) {
	# If FLUSH was requeued, state machine is already running by 1st issue.
    } else {
	# Switch current pending and running
	toggle_flush_idx($dev);
    }
}

sub flush_complete($$)
{
    my $time = shift;
    my $dev = shift;
    my $idx = flush_running_idx($dev);
    my $done = 0;

    foreach my $s (keys(%{$block_s{$dev}{"flush_pending_$idx"}})) {
	# If data part was completed, use F2
	my $part = $block_s{$dev}{"pending_w"}{$s}{"D"} ? "F2C" : "F1C";
	$block_s{$dev}{"pending_w"}{$s}{$part} = $time;

	# Finished FLUSH command
	$done = 1;
    }

    delete($block_s{$dev}{"flush_pending_$idx"});

    return $done;
}

# Cancel flush_issue()
sub flush_requeue($)
{
    my $dev = shift;

    # Get previous pending
    my $idx = flush_running_idx($dev);

    foreach my $s (keys(%{$block_s{$dev}{"flush_pending_$idx"}})) {
	# If data part was completed, use F2
	my $part = $block_s{$dev}{"pending_w"}{$s}{"D"} ? "F2D" : "F1D";
	delete($block_s{$dev}{"pending_w"}{$s}{$part});
    }

    $block_s{$dev}{"flush_requeued"} = 1;
}

sub flush_done($$)
{
    my $dev = shift;
    my $sector = shift;

    my %flush_req = %{$block_s{$dev}{"pending_w"}{$sector}};
    $flush_req{sector} = $sector;
    push(@{$block_s{$dev}{"flush_done"}}, \%flush_req);
}

use constant F_SUM_F1 => (1 << 0);
use constant F_SUM_D  => (1 << 1);
use constant F_SUM_F2 => (1 << 2);

my %flush_type_map = (
		      "f"	=> F_SUM_F1,
		      "f+d"	=> F_SUM_F1 | F_SUM_D,
		      "d+f"	=> F_SUM_D | F_SUM_F2,
		      "d+fua"	=> F_SUM_D,
		      "f+d+f"	=> F_SUM_F1 | F_SUM_D | F_SUM_F2,
		      "f+d+fua"	=> F_SUM_F1 | F_SUM_D
		     );

sub flush_summalize($)
{
    my $dev = shift;
    my %flush_sum;

    foreach my $req (@{$block_s{$dev}{"flush_done"}}) {
	my $flags = $req->{"flags"};
	my $type;

	if (($flags & (RWBS_FLUSH | RWBS_FUA)) == RWBS_FLUSH) {
	    if ($req->{"nr"} == 0) {
		# FLUSH only
		$type = "f";
	    } else {
		# FLUSH+D
		$type = "f+d";
	    }
	} elsif (($flags & (RWBS_FLUSH | RWBS_FUA)) == RWBS_FUA) {
	    if ($req->{"F2D"}) {
		# D+FLUSH
		$type = "d+f";
	    } else {
		# D+FUA
		$type = "d+fua";
	    }
	} else {
	    if ($req->{"F2D"}) {
		# FLUSH+D+FLUSH
		$type = "f+d+f";
	    } else {
		# FLUSH+D+FUA
		$type = "f+d+fua";
	    }
	}

	my $sum_type = $flush_type_map{$type};
	if ($sum_type & F_SUM_F1) {
	    my $time = $req->{"F1C"} - $req->{"F1D"};
	    num_add($flush_sum{$type}{"f1"}, $time);
	    num_max($flush_sum{$type}{"f1_max"}, $time);
	    num_min($flush_sum{$type}{"f1_min"}, $time);
	}
	if ($sum_type & F_SUM_D) {
	    my $time = $req->{"DC"} - $req->{"D"};
	    num_add($flush_sum{$type}{"d"}, $time);
	    num_max($flush_sum{$type}{"d_max"}, $time);
	    num_min($flush_sum{$type}{"d_min"}, $time);
	}
	if ($sum_type & F_SUM_F2) {
	    my $time = $req->{"F2C"} - $req->{"F2D"};
	    num_add($flush_sum{$type}{"f2"}, $time);
	    num_max($flush_sum{$type}{"f2_max"}, $time);
	    num_min($flush_sum{$type}{"f2_min"}, $time);
	}

	num_add($flush_sum{$type}{"nr"}, 1);
	# Q2C
	my $time = $req->{"C"} - $req->{"Q"};
	num_add($flush_sum{$type}{"q2c"}, $time);
	num_max($flush_sum{$type}{"q2c_max"}, $time);
	num_min($flush_sum{$type}{"q2c_min"}, $time);
    }

    $block_s{$dev}{"flush_sum"} = \%flush_sum;
}

# Output I/O position info
sub add_bno(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $rwbs_s = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE |
					 RWBS_META | RWBS_SYNC));
    my $fname = sprintf("bno_%s_%s", lc($EVENT_LONGNAME{$event_name}), $rwbs_s);

    # Create file if need
    my $fh = open_dev_datfile($dev, $fname);

    # Output I/O position per read or write
    print $fh time_str($common_secs, $common_nsecs), " $sector ",
	$sector + $nr_sector, "\n";
}

# Collect queued blocks (before merge to separate META and non-META)
sub add_queue_io(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE | RWBS_META));

    num_add($block_s{$dev}{"queue_blk_$dir"}, $nr_sector);
    num_add($block_s{$dev}{"queue_blk_c"}, $nr_sector);
    num_add($block_s{$dev}{"queue_io_$dir"}, 1);
    num_add($block_s{$dev}{"queue_io_c"}, 1);
    pr_warn("add_io: unknown dir ($dir)") if ($dir =~ /^r/ and $dir =~ /^w/);
}

# Collect completed blocks and IO/s
sub add_io(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE));
    num_add($block_s{$dev}{"complete_blocks_$dir"}[$cur_time], $nr_sector);
    num_add($block_s{$dev}{"complete_io_$dir"}[$cur_time], 1);
    pr_warn("add_io: unknown dir ($dir)") if ($dir ne "r" and $dir ne "w");
}

# Collect completed blocks and IO/s
sub add_req(@)
{
    my ($key, $rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    num_add($block_s{$dev}{"req_blocks"}{$key}, $nr_sector);
    num_add($block_s{$dev}{"req_nr"}{$key}, 1);
    # Remember maximum sectors on single request
    num_max($block_s{$dev}{"req_max"}{$key}, $nr_sector);
    # Remember minimum sectors on single request
    num_min($block_s{$dev}{"req_min"}{$key}, $nr_sector);

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE));
    my $fname = sprintf("reqsz_%s_%s", lc($key), $dir);
    # Create file if need
    my $fh = open_dev_datfile($dev, $fname);
    # Output request size per read or write
    print $fh time_str($common_secs, $common_nsecs), " $nr_sector\n";
}

# Update queue depth
sub update_qdepth($$$)
{
    my $dev = shift;
    my $time = shift;
    my $num = shift;

    # Modify queue depth
    num_add($block_s{$dev}{"qdepth"}, $num);
    num_max($block_s{$dev}{"qdepth_max"}, $block_s{$dev}{"qdepth"});

    my $fname = "qdepth_c";
    # Create file if need
    my $fh = open_dev_datfile($dev, $fname);

    my $time_str = tv64_str($time);
    print $fh  "$time_str " . $block_s{$dev}{"qdepth"} . "\n";
}

# Sanity check of pending I/O in queue
sub sanity_check_pending(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE));
    my %same;

    foreach my $d ("r", "w") {
	if ($block_s{$dev}{"pending_$d"}) {
	    foreach my $s (keys(%{$block_s{$dev}{"pending_$d"}})) {
		# Check if there is no same block address
		if ($sector <= $s and $s < $sector + $nr_sector) {
		    # There is same block address
		    $same{$s}{"dir"} = $d;
		    $same{$s}{"nr"} = $block_s{$dev}{"pending_$d"}{$s}{"nr"};
		    $same{$s}{"Q"} = $block_s{$dev}{"pending_$d"}{$s}{"Q"};
		}
	    }
	}
    }

    # If there was same block address, warn it
    if (%same) {
	my $prefix = "\n   ";

	my $devname = kdevname($dev);
	my $tv64 = to_tv64($common_secs, $common_nsecs);
	my $str = "$prefix " . make_io_str($tv64, $dir, $sector, $nr_sector);

	# Sort by Queue time
	foreach my $s (sort { $same{$a}{"Q"} <=> $same{$b}{"Q"} } keys(%same)) {
	    my $d = $same{$s}{"dir"};
	    my $nr = $same{$s}{"nr"};

	    $str .= "$prefix " . make_io_str($same{$s}{"Q"}, $d, $s, $nr);
	}

	pr_warn("Found same block address in queue ($devname):$str");
	if (not $opt_no_error) {
	    print STDERR
		"If you want to ignore error, try `--no-error' option\n";
	    exit(1);
	}
    }
}

# Collect Queue(Q) pending I/O
sub add_queue_pending(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE));
    my $time = to_tv64($common_secs, $common_nsecs);

    if ($rwbs_flags & RWBS_FLUSH) {
	pr_flush_debug(@_, "queue");

	if ($nr_sector == 0) {
	    # FLUSH without data, sector number is fake. So, use
	    # unique number to identify.
	    $sector = unique_pending_sector();
	}

	# Remember queued FLUSH request
	flush_add_pending($dev, $sector);
    } elsif ($rwbs_flags & RWBS_FUA) {
	# FUA request without FLUSH
	pr_flush_debug(@_, "queue");
    }

    # Sanity check of pending I/O in queue
    sanity_check_pending(@_) if ($nr_sector);

    # Save pending I/O
    $block_s{$dev}{"pending_$dir"}{$sector}{"Q"} = $time;
    $block_s{$dev}{"pending_$dir"}{$sector}{"nr"} = $nr_sector;
    $block_s{$dev}{"pending_$dir"}{$sector}{"flags"} = $rwbs_flags;

    # Update queue depth
    update_qdepth($dev, $time, 1);
}

sub handle_split_pending(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE));
    my $pend_str = "pending_$dir";
    my $is_split = 0;

    # Is this split request?
    if (exists($block_s{$dev}{$pend_str}{$sector}) &&
	exists($block_s{$dev}{$pend_str}{$sector}{"split"}) &&
	$block_s{$dev}{$pend_str}{$sector}{"split"}) {
	if ($block_s{$dev}{$pend_str}{$sector}{"nr"} != $nr_sector) {
	    my $devname = kdevname($dev);
	    my $time = to_tv64($common_secs, $common_nsecs);
	    pr_warn("$event_name: Split didn't match nr: ($devname): ",
		    make_io_str($time, $dir, $sector, $nr_sector));
	}
	$is_split = 1;
	# Remove split marker
	delete($block_s{$dev}{$pend_str}{$sector});
    }
    return $is_split;
}

# Update Queue pending I/O for split
sub add_split_pending(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $new_sector, $rwbs, $comm) = @_;

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE));
    my $time = to_tv64($common_secs, $common_nsecs);
    my $pend_str = "pending_$dir";

    # Is there original pending?
    if (exists($block_s{$dev}{$pend_str}{$sector}) &&
	$block_s{$dev}{$pend_str}{$sector}{"nr"}) {
	my $orignal = $block_s{$dev}{$pend_str}{$sector}{"nr"};
	my $nr_sector = ($new_sector - $sector);
	my $left = $orignal - $nr_sector;

	# Update $nr_sector of original pending
	$block_s{$dev}{$pend_str}{$sector}{"nr"} = $nr_sector;

	# At least v4.3, block:block_bio_queue is called for new split bio.
	# So, we don't need to add new pending here.
	#
	# But original pending was already written to data files with
	# original $nr_sector. So, we ignore block::block_bio_queue
	# event for split request, to avoid accounting request again.
	#
	# FIXME: If possible, it may be better to account after split
	# requests, not original request.

	# Add split marker
	$block_s{$dev}{$pend_str}{$new_sector}{"split"} = 1;
	$block_s{$dev}{$pend_str}{$new_sector}{"nr"} = $left;

	num_add($block_s{$dev}{"split"}{"nr"}, 1);
	num_add($block_s{$dev}{"split"}{"blocks"}, $orignal);
	return;
    }

    my $devname = kdevname($dev);
    pr_warn("$event_name: Couldn't find Split: ($devname): ",
	    make_io_str($time, $dir, $sector, 0));
}

# Update Queue pending I/O for merge
sub update_pending($$$$$)
{
    my $dev = shift;
    my $dir = shift;
    my $old_sector = shift;
    my $sector = shift;
    my $nr_sector = shift;

    my $q_time = $block_s{$dev}{"pending_$dir"}{$old_sector}{"Q"};
    my $d_time = $block_s{$dev}{"pending_$dir"}{$old_sector}{"D"};
    my $flags = $block_s{$dev}{"pending_$dir"}{$old_sector}{"flags"};
    delete($block_s{$dev}{"pending_$dir"}{$old_sector});

    # Update pending I/O
    $block_s{$dev}{"pending_$dir"}{$sector}{"Q"} = $q_time;
    $block_s{$dev}{"pending_$dir"}{$sector}{"D"} = $d_time;
    $block_s{$dev}{"pending_$dir"}{$sector}{"nr"} = $nr_sector;
    $block_s{$dev}{"pending_$dir"}{$sector}{"flags"} = $flags;
}

# Collect info of FrontMerge pending I/O
sub add_frontmerge_pending(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE));
    my $time = to_tv64($common_secs, $common_nsecs);
    my $sector_end = $sector + $nr_sector;
    my $pend_str = "pending_$dir";

    foreach my $s (keys(%{$block_s{$dev}{$pend_str}})) {
	# There was no queue event for this
	next if (!defined($block_s{$dev}{$pend_str}{$s}{"nr"}));

	if ($sector_end == $s) {
	    my $nr = $block_s{$dev}{$pend_str}{$s}{"nr"};

	    # Remove old pending I/O
	    delete($block_s{$dev}{$pend_str}{$sector});

	    num_add($block_s{$dev}{"frontmerge"}{"nr"}, 1);
	    num_add($block_s{$dev}{"frontmerge"}{"blocks"}, $nr_sector);
	    num_add($block_s{$dev}{"combinemerge"}{"nr"}, 1);
	    num_add($block_s{$dev}{"combinemerge"}{"blocks"}, $nr_sector);
	    # Front merge
	    $nr_sector += $nr;

	    update_pending($dev, $dir, $s, $sector, $nr_sector);
	    # Update queue depth
	    update_qdepth($dev, $time, -1);
	    return;
	}
    }

    my $devname = kdevname($dev);
    pr_warn("$event_name: Couldn't find FrontMerge: ($devname): ",
	    make_io_str($time, $dir, $sector, $nr_sector));
}

# Collect info of merge pending I/O
sub add_backmerge_pending(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE));
    my $time = to_tv64($common_secs, $common_nsecs);
    my $pend_str = "pending_$dir";

    foreach my $s (keys(%{$block_s{$dev}{$pend_str}})) {
	# There was no queue event for this
	next if (!defined($block_s{$dev}{$pend_str}{$s}{"nr"}));

	my $nr = $block_s{$dev}{$pend_str}{$s}{"nr"};

	if ($s + $nr == $sector) {
	    # Remove old pending I/O
	    delete($block_s{$dev}{$pend_str}{$sector});

	    num_add($block_s{$dev}{"backmerge"}{"nr"}, 1);
	    num_add($block_s{$dev}{"backmerge"}{"blocks"}, $nr_sector);
	    num_add($block_s{$dev}{"combinemerge"}{"nr"}, 1);
	    num_add($block_s{$dev}{"combinemerge"}{"blocks"}, $nr_sector);
	    # Back merge
	    $nr += $nr_sector;

	    update_pending($dev, $dir, $s, $s, $nr);
	    # Update queue depth
	    update_qdepth($dev, $time, -1);
	    return;
	}
    }

    my $devname = kdevname($dev);
    pr_warn("$event_name: Couldn't find BackMerge: ($devname): ",
	    make_io_str($time, $dir, $sector, $nr_sector));
}

# Collect Issue(D) pending I/O
sub add_issue_pending(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE));
    my $time = to_tv64($common_secs, $common_nsecs);

    if ($rwbs_flags & RWBS_FLUSH) {
	pr_flush_debug(@_, "issue flush");

	# FLUSH command was issued, update time of all current pending
	flush_issue($time, $dev);
    } else {
	if ($block_s{$dev}{"pending_$dir"}{$sector}) {
	    my $flags = $block_s{$dev}{"pending_$dir"}{$sector}{"flags"};
	    if ($flags & (RWBS_FLUSH | RWBS_FUA)) {
		pr_flush_debug(@_, "issue data");
	    }
	    if ($flags & RWBS_FLUSH) {
		if (!$flush_has_complete) {
		    # Data part issued, this means FLUSH command was done
		    flush_complete($time, $dev);
		}
	    } elsif ($flags & RWBS_FUA) {
		# FUA request without FLUSH
	    }
	}
	# Save pending I/O
	$block_s{$dev}{"pending_$dir"}{$sector}{"D"} = $time;
    }
}

# Complete(C) pending I/O
sub add_complete_pending(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE));
    my $time = to_tv64($common_secs, $common_nsecs);
    my $pend_str = "pending_$dir";

    my $devname = kdevname($dev);
    if ($nr_sector == 0) {
	# Request included FLUSH was done
	if ($sector == 0) {
	    # FLUSH request without data, get from fake sector by FIFO order
	    $sector = unique_complete_sector();
	}
	if (!defined($block_s{$dev}{$pend_str}{$sector})) {
	    # FLUSH command completion
	    my $done = flush_complete($time, $dev);
	    if ($done) {
		pr_flush_debug(@_, "complete flush");
		$flush_has_complete = 1;
	    } else {
		my $devname = kdevname($dev);
		pr_warn("$event_name: Missing queue event: ($devname): ",
			make_io_str($time, $dir, $sector, $nr_sector));
	    }
	    return;
	}

	pr_flush_debug(@_, "done");

	if (!$flush_has_complete) {
	    my $flags = $block_s{$dev}{$pend_str}{$sector}{"flags"};
	    if (is_unique_sector($sector) or
		($flags & RWBS_FUA) and !($rwbs_flags & RWBS_FUA)) {
		# FLUSH only request or FLUSH for converted FUA was done
		flush_complete($time, $dev);
	    }
	}

	# FLUSH request was done
	$block_s{$dev}{$pend_str}{$sector}{"C"} = $time;

	# Remember FLUSH requests
	flush_done($dev, $sector);

	delete($block_s{$dev}{$pend_str}{$sector});
	update_qdepth($dev, $time, -1);
	return;
    } elsif ($block_s{$dev}{$pend_str}{$sector}) {
	my $flags = $block_s{$dev}{$pend_str}{$sector}{"flags"};
	# Data part of FLUSH request, or FUA converted to FLUSH
	if (($flags & RWBS_FLUSH) or
	    (($flags & RWBS_FUA) and !($rwbs_flags & RWBS_FUA))) {
	    pr_flush_debug(@_, "data complete");

	    # FUA converted to FLUSH
	    if (($flags & RWBS_FUA) and !($rwbs_flags & RWBS_FUA)) {
		# This device doesn't support FUA, add pending FLUSH
		flush_add_pending($dev, $sector);
	    }

	    # There is more completion for this request
	    $block_s{$dev}{$pend_str}{$sector}{"DC"} = $time;
	    return;
	} elsif ($rwbs_flags & RWBS_FUA) {
	    pr_flush_debug(@_, "fua done");

	    # FUA request without FLUSH
	    $block_s{$dev}{$pend_str}{$sector}{"DC"} = $time;
	    $block_s{$dev}{$pend_str}{$sector}{"C"} = $time;

	    # Remember FLUSH requests
	    flush_done($dev, $sector);

	    delete($block_s{$dev}{$pend_str}{$sector});
	    update_qdepth($dev, $time, -1);
	    return;
	}
    }

    if (!defined($block_s{$dev}{$pend_str}{$sector}) ||
	!defined($block_s{$dev}{$pend_str}{$sector}{"nr"})) {
	my $devname = kdevname($dev);
	pr_warn("$event_name: Missing queue event: ($devname): ",
		make_io_str($time, $dir, $sector, $nr_sector));
	return;
    }

    # Find completed pending I/O (pending I/O may be merged)
    my ($q_time, $d_time);
    while ($block_s{$dev}{$pend_str}{$sector} and $nr_sector) {
	my $nr = $block_s{$dev}{$pend_str}{$sector}{"nr"};
	if ($nr <= $nr_sector) {
	    $q_time = min($q_time, $block_s{$dev}{$pend_str}{$sector}{"Q"});
	    $d_time = min($d_time, $block_s{$dev}{$pend_str}{$sector}{"D"});
	    delete($block_s{$dev}{$pend_str}{$sector});

	    # Update queue depth
	    update_qdepth($dev, $time, -1);

	    $sector += $nr;
	    $nr_sector -= $nr;
	} else {
	    # partial complete
	    $nr -= $nr_sector;
	    $nr_sector = 0;
	    update_pending($dev, $dir, $sector, $sector + $nr_sector, $nr);
	}
    }
    if (!defined($q_time) or !defined($d_time)) {
	my $devname = kdevname($dev);
	die "$event_name: Found bogus I/O completion: ($devname): ",
	    make_io_str($time, $dir, $sector, $nr_sector), "\n";
    }
    my $lat_q2c = $time - $q_time;
    my $lat_d2c = $time - $d_time;

    # Remember block_s of I/O latency
    foreach my $d ($dir, "c") {
	num_add($block_s{$dev}{"lat_q2c_total_$d"}, $lat_q2c);
	num_max($block_s{$dev}{"lat_q2c_max_$d"}, $lat_q2c);
	num_min($block_s{$dev}{"lat_q2c_min_$d"}, $lat_q2c);
	num_add($block_s{$dev}{"lat_q2c_nr_$d"}, 1);

	num_add($block_s{$dev}{"lat_d2c_total_$d"}, $lat_d2c);
	num_add($block_s{$dev}{"lat_d2c_nr_$d"}, 1);
	num_max($block_s{$dev}{"lat_d2c_max_$d"}, $lat_d2c);
	num_min($block_s{$dev}{"lat_d2c_min_$d"}, $lat_d2c);
    }

    for my $d ($dir, "c") {
	my $fname_q2c = sprintf("lat_q2c_%s", $d);
	my $fname_d2c = sprintf("lat_d2c_%s", $d);

	my $time_str = tv64_str($time);

	# Output Q2C latency time
	my $fh_q2c = open_dev_datfile($dev, $fname_q2c);
	my $lat_q2c_str = tv64_str($lat_q2c);
	print $fh_q2c "$time_str $lat_q2c_str\n";

	# Output D2C latency time
	my $fh_d2c = open_dev_datfile($dev, $fname_d2c);
	my $lat_d2c_str = tv64_str($lat_d2c);
	print $fh_d2c "$time_str $lat_d2c_str\n";
    }
}

# Requeue(R) pending I/O
sub add_requeue_pending(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE));

    if ($rwbs_flags & RWBS_FLUSH) {
	pr_flush_debug(@_, "requeue");
	flush_requeue($dev);
    } else {
	delete($block_s{$dev}{"pending_$dir"}{$sector}{"D"});
    }
}

sub seek_distance($$$$)
{
    my ($start, $end, $last_start, $last_end) = @_;

    if (!$opt_seek_relative) {
	# Absolute seek
	return abs($last_end - $start);
    }

    # Relative seek
    die "unimplemented yet";
    return 0;
}

sub add_seek_distance($$$$$)
{
    my ($dir, $dev, $time, $start, $end) = @_;
    my $distance = 0;

    # Ignore first seek
    if (defined($block_s{$dev}{"last_end_$dir"})) {
	$distance = seek_distance($start, $end,
				  $block_s{$dev}{"last_start_$dir"},
				  $block_s{$dev}{"last_end_$dir"});
	if ($distance > $opt_seek_threshold) {
	    num_add($block_s{$dev}{"seek_nr_$dir"}[$cur_time], 1);
	    num_add($block_s{$dev}{"seek_distance_$dir"}[$cur_time], $distance);
	} else {
	    $distance = 0;
	}
    }
    # Update last location
    $block_s{$dev}{"last_time_$dir"} = $time;
    $block_s{$dev}{"last_start_$dir"} = $start;
    $block_s{$dev}{"last_end_$dir"} = $end;

    return $distance;
}

# Collect issued block
sub add_seek(@)
{
    my ($rwbs_flags,
	$event_name, $context, $common_cpu, $common_secs,
	$common_nsecs, $common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $dir = rwbs_str($rwbs_flags & (RWBS_READ | RWBS_WRITE));
    my $time = to_tv64($common_secs, $common_nsecs);
    my $distance;

    foreach my $d ($dir, "c") {
	my $fname = sprintf("seek_step_%s", $d);
	my $last_time = $block_s{$dev}{"last_time_$d"} || 0;
	my $last_end = $block_s{$dev}{"last_end_$d"} || 0;

	# Add seek distance
	$distance = add_seek_distance($d, $dev, $time,
				      $sector, $sector + $nr_sector);
	# Create file if need
	my $fh = open_dev_datfile($dev, $fname);

	# Output seek step
	my $end = $sector + $nr_sector;
#	print $fh "#$time $sector $end\n";
	if ($distance) {
	    my $last_time_str = tv64_str($last_time);
	    my $time_str = tv64_str($time);
	    print $fh "\n";
	    print $fh "$last_time_str $last_end\n";
	    print $fh "$time_str $sector\n";
	}
    }
}

sub block_main
{
    my @devs = sort { $a <=> $b } keys(%block_s);
    my %result;

    # Close all to prevent to closed $log
    close_file_all();

    my $log = open_file("fsperf-block.log", 0644, 1);

    calc_start_end_time();

    # Output MB/s and IO/s
    foreach my $dev (@devs) {
	my %total_blk = ("r" => 0, "w" => 0);
	my %total_io = ("r" => 0, "w" => 0);

	my $fh_blk = open_dev_datfile($dev, "blkps_c");
	my $fh_io = open_dev_datfile($dev, "iops_c");
	for my $t (to_sec($perf_start)..to_sec($perf_end)) {
	    my $blkps_c = 0;
	    my $iops_c = 0;

	    foreach my $dir ("r", "w") {
		my $complete_blocks = $block_s{$dev}{"complete_blocks_$dir"};
		my $complete_io = $block_s{$dev}{"complete_io_$dir"};
		my $blocks = ($complete_blocks->[$t] || 0);
		my $io = ($complete_io->[$t] || 0);

		$total_blk{$dir} += $blocks;
		$total_io{$dir} += $io;

		$blkps_c += $blocks;
		$iops_c += $io;
	    }

	    print $fh_blk "$t.5 $blkps_c\n";
	    print $fh_io "$t.5 $iops_c\n";
	}
	close_file($fh_blk);
	close_file($fh_io);

	# Remember for short summary
	foreach my $dir ("r", "w") {
	    $result{$dev}{"total_blk_$dir"} = $total_blk{$dir};
	    $result{$dev}{"total_io_$dir"} = $total_io{$dir};
	    num_add($result{$dev}{"total_blk_c"}, $total_blk{$dir});
	    num_add($result{$dev}{"total_io_c"}, $total_io{$dir});
	}

	# Create plot script
	output_plot_bno($dev);
	output_plot_mbps($dev);
	output_plot_iops($dev);
    }

    # Summary Time
    print $log <<"EOF";
                      Time
----------------------------------------------------------
  Dev         Start(sec)           End(sec)        Elapse(sec)
EOF
    my $elapse = $perf_end - $perf_start;

    foreach my $dev (@devs) {
	printf $log " %4s    %15s    %15s    %15s\n",
	    kdevname($dev), tv64_str($perf_start), tv64_str($perf_end),
	    tv64_str($elapse);
    }

    # Summary IO (Queue)
    print $log <<"EOF";

                      IO (Queue)
-----------------------------------------------------------------
  Dev    Direction     MB/s     Total(MB)         IO/s     Total(IO)
EOF
    foreach my $dev (@devs) {
	foreach my $dir ("r", "rm", "w", "wm", "c") {
	    my $total_blk = $block_s{$dev}{"queue_blk_$dir"} || 0;
	    my $total_io = $block_s{$dev}{"queue_io_$dir"} || 0;
	    my $total_mb = ($total_blk * 512) / (1024 * 1024);
	    printf $log " %4s   %10s %8.2f      %8.2f     %8.2f      %8u\n",
		kdevname($dev), $DIR_LONGNAME{$dir},
		$total_mb / to_float_tv64($elapse),
		$total_mb, $total_io / to_float_tv64($elapse),
		$total_io;
	}
    }

    # Summary IO (Complete)
    print $log <<"EOF";

                      IO (Complete)
-----------------------------------------------------------------
  Dev    Direction     MB/s     Total(MB)         IO/s     Total(IO)
EOF
    foreach my $dev (@devs) {
	foreach my $dir ("r", "w", "c") {
	    my $total_blk = $result{$dev}{"total_blk_$dir"};
	    my $total_io = $result{$dev}{"total_io_$dir"};
	    my $total_mb = ($total_blk * 512) / (1024 * 1024);
	    printf $log " %4s   %10s %8.2f      %8.2f     %8.2f      %8u\n",
		kdevname($dev), $DIR_LONGNAME{$dir},
		$total_mb / to_float_tv64($elapse),
		$total_mb, $total_io / to_float_tv64($elapse),
		$total_io;
	}
    }

    # Summary Request size
    print $log <<"EOF";

                    Request size
-----------------------------------------------------------------
  Dev      Type       NR      Avg    Min    Max  (1 == 512 bytes)
EOF
    foreach my $dev (@devs) {
	foreach my $type ("Queue", "Complete") {
	    my $key = lc($type);
	    my $req_blocks = $block_s{$dev}{"req_blocks"}{$key} || 0;
	    my $req_nr = $block_s{$dev}{"req_nr"}{$key} || 0;
	    my $req_max = $block_s{$dev}{"req_max"}{$key} || 0;
	    my $req_min = $block_s{$dev}{"req_min"}{$key} || 0;

	    # Output short summary
	    my $avg = $req_nr ? ($req_blocks / $req_nr) : 0;
	    printf $log " %4s  %8s %8u %8.2f  %5u  %5u\n",
		kdevname($dev), $type, $req_nr, $avg, $req_min, $req_max;
	}

	# Create plot script
	output_plot_reqsz($dev);
    }

    # Summary Split Request
    print $log <<"EOF";

                    Split Request
-----------------------------------------------------------------
  Dev        NR      Avg      Total               (1 == 512 bytes)
EOF
    foreach my $dev (@devs) {
	my $nr = $block_s{$dev}{"split"}{"nr"} || 0;
	my $blocks = $block_s{$dev}{"split"}{"blocks"} || 0;

	# Output short summary
	my $avg = $nr ? ($blocks / $nr) : 0;
	printf $log " %4s  %8u %8.2f   %8u\n",
	    kdevname($dev), $nr, $avg, $blocks;
    }

    # Summary Merged Request
    print $log <<"EOF";

                    Merged Request
-----------------------------------------------------------------
  Dev      Type       NR      Avg      Total      (1 == 512 bytes)
EOF
    foreach my $dev (@devs) {
	foreach my $type ("Front", "Back", "Combine") {
	    my $key = lc($type) . "merge";
	    my $nr = $block_s{$dev}{$key}{"nr"} || 0;
	    my $blocks = $block_s{$dev}{$key}{"blocks"} || 0;

	    # Output short summary
	    my $avg = $nr ? ($blocks / $nr) : 0;
	    printf $log " %4s  %8s %8u %8.2f   %8u\n",
		kdevname($dev), $type, $nr, $avg, $blocks;
	}
    }

    # Summary Queue depth
    print $log <<"EOF";

                    Queue Depth
-----------------------------------------------------------------
  Dev      Max
EOF
    foreach my $dev (@devs) {
	# Output short summary
	my $max = $block_s{$dev}{"qdepth_max"} || 0;

	printf $log " %4s     %4u\n",
	    kdevname($dev), $max;

	# Create plot script
	output_plot_qdepth($dev);
    }

    # Summary Q2C/D2C Latency
    print $log <<"EOF";

                    Latency time
-----------------------------------------------------------------
  Dev   Type  Direction          Avg          Min          Max    (secs)
EOF
    foreach my $dev (@devs) {
	foreach my $dir ("r", "w", "c") {
	    foreach my $type ("q2c", "d2c") {
		# Output short summary
		my $total = $block_s{$dev}{"lat_${type}_total_${dir}"} || 0;
		my $nr = $block_s{$dev}{"lat_${type}_nr_${dir}"} || 0;
		my $max = $block_s{$dev}{"lat_${type}_max_${dir}"} || 0;
		my $min = $block_s{$dev}{"lat_${type}_min_${dir}"} || 0;
		my $avg = $nr ? ($total / $nr) : 0;

		printf $log " %4s   %4s   %8s  %s  %s  %s\n",
		    kdevname($dev), uc($type), $DIR_LONGNAME{$dir},
		    tv64_str($avg), tv64_str($min), tv64_str($max);
	    }

	    # Create plot script
	    output_plot_lat_q2c($dev, $dir);
	    output_plot_lat_d2c($dev, $dir);
	}
    }

    # Summary FLUSH requests
    for my $dev (@devs) {
	flush_summalize($dev);
    }

    print $log <<"EOF";

                    FLUSH/FUA Latency time
-----------------------------------------------------------------
  Dev  Req          NR  Type           Avg          Min          Max    (secs)
EOF
    foreach my $dev (@devs) {
	foreach my $type ("f", "f+d", "d+f", "d+fua", "f+d+f", "f+d+fua") {
	    next if (not $block_s{$dev}{"flush_sum"}{$type}{"nr"});

	    # Output short summary
	    my $nr = $block_s{$dev}{"flush_sum"}{$type}{"nr"};
	    my $sum_type = $flush_type_map{$type};
	    my @reqs = ("q2c");
	    if ($sum_type & F_SUM_F1) {
		push(@reqs, "f1");
	    }
	    if ($sum_type & F_SUM_D) {
		push(@reqs, "d");
	    }
	    if ($sum_type & F_SUM_F2) {
		push(@reqs, "f2");
	    }

	    foreach my $req (@reqs) {
		my $total = $block_s{$dev}{"flush_sum"}{$type}{"$req"};
		my $max = $block_s{$dev}{"flush_sum"}{$type}{"${req}_max"};
		my $min = $block_s{$dev}{"flush_sum"}{$type}{"${req}_min"};
		my $avg = $total / $nr;

		if ($req eq "q2c") {
		    printf $log " %4s  %-8s %6u  %-4s   %s  %s  %s\n",
			kdevname($dev), uc($type), $nr, uc($req),
			tv64_str($avg), tv64_str($min), tv64_str($max);
		} else {
		    printf $log " %4s  %-8s %6s  %-4s   %s  %s  %s\n",
			" ", " ", " ", uc($req),
			tv64_str($avg), tv64_str($min), tv64_str($max);
		}
	    }
	}
    }

    # Output/Summary Seeks/s
    print $log <<"EOF";

                      Seeks/s (Issue)
-----------------------------------------------------------------
  Dev   Direction    Seeks/s  Total(Seeks)  Avg Distance(MB) Total Distance(MB)
EOF
    foreach my $dev (@devs) {
	foreach my $dir ("r", "w", "c") {
	    my $fname_nr = "seek_nr_$dir";
	    my $fname_distance = "seek_distance_$dir";
	    my $total_nr = 0;
	    my $total_distance = 0;

	    my $fh = open_dev_datfile($dev, $fname_nr);
	    for my $t (to_sec($perf_start)..to_sec($perf_end)) {
		my $nr = $block_s{$dev}{$fname_nr}[$t] || 0;
		my $distance = $block_s{$dev}{$fname_distance}[$t] || 0;

		$total_nr += $nr;
		$total_distance += $distance;
		print $fh "$t.5 ", $nr, "\n";
	    }
	    close_file($fh);

	    # Output short summary
	    my $avg_nr = $total_nr / to_float_tv64($elapse);
	    my $avg_distance = $total_nr ? ($total_distance / $total_nr) : 0;
	    printf $log " %4s    %8s   %8.2f      %8.2f      %8.2f     %12.2f\n",
		kdevname($dev), $DIR_LONGNAME{$dir}, $avg_nr, $total_nr,
		($avg_distance * 512) / (1024 * 1024),
		($total_distance * 512) / (1024 * 1024);

	    # Create plot script
	    output_plot_seek_nr($dev, $dir);
	    output_plot_seek_step($dev, $dir);
	}
    }
    close_file($log);

    # Create summary plot
#    foreach my $dev (@devs) {
#	output_plot_summary($dev, 0);
#    }

    # Sanity check for pending I/O
    foreach my $dev (@devs) {
	my $devname = kdevname($dev);

	foreach my $dir ("r", "w") {
	    if ($block_s{$dev}{"pending_$dir"}) {
		foreach my $s (keys(%{$block_s{$dev}{"pending_$dir"}})) {
		    my $nr = $block_s{$dev}{"pending_$dir"}{$s}{"nr"} || 0;
		    my $q_time = $block_s{$dev}{"pending_$dir"}{$s}{"Q"} || 0;
		    my $d_time = $block_s{$dev}{"pending_$dir"}{$s}{"D"} || 0;
		    my $q_str = tv64_str($q_time);
		    my $d_str = tv64_str($d_time);

		    pr_warn("Missing Pending I/O: ($devname): ",
			    make_io_str($q_time, $dir, $s, $nr),
			    ", $d_str");
		}
	    }
	}
    }

    # Remember $perf_xstart and $perf_xend
    $ENV{FSPERF_XSTART} = $perf_xstart;
    $ENV{FSPERF_XEND} = $perf_xend;
    $ENV{FSPERF_DEV} = join(",", keys(%block_s));

    return 0;
}

# remap partion to whole disk: $dev
sub block_remap_dev
{
    my $args = shift;
    $args->[DEV_POS] = get_whole_dev($args->[DEV_POS]);
}

#sub block::block_rq_remap
#{
#    perf_args_normalize(\@_);
#    block_remap_dev(\@_);
#
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$dev, $sector, $nr_sector, $old_dev, $old_sector, $rwbs) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub block::block_bio_remap
#{
#    perf_args_normalize(\@_);
#    block_remap_dev(\@_);
#
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$dev, $sector, $nr_sector, $old_dev, $old_sector, $rwbs) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

sub block::block_split
{
    perf_args_normalize(\@_);
    block_remap_dev(\@_);

    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$dev, $sector, $new_sector, $rwbs, $comm) = @_;

    update_cur_time($common_secs, $common_nsecs);

    my $rwbs_flags = parse_rwbs(@_);
    if ($rwbs_flags & (RWBS_READ | RWBS_WRITE)) {
	add_split_pending($rwbs_flags, @_);
    } else {
	my $devname = kdevname($dev);
	my $tv64 = to_tv64($common_secs, $common_nsecs);

	pr_warn("$event_name: Ignore unknown direction: ($devname): ",
		make_io_str($tv64, $rwbs, $sector, 0));
    }
}

#sub block::block_unplug
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$nr_rq, $comm) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub block::block_plug
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$comm) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub block::block_sleeprq
#{
#    perf_args_normalize(\@_);
#    block_remap_dev(\@_);
#
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub block::block_getrq
#{
#    perf_args_normalize(\@_);
#    block_remap_dev(\@_);
#
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

sub block::block_bio_queue
{
    perf_args_normalize(\@_);
    block_remap_dev(\@_);

    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    my $rwbs_flags = parse_rwbs(@_);

    update_cur_time($common_secs, $common_nsecs);

    # Unknown no data request?
    if (!($rwbs_flags & RWBS_FLUSH) and $nr_sector == 0) {
	my $devname = kdevname($dev);
	my $tv64 = to_tv64($common_secs, $common_nsecs);

	pr_warn("$event_name: Ignore no data request: ($devname): ",
		make_io_str($tv64, $rwbs, $sector, $nr_sector));
	return;
    }

    if ($rwbs_flags & (RWBS_READ | RWBS_WRITE)) {
	if ($nr_sector) {
	    if (handle_split_pending($rwbs_flags, @_)) {
		# This is split request. Accounting was already done by
		# $nr_sector in original request.
	    } else {
		add_bno($rwbs_flags, @_);
		add_req("queue", $rwbs_flags, @_);
		add_queue_io($rwbs_flags, @_);
	    }
	}
	add_queue_pending($rwbs_flags, @_);
    } else {
	my $devname = kdevname($dev);
	my $tv64 = to_tv64($common_secs, $common_nsecs);

	pr_warn("$event_name: Ignore unknown direction: ($devname): ",
		make_io_str($tv64, $rwbs, $sector, $nr_sector));
    }
}

sub block::block_bio_frontmerge
{
    perf_args_normalize(\@_);
    block_remap_dev(\@_);

    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    update_cur_time($common_secs, $common_nsecs);

    my $rwbs_flags = parse_rwbs(@_);
    if ($rwbs_flags & (RWBS_READ | RWBS_WRITE)) {
	add_frontmerge_pending($rwbs_flags, @_);
    } else {
	my $devname = kdevname($dev);
	my $tv64 = to_tv64($common_secs, $common_nsecs);

	pr_warn("$event_name: Ignore unknown direction: ($devname): ",
		make_io_str($tv64, $rwbs, $sector, $nr_sector));
    }
}

sub block::block_bio_backmerge
{
    perf_args_normalize(\@_);
    block_remap_dev(\@_);

    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;

    update_cur_time($common_secs, $common_nsecs);

    my $rwbs_flags = parse_rwbs(@_);
    if ($rwbs_flags & (RWBS_READ | RWBS_WRITE)) {
	add_backmerge_pending($rwbs_flags, @_);
    } else {
	my $devname = kdevname($dev);
	my $tv64 = to_tv64($common_secs, $common_nsecs);

	pr_warn("$event_name: Ignore unknown direction: ($devname): ",
		make_io_str($tv64, $rwbs, $sector, $nr_sector));
    }
}

#sub block::block_bio_complete
#{
#    perf_args_normalize(\@_);
#    block_remap_dev(\@_);
#
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$dev, $sector, $nr_sector, $error, $rwbs) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub block::block_bio_bounce
#{
#    perf_args_normalize(\@_);
#    block_remap_dev(\@_);
#
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$dev, $sector, $nr_sector, $rwbs, $comm) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

sub block::block_rq_issue
{
    perf_args_normalize(\@_);
    block_remap_dev(\@_);

    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $bytes, $rwbs, $comm, $cmd) = @_;

    my @normalized_args = @_;
    splice(@normalized_args, DEV_POS + 3, 1);	# remove $bytes
    splice(@normalized_args, DEV_POS + 5);	# remove $cmd

    my $rwbs_flags = parse_rwbs(@normalized_args);

    update_cur_time($common_secs, $common_nsecs);

    # Unknown no data request?
    if (!($rwbs_flags & RWBS_FLUSH) and $nr_sector == 0) {
	my $devname = kdevname($dev);
	my $tv64 = to_tv64($common_secs, $common_nsecs);

	pr_warn("$event_name: Ignore no data request: ($devname): ",
		make_io_str($tv64, $rwbs, $sector, $nr_sector));
	return;
    }

    if ($rwbs_flags & (RWBS_READ | RWBS_WRITE)) {
	if ($nr_sector) {
	    add_bno($rwbs_flags, @normalized_args);
	    add_seek($rwbs_flags, @normalized_args);
	}
	add_issue_pending($rwbs_flags, @normalized_args);
    } else {
	my $devname = kdevname($dev);
	my $tv64 = to_tv64($common_secs, $common_nsecs);

	pr_warn("$event_name: Ignore unknown direction: ($devname): ",
		make_io_str($tv64, $rwbs, $sector, $nr_sector));
    }
}

#sub block::block_rq_insert
#{
#    perf_args_normalize(\@_);
#    block_remap_dev(\@_);
#
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$dev, $sector, $nr_sector, $bytes, $rwbs, $comm, $cmd) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

sub block::block_rq_complete
{
    perf_args_normalize(\@_);
    block_remap_dev(\@_);

    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $errors, $rwbs, $cmd) = @_;

    my @normalized_args = @_;
    splice(@normalized_args, DEV_POS + 3, 1); # remove $errors

    my $rwbs_flags = parse_rwbs(@normalized_args);

    update_cur_time($common_secs, $common_nsecs);

    if ($rwbs_flags & (RWBS_READ | RWBS_WRITE)) {
	if ($nr_sector) {
	    add_bno($rwbs_flags, @normalized_args);
	    add_io($rwbs_flags, @normalized_args);
	    add_req("complete", $rwbs_flags, @normalized_args);
	}
	add_complete_pending($rwbs_flags, @normalized_args);
    } else {
	my $devname = kdevname($dev);
	my $tv64 = to_tv64($common_secs, $common_nsecs);

	pr_warn("$event_name: Ignore unknown direction: ($devname): ",
		make_io_str($tv64, $rwbs, $sector, $nr_sector));
    }
}

sub block::block_rq_requeue
{
    perf_args_normalize(\@_);
    block_remap_dev(\@_);

    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$dev, $sector, $nr_sector, $errors, $rwbs, $cmd) = @_;

    my @normalized_args = @_;
    splice(@normalized_args, DEV_POS + 3, 1); # remove $errors

    my $rwbs_flags = parse_rwbs(@normalized_args);

    update_cur_time($common_secs, $common_nsecs);

    if ($rwbs_flags & (RWBS_READ | RWBS_WRITE)) {
	add_requeue_pending($rwbs_flags, @normalized_args);
    } else {
	my $devname = kdevname($dev);
	my $tv64 = to_tv64($common_secs, $common_nsecs);

	pr_warn("$event_name: Ignore unknown direction: ($devname): ",
		make_io_str($tv64, $rwbs, $sector, $nr_sector));
    }
}

#sub block::block_rq_abort
#{
#    perf_args_normalize(\@_);
#    block_remap_dev(\@_);
#
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$dev, $sector, $nr_sector, $errors, $rwbs, $cmd) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

##################################
#
# sched events
#

use constant TASK_RUNNING		=> 0;
use constant TASK_INTERRUPTIBLE		=> 1;
use constant TASK_UNINTERRUPTIBLE	=> 2;
use constant TASK_STOPPED		=> 4;
use constant TASK_TRACED		=> 8;
use constant EXIT_ZOMBIE		=> 16;
use constant EXIT_DEAD			=> 32;
use constant TASK_DEAD			=> 64;
use constant TASK_WAKEKILL		=> 128;
use constant TASK_WAKING		=> 256;
use constant TASK_PARKED		=> 512;

sub fname_plot_sched_summary()
{
    return "sched_summary.gp";
}

sub output_plot_sched_summary_pre($)
{
    my $nr_plots = shift;
    my $fname_summary = fname_plot_sched_summary();
    my $fh = open_file($fname_summary, 0755);

    my $height = 20 * $nr_plots;
    my $width = 800;
    my $fontscale = 0.8;

    print $fh <<"EOF"
#!/usr/bin/gnuplot

set term png truecolor size ${width},${height} fontscale ${fontscale}
set output 'sched_summary.png'
set tmargin 0
set bmargin 0
set multiplot layout ${nr_plots},1 columnsfirst scale 1.0,1.0 offset 0.0,0.0
unset xtics

EOF
}

sub output_plot_sched_summary()
{
    # Close all cached FH
    close_file_all();

    my $oldpwd = getcwd();
    chdir($output_dir);

    # Run summary.gp
    my $fname = fname_plot_sched_summary();
    my $cmd = "./$fname";
    safe_system_with_output("$fname.output", $cmd);

    chdir($oldpwd);

    # Move .png to current dir
    $cmd = "mv $output_dir/*.png .";
    safe_system($cmd);
}

sub fname_plot_sched($)
{
    my $id = shift;
    return "sched_$id.gp";
}

# Output sched_*-*.gp and sched_summary.gp
sub output_plot_sched($$)
{
    my $id = shift;
    my $comm = shift;
    my $fname = fname_plot_sched($id);
    my $fname_summary = fname_plot_sched_summary();
    my $datafile_time = "sched_${id}_time.dat";
    my $datafile_stat = "sched_${id}_stat.dat";

    foreach my $n (0 .. 1) {
	my $ylow = -0.4;
	my $ylen = 0.025;
	my ($fh, $title, $xlabel, $ylabel, $ymin, $ymax, $custom);

	if ($n == 0) {
	    my $type = is_wid($id) ? "Workqueue" : "Task";
	    # sched_*-*.gp
	    $fh = open_file($fname, 0755);
	    $title = "$type $id ($comm) Schedule";
	    $xlabel = "Time (secs)";
	    $ylabel = "Schedule Time (secs)";
	    $ymin = $ylow - 0.1;
	    $ymax = 1.1;
	} else {
	    # sched_summary.gp
	    $fh = open_file($fname_summary, 0755);
	    $title = undef;
	    $ylabel = undef;
	    # yrange cuts only @sched_tpes bars area
	    $ymin = $ylow;
	    $ymax = $ylow + $ylen * $sched_total_height;
	    $custom = "set xlabel \"$id ($comm)\" offset 0,1.5";
	}

	output_plot_pre($fh, $title, $xlabel, $ylabel,
			"set yrange [$ymin:$ymax]",
			"set ytics 0,0.1,1",
			"set style fill transparent solid 0.75 noborder",
			"set datafile missing '$plot_missing_char'",
			"",
			"ylow = $ylow",
			"ylen = $ylen",
			$custom);

	my $need_comma = 0;
	if ($n == 0) {
	    # Line graph
	    foreach my $type (@sched_types) {
		my $info = $sched_graph_info{$type};
		my $col = $info->{col};
		my $name = $info->{name};
		my $color = $info->{color};

		print $fh ", \\\n" if ($need_comma);
		print $fh "'$datafile_stat' using 1:${col} title \"${name}\" with lines linetype ${color}";

		$need_comma = 1;
	    }
	}

	# Bars
	my $base = ($n == 0) ? 7 : 0;
	my $ypos_h = $base + $sched_total_height;
	foreach my $type (@sched_types) {
	    my $info = $sched_graph_info{$type};
	    my $col = $info->{col};
	    my $color = $info->{color};
	    my $ypos_l = $ypos_h - $info->{height};

	    # For bar each schedule type
	    if ($need_comma) {
		print $fh ", \\\n";
	    }
	    print $fh
		"'$datafile_time' using 1:(ylow):1:${col}:(ylow + ylen * ${ypos_l}):(ylow + ylen * ${ypos_h}) notitle with boxxyerrorbars linetype ${color}";
	    if ($n == 0) {
		# For bar of combined schedule type
		print $fh ", \\\n";
		print $fh
		    "'$datafile_time' using 1:(ylow):1:${col}:(ylow + ylen * 0):(ylow + ylen * 5) notitle with boxxyerrorbars linetype ${color}";
	    }

	    $ypos_h = $ypos_l;
	    $need_comma = 1;
	}
	print $fh "\n";

	if ($n == 0) {
	    output_plot_post($fh);
	    close_file($fh);
	} else {
	    print $fh "\n";
	}
    }
}

sub open_id_datfile($$)
{
    my $id = shift;
    my $postfix = shift;
    my $base = sprintf("sched_%s_%s", $id, $postfix);
    return open_datfile($base);
}

sub is_interesting_pid($)
{
    my $pid = shift;

    if (!scalar(@opt_target_pid) or grep { $_ == $pid } @opt_target_pid) {
	return 1;
    }
    return 0;
}

sub sched_stat_process
{
    my $type = shift;
    my $id = shift;
    my $comm = shift;
    my $time = shift;
    my $elapse = shift;

    my $fh = open_id_datfile($id, "time");

    my $start = $time - $elapse;
    my $start_str = tv64_str($start);
    my $end = $time;
    my $end_str = tv64_str($end);

    # Output sched_*_time.dat

    # fields are "start_str" + "one" for each types
    my @cols = ($plot_missing_char) x (1 + scalar(@sched_types));
    $cols[0] = $start_str;
    $cols[$sched_graph_info{$type}{col} - 1] = $end_str;

    print $fh "@cols\n";

    # Remember stat
    $sched_s{$id}{comm} = $comm;
    num_min($sched_s{$id}{start}, $start);
    num_max($sched_s{$id}{end}, $end);
    num_add($sched_s{$id}{$type}{total}, $elapse);
    # Add elapse to proper position
    my $cur_sec = to_sec($end);
    while ($elapse > 0) {
	my $cur_start = to_tv64($cur_sec, 0);
	my $cur_elapse = min($end - $cur_start, $elapse);

	num_add($sched_s{$id}{$type}{sec}{$cur_sec}, $cur_elapse);
	$elapse -= $cur_elapse;

	$end = $cur_start;
	$cur_sec--;
    }
}

# Collect pid/wid time based on sched_stat_*
sub sched_stat($$$$$)
{
    my $type = shift;
    my $pid = shift;
    my $comm = shift;
    my $time = shift;
    my $elapse = shift;

    # Add stat as process
    if (is_interesting_pid($pid)) {
	sched_stat_process($type, $pid, $comm, $time, $elapse);
    }

    # Check workqueue work
    foreach my $wid (wids_in_range($pid, $time, $elapse)) {
	my $time_start = $time - $elapse;
	my $work_start = $wq_state{$pid}{$wid}{start};
	# If {end} is undef, work is still running
	my $work_end = $wq_state{$pid}{$wid}{end} || $time;
	my $sym = $wq_state{$pid}{$wid}{sym};

	my $start = max($time_start, $work_start);
	my $end = min($time, $work_end);

	# Add stat as workqueue work
	sched_stat_process($type, $wid, $sym, $end, $end - $start);
    }
}

# Collect pid/wid time based on sched_switch
sub sched_stat_on_switch($$$$)
{
    my $prev_type = shift;
    my $prev_pid = shift;
    my $next_pid = shift;
    my $time = shift;

    # Ignore swapper
    if ($prev_pid != 0) {
	# Running prev_pid switched to next_pid
	if ($switch_state{$prev_pid}{time}) {
	    my $state = $switch_state{$prev_pid}{state};
	    my $prev = $switch_state{$prev_pid}{time};
	    my $comm = $switch_state{$prev_pid}{comm};
	    sched_stat("R", $prev_pid, $comm, $time, $time - $prev);
	}
    }
    if ($next_pid != 0) {
	# Sleeping next_pid switched from prev_pid
	if ($switch_state{$next_pid}{time}) {
	    my $state = $switch_state{$next_pid}{state};
	    my $prev = $switch_state{$next_pid}{time};
	    my $comm = $switch_state{$next_pid}{comm};
	    sched_stat($state, $next_pid, $comm, $time, $time - $prev);
	}
    }
}

sub sched_stat_cpu($$$$$)
{
    my $cpu = shift;
    my $prev_type = shift;
    my $prev_pid = shift;
    my $next_pid = shift;
    my $time = shift;

    # per cpu stat
    my $cid = to_cid($cpu);
    if ($cpu_state{$cid}{time}) {
	my $state = $cpu_state{$cid}{state};
	my $prev = $cpu_state{$cid}{time};
	sched_stat_process($state, $cid, $cid, $time, $time - $prev);
    }

    # If switched to swapper, remember state. Otherwise running
    $cpu_state{$cid}{state} = ($next_pid == 0) ? $prev_type : "R";
    $cpu_state{$cid}{time} = $time;
}

#sub sched::sched_pi_setprio
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$comm, $pid, $oldprio, $newprio) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

sub sched::sched_stat_runtime
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$comm, $pid, $runtime, $vruntime) = @_;

    update_cur_time($common_secs, $common_nsecs);

    if ($opt_use_sched_switch == 0 and $runtime) {
	my $time = to_tv64($common_secs, $common_nsecs);
	sched_stat("R", $pid, $comm, $time, $runtime);
    }
}

sub sched::sched_stat_blocked
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$comm, $pid, $delay) = @_;

    update_cur_time($common_secs, $common_nsecs);

    if ($opt_use_sched_switch == 0 and $delay) {
	my $time = to_tv64($common_secs, $common_nsecs);
	sched_stat("D", $pid, $comm, $time, $delay);
    }
}

#sub sched::sched_stat_iowait
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$comm, $pid, $delay) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

sub sched::sched_stat_sleep
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$comm, $pid, $delay) = @_;

    update_cur_time($common_secs, $common_nsecs);

    if ($opt_use_sched_switch == 0 and $delay) {
	my $time = to_tv64($common_secs, $common_nsecs);
	sched_stat("S", $pid, $comm, $time, $delay);
    }
}

sub sched::sched_stat_wait
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$comm, $pid, $delay) = @_;

    update_cur_time($common_secs, $common_nsecs);

    if ($opt_use_sched_switch == 0 and $delay) {
	my $time = to_tv64($common_secs, $common_nsecs);
	sched_stat("W", $pid, $comm, $time, $delay);
    }
}

sub sched::sched_process_exec
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$filename, $pid, $old_pid) = @_;

    update_cur_time($common_secs, $common_nsecs);

    # Update comm
    if ($switch_state{$old_pid}{comm}) {
	$switch_state{$old_pid}{comm} = $common_comm;
    }
}

#sub sched::sched_process_fork
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$parent_comm, $parent_pid, $child_comm, $child_pid) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub sched::sched_process_wait
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$comm, $pid, $prio) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub sched::sched_wait_task
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$comm, $pid, $prio) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub sched::sched_process_exit
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$comm, $pid, $prio) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub sched::sched_process_free
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$comm, $pid, $prio) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub sched::sched_migrate_task
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$comm, $pid, $prio, $orig_cpu, $dest_cpu) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

sub remember_switch($$$$)
{
    my $type = shift;
    my $pid = shift;
    my $comm = shift;
    my $time = shift;

    # Ignore swapper
    if ($pid != 0) {
	$switch_state{$pid}{state} = $type;
	$switch_state{$pid}{comm} = $comm;
	$switch_state{$pid}{time} = $time;
    }
}

sub count_switch($$$$)
{
    my $type = shift;
    my $cpu = shift;
    my $pid = shift;
    my $time = shift;

    # If switching from swapper, ignore
    if ($pid != 0) {
	if (is_interesting_pid($pid)) {
	    num_add($sched_s{$pid}{$type}, 1);
	}

	my $cid = to_cid($cpu);
	num_add($sched_s{$cid}{$type}, 1);

	# Check workqueue work
	foreach my $wid (wids_in_range($pid, $time, 1)) {
	    num_add($sched_s{$wid}{$type}, 1);
	}
    }
}

sub sched::sched_switch
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$prev_comm, $prev_pid, $prev_prio, $prev_state,
	$next_comm, $next_pid, $next_prio) = @_;

    update_cur_time($common_secs, $common_nsecs);

    # Remember state of previous task
    my $time = to_tv64($common_secs, $common_nsecs);
    my $prev_type = "R";
    my $voluntary;
    if ($prev_state & TASK_INTERRUPTIBLE) {
	$prev_type = "S";
	$voluntary = "voluntary-S";
    } elsif ($prev_state & TASK_UNINTERRUPTIBLE) {
	$prev_type = "D";
	$voluntary = "voluntary-D";
    } elsif ($prev_state == TASK_RUNNING) {
	$prev_type = "R";
	$voluntary = "involuntary";
    }
    if ($opt_use_sched_switch) {
	# Per pid stat if need
	sched_stat_on_switch($prev_type, $prev_pid, $next_pid, $time);
    }
    if ($voluntary) {
	remember_switch($prev_type, $prev_pid, $prev_comm, $time);
	count_switch($voluntary, $common_cpu, $prev_pid, $time);
    } else {
	delete($switch_state{$prev_pid});
    }

    # Remember state of next task
    remember_switch("R", $next_pid, $next_comm, $time);

    # Per cpu stat
    sched_stat_cpu($common_cpu, $prev_type, $prev_pid, $next_pid, $time);
}

#sub sched::sched_wakeup_new
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$comm, $pid, $prio, $success,
#	$target_cpu) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub sched::sched_wakeup
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$comm, $pid, $prio, $success,
#	$target_cpu) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub sched::sched_kthread_stop_ret
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$ret) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

#sub sched::sched_kthread_stop
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$comm, $pid) = @_;
#
#    update_cur_time($common_secs, $common_nsecs);
#}

sub sched_main
{
    # Get $xstart and $xend
    $perf_xstart = $ENV{FSPERF_XSTART};
    $perf_xend = $ENV{FSPERF_XEND};

    # Add stat from last switch to now
    foreach my $cid (keys(%cpu_state)) {
	my $cpu = is_cpu($cid);
	my $state = $cpu_state{$cid}{state};
	sched_stat_cpu($cpu, $state, 0, 0, $perf_end);
    }
    foreach my $pid (keys(%switch_state)) {
	my $comm = $switch_state{$pid}{comm};
	my $type = $switch_state{$pid}{state};
	my $time = $switch_state{$pid}{time};

	my $elapse = $perf_end - $time;
	sched_stat($type, $pid, $comm, $perf_end, $elapse);
    }
    process_partial_syscall();

    my @ids = sort { id_for_cmp($a) <=> id_for_cmp($b) } keys(%sched_s);

    # Close all to prevent to closed $log
    close_file_all();

    # Prepare for sched_summary.gp
    output_plot_sched_summary_pre(scalar(@ids));

    # Output Scheduler stat
    my $log = open_file("fsperf-sched.log", 0644, 1);

    print $log <<"EOF";
                        Schedule Time
EOF
    my $cpu_header = 1;
    my $pid_header = 1;
    my $wid_header = 1;
    foreach my $id (@ids) {
	my $comm = $sched_s{$id}{comm};
	my $start_time = $sched_s{$id}{start} || 0;
	my $end_time = $sched_s{$id}{end} || 0;
	my $run_time = $sched_s{$id}{R}{total} || 0;
	my $wait_time = $sched_s{$id}{W}{total} || 0;
	my $sleep_time = $sched_s{$id}{S}{total} || 0;
	my $block_time = $sched_s{$id}{D}{total} || 0;
	my $voluntary_s = $sched_s{$id}{"voluntary-S"} || 0;
	my $voluntary_d = $sched_s{$id}{"voluntary-D"} || 0;
	my $involuntary = $sched_s{$id}{"involuntary"} || 0;
	my $sched_type =
	    $opt_use_sched_switch ? "sched_switch" : "sched_stat_*";
	my $elapse;

	if (defined(is_cpu($id))) {
	    # Elapse time is whole time of task
	    $elapse = $end_time - $start_time;

	    if ($cpu_header) {
		print $log <<"EOF";
----------------------------------------------------------
                        CPU (Based on sched_switch)
EOF
		$cpu_header = 0;
	    }
	    print $log <<"EOF";
----------------------------------------------------------
   CPU                       Start(sec)         End(sec)      Elapse(sec)
EOF
	    printf $log "%6s %15s %16s %16s %16s\n",
		$id, " ",
		tv64_str($start_time), tv64_str($end_time), tv64_str($elapse);
	} elsif (!defined(is_wid($id))) {
	    # Elapse time is whole time of task
	    $elapse = $end_time - $start_time;

	    if ($pid_header) {
		print $log <<"EOF";
----------------------------------------------------------
                        Process (Based on $sched_type)
EOF
		$pid_header = 0;
	    }
	    print $log <<"EOF";
----------------------------------------------------------
   Pid            comm       Start(sec)         End(sec)      Elapse(sec)
EOF
	    printf $log "%6u %15s %16s %16s %16s\n",
		$id, $comm,
		tv64_str($start_time), tv64_str($end_time), tv64_str($elapse);
	} else {
	    # Elapse time is only spent on work
	    $elapse = $sched_s{$id}{elapse} || 0;

	    if ($wid_header) {
		print $log <<"EOF";
----------------------------------------------------------
                        Workqueue work (Based on $sched_type)
EOF
		$wid_header = 0;
	    }
	    print $log <<"EOF";
----------------------------------------------------------
 Work-id                 func       Start(sec)         End(sec)      Elapse(sec)
EOF
	    printf $log "%8s %20s %16s %16s %16s\n",
		$id, $comm,
		tv64_str($start_time), tv64_str($end_time), tv64_str($elapse);
	}

	print $log <<"EOF";

                     Running(sec)                  CPU wait(sec)
EOF
	printf $log "       %16s (%6.2f%%)     %16s (%6.2f%%)\n",
	    tv64_str($run_time), $elapse ? ($run_time * 100) / $elapse : 0,
	    tv64_str($wait_time), $elapse ? ($wait_time * 100) / $elapse : 0;

	print $log <<"EOF";
                       Sleep(sec)                     Block(sec)
EOF
	printf $log "       %16s (%6.2f%%)     %16s (%6.2f%%)\n",
	    tv64_str($sleep_time), $elapse ? ($sleep_time * 100) / $elapse : 0,
	    tv64_str($block_time), $elapse ? ($block_time * 100) / $elapse : 0;

	printf $log "\n";
	if (defined(is_wid($id))) {
	    my $nr_called = $sched_s{$id}{nr_called} || 0;
	    printf $log "       Number of called %9u\n", $nr_called;
	}
	printf $log
	    "       Voluntary(sleep) %9u     Voluntary(block) %9u\n",
	    $voluntary_s, $voluntary_d;
	printf $log
	    "       Involuntary      %9u\n",
	    $involuntary;

	# syscall stats
	if (exists($syscall_s{$id})) {
	    printf $log "\n";
	    print $log <<"EOF";
                           NR    Err      Max(sec)      Min(sec)      Avg(sec)
     Syscall                           Elapse(sec)
EOF

	    my ($total_nr, $total_elapse, $total_failed);
	    my $fmt = " %-20s: %6s %6s %13s %13s %13s\n";
	    foreach my $sys_id (sort { $a <=> $b } keys(%{$syscall_s{$id}})) {
		my $nr = $syscall_s{$id}{$sys_id}{nr};
		my $elapse = $syscall_s{$id}{$sys_id}{elapse};
		my $failed = $syscall_s{$id}{$sys_id}{failed} || 0;
		my $max = $syscall_s{$id}{$sys_id}{max};
		my $min = $syscall_s{$id}{$sys_id}{min};
		my $avg = $elapse / $nr;
		printf $log $fmt,
		    $sys_id, $nr, $failed, tv64_str($max),
		    tv64_str($min), tv64_str($avg);
		printf $log $fmt,
		    "", "", "", tv64_str($elapse), "", "";

		num_add($total_nr, $nr);
		num_add($total_elapse, $elapse);
		num_add($total_failed, $failed);
	    }
	    printf $log $fmt,
		"TOTAL", $total_nr, $total_failed, "", "", "";
	    printf $log $fmt,
		"", "", "", tv64_str($total_elapse), "", "";
	}

	# irq/softirq stats
	if ($irq_s{$id}) {
	    printf $log "\n";
	    print $log <<"EOF";
                                   NR           Max           Min           Avg
        IRQ/Softirq                          Elapse       Runtime         (sec)
EOF

	    my ($intr_nr, $intr_run, $intr_max, $intr_min);
	    my $fmt1 = " %-28s: %6u %13s %13s %13s\n";
	    my $fmt2 = " %-28s: %6s %13s %13s %13s\n";
	    foreach my $name (sort(keys(%{$irq_s{$id}}))) {
		my $nr = $irq_s{$id}{$name}{nr};
		my $elapse = $irq_s{$id}{$name}{total_elapse};
		my $runtime = $irq_s{$id}{$name}{total_run};
		my $max = $irq_s{$id}{$name}{max};
		my $min = $irq_s{$id}{$name}{min};
		my $avg = $runtime / $nr;
		printf $log $fmt1,
		    $name, $nr, tv64_str($max), tv64_str($min), tv64_str($avg);
		printf $log $fmt2,
		    "", "", tv64_str($elapse), tv64_str($runtime), "";

		num_add($intr_nr, $nr);
		num_add($intr_run, $runtime);
		num_max($intr_max, $max);
		num_min($intr_min, $min);
	    }
	    my $intr_avg = $intr_nr ? ($intr_run / $intr_nr) : 0;
	    printf $log $fmt1,
		"TOTAL", $intr_nr, tv64_str($intr_max), tv64_str($intr_min),
		tv64_str($intr_avg);
	    printf $log $fmt2,
		"", "", "", tv64_str($intr_run), "";
	}

	my $fh = open_id_datfile($id, "stat");
	foreach my $idx (to_sec($start_time)..to_sec($end_time)) {
	    print $fh $idx + 0.5;
	    foreach my $type (@sched_types) {
		my $time_str = tv64_str($sched_s{$id}{$type}{sec}{$idx} || 0);
		print $fh " $time_str";
	    }
	    print $fh "\n";
	}
	close_file($fh);
	# Make sure sched_*_time.dat is available for sched_summary.gp
	my $dummy = open_id_datfile($id, "time");
	close_file($dummy);

	# Create sched plot
	output_plot_sched($id, $comm);
    }

    close_file($log);

    # Create sched processes plot
    output_plot_sched_summary();

    # Create summary plot
#    foreach my $dev (split(/,/, $ENV{FSPERF_DEV})) {
#	output_plot_summary($dev, 1);
#    }
}

sub sched_only_main
{
    # Calc start and end time
    calc_start_end_time();
    $ENV{FSPERF_XSTART} = $perf_xstart;
    $ENV{FSPERF_XEND} = $perf_xend;

    sched_main();
}

##################################
#
# cpu events to help sched stats
#

sub is_cpu($)
{
    my $cid = shift;

    if ($cid =~ m!^cpu-(\d+)!) {
	return $1;
    }
    return undef;
}

sub to_cid($)
{
    my $val = shift;
    return "cpu-$val";
}

##################################
#
# workqueue events to help sched stats
#

my %wq_work_id;
my $last_work_id = 1;

sub is_wid($)
{
    my $wid = shift;

    if ($wid =~ m!^work-(\d+)!) {
	return $1;
    }
    return undef;
}

sub id_for_cmp($)
{
    my $id = shift;
    my $base = 10000000000;

    my $cid_val = is_cpu($id);
    if (defined($cid_val)) {
	# $id is cpu_id
	return $cid_val;
    }

    my $wid_val = is_wid($id);
    if (defined($wid_val)) {
	# $id is work_id, use value bigger than any pid
	return ($base * 2) + $wid_val;
    }

    # $id is pid, use value bigger than any cid
    return $base + $id;
}

sub to_wid($)
{
    my $val = shift;
    return "work-$val";
}

sub work_to_wid($)
{
    my $work = shift;

    if (!exists($wq_work_id{$work})) {
	$wq_work_id{$work} = $last_work_id++;
    }

    return to_wid($wq_work_id{$work});
}

sub is_interesting_wid($)
{
    my $wid = shift;

    if (!scalar(@opt_target_wid) or grep { $_ eq $wid } @opt_target_wid) {
	return 1;
    }
    return 0;
}

# return wids if interesting and running in specified time range
sub wids_in_range($$$)
{
    my $pid = shift;
    my $time_end = shift;
    my $elapse = shift;
    my @wids;

    foreach my $wid (keys(%{$wq_state{$pid}})) {
	if (is_interesting_wid($wid)) {
	    my $time_start = $time_end - $elapse;
	    my $work_start = $wq_state{$pid}{$wid}{start};
	    # If {end} is undef, work is still running
	    my $work_end = $wq_state{$pid}{$wid}{end} || $time_end;

	    # There was no workqueue_execute_start event
	    if (!defined($wq_state{$pid}{$wid}{start})) {
		pr_warn("$wid is missing workqueue_execute_start event");
		next;
	    }

	    # Stat is in workqueue work?
	    if ($work_start < $time_end and $time_start < $work_end) {
		push(@wids, $wid);
	    }
	}
    }

    return @wids;
}

sub workqueue::workqueue_execute_start
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$work, $function) = @_;

    my $wid = work_to_wid($work);
    if (!is_interesting_wid($wid)) {
	return;
    }

    $wq_state{$common_pid}{$wid}{start} = to_tv64($common_secs, $common_nsecs);
    $wq_state{$common_pid}{$wid}{end} = undef;
    $wq_state{$common_pid}{$wid}{sym} = kallsyms_find_by_addr($function);

    num_add($sched_s{$wid}{nr_called}, 1);
}

sub workqueue::workqueue_execute_end
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$work) = @_;

    my $wid = work_to_wid($work);
    if (!is_interesting_wid($wid)) {
	return;
    }

    $wq_state{$common_pid}{$wid}{end} = to_tv64($common_secs, $common_nsecs);

    if ($wq_state{$common_pid}{$wid}{start}) {
	my $start = $wq_state{$common_pid}{$wid}{start};
	my $end = $wq_state{$common_pid}{$wid}{end};
	my $elapse = $end - $start;

	# Add time only spent on work
	if (not $sched_s{$wid}{comm}) {
	    $sched_s{$wid}{comm} = $wq_state{$common_pid}{$wid}{sym};
	}
	num_add($sched_s{$wid}{elapse}, $elapse);
    } else {
	# There was no workqueue_execute_start event
	$sched_s{$wid}{comm} = "[unknown]";
    }
}

#sub workqueue::workqueue_activate_work
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$work) = @_;
#}

#sub workqueue::workqueue_queue_work
#{
#    perf_args_normalize(\@_);
#    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
#	$common_pid, $common_comm, $common_callchain,
#	$work, $function, $workqueue, $req_cpu, $cpu) = @_;
#}

##################################
#
# syscall events to help sched stats
#

my %syscall_state;
my $sys_sigreturn_id = 15;
my $sys_ioctl_id = 16;

sub syscall_should_warn(@)
{
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$id, $dummy) = @_;

    # perf modify event state, so missing sys_enter/sys_exit is normal.
    # Not warn about it.
    if ($common_comm eq "perf" and $id == $sys_ioctl_id) {
	return 0;
    }
    return 1;
}

sub has_sys_enter($$)
{
    my $pid = shift;
    my $id = shift;

    if (!exists($syscall_state{$pid}) ||
	!exists($syscall_state{$pid}{enter_id})) {
	return -1;
    }

    my $enter_id = $syscall_state{$pid}{enter_id};
    if ($enter_id == $id) {
	return $enter_id;
    }
    # sys_exit for rt_sigreturn is called with $id == -1
    if ($enter_id == $sys_sigreturn_id && $id == -1) {
	return $enter_id;
    }
    return -1;
}

# Process corner case of sys_enter/sys_exit
sub process_partial_syscall
{
    foreach my $pid (keys(%syscall_state)) {
	# perf was stopped before sys_exit
	if (exists($syscall_state{$pid}{enter_id}) &&
	    exists($sched_s{$pid}) && exists($sched_s{$pid}{end})) {
	    my $comm = $sched_s{$pid}{comm};
	    my $start = $syscall_state{$pid}{enter_time};
	    my $end = $sched_s{$pid}{end};

	    sched_stat("sys", $pid, $comm, $end, $end - $start);
	}
	# perf was started after sys_enter
	if (exists($syscall_state{$pid}{exit_id}) &&
	    exists($sched_s{$pid}) && exists($sched_s{$pid}{start})) {
	    my $comm = $sched_s{$pid}{comm};
	    my $start = $sched_s{$pid}{start};
	    my $end = $syscall_state{$pid}{exit_time};

	    sched_stat("sys", $pid, $comm, $end, $end - $start);
	}
    }
}

sub raw_syscalls::sys_enter
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$id, $args) = @_;

    update_cur_time($common_secs, $common_nsecs);
    my $time = to_tv64($common_secs, $common_nsecs);

    if (exists($syscall_state{$common_pid}{enter_id})) {
	pr_warn("[" . tv64_str($time) . "] " .
		"sys_enter($id) event without sys_exit: ignored old")
	    if (syscall_should_warn(@_));
    }
    $syscall_state{$common_pid}{enter_id} = $id;
    $syscall_state{$common_pid}{enter_time} = $time;
}

sub raw_syscalls::sys_exit
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$id, $ret) = @_;

    update_cur_time($common_secs, $common_nsecs);
    my $time = to_tv64($common_secs, $common_nsecs);

    my $enter_id = has_sys_enter($common_pid, $id);
    if ($enter_id >= 0) {
	my $elapse = $time - $syscall_state{$common_pid}{enter_time};
	delete($syscall_state{$common_pid}{enter_id});
	delete($syscall_state{$common_pid}{enter_time});

	sched_stat("sys", $common_pid, $common_comm, $time, $elapse);

	num_add($syscall_s{$common_pid}{$enter_id}{nr}, 1);
	num_add($syscall_s{$common_pid}{$enter_id}{elapse}, $elapse);
	num_max($syscall_s{$common_pid}{$enter_id}{max}, $elapse);
	num_min($syscall_s{$common_pid}{$enter_id}{min}, $elapse);
	if ($ret < 0) {
	    num_add($syscall_s{$common_pid}{$enter_id}{failed}, 1);
	}
    } else {
	if (exists($syscall_state{$common_pid}) ||
	    exists($syscall_s{$common_pid})) {
	    pr_warn("[" . tv64_str($time) . "] " .
		    "sys_exit($id) event without sys_entry: ignored")
		if (syscall_should_warn(@_));
	} else {
	    # Remember initial sys_exit without sys_enter. (maybe,
	    # perf was started while process is running in syscall)
	    $syscall_state{$common_pid}{exit_id} = $id;
	    $syscall_state{$common_pid}{exit_time} = $time;
	}
    }
}

##################################
#
# irq/softirq events to help sched stats
#

my %cpu_irq_state;

sub sched_irq_stat($$$$)
{
    my $cid = shift;
    my $time = shift;
    my $pid = shift;
    my $comm = shift;

    my $data = pop(@{$cpu_irq_state{$cid}});

    my $start_time = $data->{start};
    my $irq_type = $data->{irq_type};
    my $irq_name = $data->{irq_name};

    my $elapse = $time - $start_time;
    my $run = $elapse;
    # Subtract time from interrupted irq runtime
    if ($irq_s{$cid}{$irq_name}{nested_run}) {
	num_sub($run, $irq_s{$cid}{$irq_name}{nested_run});
	$irq_s{$cid}{$irq_name}{nested_run} = 0;
    }
    # intr is nested?
    if (scalar(@{$cpu_irq_state{$cid}}) > 0) {
	# Remember latest irq runtime
	my $nest = $cpu_irq_state{$cid}[-1];
	$irq_s{$cid}{$nest->{irq_name}}{nested_run} = $elapse;
    }

    my @ids;
    if ($pid == 0) {
	# skip if pid == 0
	@ids = ($cid);
    } else {
	@ids = ($cid, $pid);
    }
    foreach my $id (@ids) {
	num_add($irq_s{$id}{$irq_name}{nr}, 1);
	num_add($irq_s{$id}{$irq_name}{total_elapse}, $elapse);
	num_add($irq_s{$id}{$irq_name}{total_run}, $run);
	num_max($irq_s{$id}{$irq_name}{max}, $run);
	num_min($irq_s{$id}{$irq_name}{min}, $run);
    }
    # per cpu stats
    sched_stat_process($irq_type, $cid, $cid, $time, $elapse);
    # per pid/workqueue stats
    sched_stat($irq_type, $pid, $comm, $time, $elapse) if ($pid != 0);
}

sub sched_irq_stat_entry($$$$)
{
    my $cpu = shift;
    my $time = shift;
    my $irq_type = shift;
    my $irq_name = shift;

    my $cid = to_cid($cpu);
    my %data = (start => $time, irq_type => $irq_type, irq_name => $irq_name);
    push(@{$cpu_irq_state{$cid}}, \%data);
}

sub sched_irq_stat_exit($$$$)
{
    my $cpu = shift;
    my $time = shift;
    my $pid = shift;
    my $comm = shift;

    my $cid = to_cid($cpu);
    if (scalar(@{$cpu_irq_state{$cid}}) > 0) {
	sched_irq_stat($cid, $time, $pid, $comm);
    } else {
	pr_warn("intr_exit event without intr_entry");
    }
}

sub irq::irq_handler_entry
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$irq, $name) = @_;

    update_cur_time($common_secs, $common_nsecs);
    my $time = to_tv64($common_secs, $common_nsecs);

    sched_irq_stat_entry($common_cpu, $time, "irq", "irq$irq-$name");
}

sub irq::irq_handler_exit
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$irq, $ret) = @_;

    update_cur_time($common_secs, $common_nsecs);
    my $time = to_tv64($common_secs, $common_nsecs);

    sched_irq_stat_exit($common_cpu, $time, $common_pid, $common_comm);
}

sub to_softirq_name($)
{
    my $nr = shift;
    # Convert vec to name by perf's symbolic fields
    my $str = symbol_str("irq::softirq_entry", "vec", $nr);
    if ($str) {
	return $str;
    }
    return "$nr";
}

sub irq::softirq_entry
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$vec_nr) = @_;

    update_cur_time($common_secs, $common_nsecs);
    my $time = to_tv64($common_secs, $common_nsecs);

    my $name = to_softirq_name($vec_nr);
    sched_irq_stat_entry($common_cpu, $time, "sirq", "softirq-$name");
}

sub irq::softirq_exit
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$vec_nr) = @_;

    update_cur_time($common_secs, $common_nsecs);
    my $time = to_tv64($common_secs, $common_nsecs);

    sched_irq_stat_exit($common_cpu, $time, $common_pid, $common_comm);
}

sub irq_vector_entry
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$vector) = @_;

    update_cur_time($common_secs, $common_nsecs);
    my $time = to_tv64($common_secs, $common_nsecs);

    # remove irq_vectors:: prefix, and _entry postfix
    my $name = substr($event_name, length("irq_vectors::"), -length("_entry"));
    sched_irq_stat_entry($common_cpu, $time, "irq", "vec$vector-$name");
}

sub irq_vector_exit
{
    perf_args_normalize(\@_);
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain,
	$vector) = @_;

    update_cur_time($common_secs, $common_nsecs);
    my $time = to_tv64($common_secs, $common_nsecs);

    sched_irq_stat_exit($common_cpu, $time, $common_pid, $common_comm);
}

sub irq_vectors::call_function_entry
{
    irq_vector_entry(@_);
}
sub irq_vectors::call_function_exit
{
    irq_vector_exit(@_);
}
sub irq_vectors::call_function_single_entry
{
    irq_vector_entry(@_);
}
sub irq_vectors::call_function_single_exit
{
    irq_vector_exit(@_);
}
sub irq_vectors::deferred_error_apic_entry
{
    irq_vector_entry(@_);
}
sub irq_vectors::deferred_error_apic_exit
{
    irq_vector_exit(@_);
}
sub irq_vectors::error_apic_entry
{
    irq_vector_entry(@_);
}
sub irq_vectors::error_apic_exit
{
    irq_vector_exit(@_);
}
sub irq_vectors::irq_work_entry
{
    irq_vector_entry(@_);
}
sub irq_vectors::irq_work_exit
{
    irq_vector_exit(@_);
}
sub irq_vectors::local_timer_entry
{
    irq_vector_entry(@_);
}
sub irq_vectors::local_timer_exit
{
    irq_vector_exit(@_);
}
sub irq_vectors::reschedule_entry
{
    irq_vector_entry(@_);
}
sub irq_vectors::reschedule_exit
{
    irq_vector_exit(@_);
}
sub irq_vectors::spurious_apic_entry
{
    irq_vector_entry(@_);
}
sub irq_vectors::spurious_apic_exit
{
    irq_vector_exit(@_);
}
sub irq_vectors::thermal_apic_entry
{
    irq_vector_entry(@_);
}
sub irq_vectors::thermal_apic_exit
{
    irq_vector_exit(@_);
}
sub irq_vectors::threshold_apic_entry
{
    irq_vector_entry(@_);
}
sub irq_vectors::threshold_apic_exit
{
    irq_vector_exit(@_);
}
sub irq_vectors::x86_platform_ipi_entry
{
    irq_vector_entry(@_);
}
sub irq_vectors::x86_platform_ipi_exit
{
    irq_vector_exit(@_);
}

sub trace_unhandled
{
    my ($event_name, $context, $common_cpu, $common_secs, $common_nsecs,
	$common_pid, $common_comm, $common_callchain) = @_;

    update_cur_time($common_secs, $common_nsecs);
}

sub print_backtrace
{
    my $callchain = shift;
    for my $node (@{$callchain}) {
	if (exists($node->{sym})) {
	    printf "\t[\%x] \%s\n", $node->{ip}, $node->{sym}{name};
	} else {
	    printf "\t[\%x]\n", $node->{ip};
	}
    }
}

sub print_header
{
    my ($event_name, $cpu, $secs, $nsecs, $pid, $comm) = @_;

    printf("%-20s %5u %05u.%09u %8u %-20s ",
	   $event_name, $cpu, $secs, $nsecs, $pid, $comm);
}

# Packed byte string args of process_event():
#
# $event:	union perf_event	util/event.h
# $attr:	struct perf_event_attr	linux/perf_event.h
# $sample:	struct perf_sample	util/event.h
# $raw_data:	perf_sample->raw_data	util/event.h
#sub process_event
#{
#    my ($event, $attr, $sample, $raw_data) = @_;
#
#    my @event	= unpack("LSS", $event);
#    my @attr	= unpack("LLQQQQQLLQQ", $attr);
#    my @sample	= unpack("QLLQQQQQLL", $sample);
#    my @raw_data	= unpack("C*", $raw_data);
#
#    use Data::Dumper;
#    print Dumper \@event, \@attr, \@sample, \@raw_data;
#}

sub graph_only_main
{
    if (not -d $output_dir) {
	die "Coundn't find $output_dir directory: $!";
    }

    # Make target kdevs
    my %devs;
    foreach my $name (glob("$output_dir/*,*")) {
	if ($name =~ m!(\d+,\d+)_.*!) {
	    my $dev = "$1";
	    my ($major, $minor) = split(/,/, $dev);
	    my $kdev = kmakedev($major, $minor);
	    $devs{$kdev} = 1;
	}
    }

    $ENV{FSPERF_DEV} = join(",", keys(%devs));
}

my %mode_table = (
		  FSPERF_MODE_REPORT	=> {
					    func => undef,
					    next => "FSPERF_MODE_BLOCK",
					    data => $perf_block_data,
					   },
		  FSPERF_MODE_BLOCK	=> {
					    func => \&block_main,
					    next => "FSPERF_MODE_SCHED",
					    data => $perf_sched_data,
					   },
		  FSPERF_MODE_SCHED	=> {
					    func => \&sched_main,
					    next => "FSPERF_MODE_GRAPH",
					    data => undef,
					   },
		  FSPERF_MODE_GRAPH	=> {
					    func => \&graph_main,
					    next => undef,
					    data => undef,
					   },

		  # For $opt_no_block
		  FSPERF_MODE_REPORT2	=> {
					    func => undef,
					    next => "FSPERF_MODE_SCHED2",
					    data => $perf_sched_data,
					   },
		  FSPERF_MODE_SCHED2	=> {
					    func => \&sched_only_main,
					    next => undef,
					    data => undef,
					   },

		  # For $opt_no_sched
		  FSPERF_MODE_REPORT3	=> {
					    func => undef,
					    next => "FSPERF_MODE_BLOCK3",
					    data => $perf_block_data,
					   },
		  FSPERF_MODE_BLOCK3	=> {
					    func => \&block_main,
					    next => "FSPERF_MODE_GRAPH",
					    data => undef,
					   },

		  # For $opt_graph_only
		  FSPERF_MODE_REPORT4	=> {
					    func => \&graph_only_main,
					    next => "FSPERF_MODE_GRAPH",
					    data => undef,
					   },
		 );

# Called from perf before starting events
sub trace_begin
{
    # Setup parameters from environment
    @opt_target_pid = split(/,/, $ENV{FSPERF_TARGET_PID});
    @opt_target_wid = split(/,/, $ENV{FSPERF_TARGET_WID});
    $opt_no_error = $ENV{FSPERF_NO_ERROR};
    $opt_seek_threshold = $ENV{FSPERF_SEEK_THRESHOLD};
    $opt_seek_relative = $ENV{FSPERF_SEEK_RELATIVE};
    $opt_kallsyms = $ENV{FSPERF_KALLSYMS};
    $opt_use_sched_switch = $ENV{FSPERF_USE_SCHED_SWITCH};
    $opt_debug_event = $ENV{FSPERF_DEBUG_EVENT};

    read_devmap();
}

# Called from perf after all events was done
sub trace_end
{
    my $mode = $ENV{FSPERF_MODE};
    my $func = $mode_table{$mode}->{func};

    # Call post process function
    $func->();

    # Run next command
    run_next_cmd();
}

##################################
#
# State machine
#

sub run_perf_script($)
{
    my $data = shift;
    my $script = $ENV{FSPERF_SCRIPT};

    my $cmd = "perf script --hide-call-graph -s $script -i $data";

    safe_system($cmd);
}

sub run_next_cmd
{
    my $mode = $ENV{FSPERF_MODE};

    if ($mode_table{$mode}->{next}) {
	$ENV{FSPERF_MODE} = $mode_table{$mode}->{next};

	if ($mode_table{$mode}->{data}) {
	    run_perf_script($mode_table{$mode}->{data});
	} else {
	    trace_begin();
	    trace_end();
	}
    }
}

sub run_cmd
{
    my $mode = $ENV{FSPERF_MODE};

    # Call function
    if ($mode_table{$mode}->{func}) {
	$mode_table{$mode}->{func}->();
    }

    run_next_cmd();
}

##################################
#
# Commands
#

sub record_help
{
    print <<"EOF";
Usage: $0 record <options> -- <cmdline>...

Options:
 <cmdline>...		 Any command you can specify in a shell.
 -d, --device=DEV        Record events only for DEV.
                         Accepts multiple times (e.g. -d /dev/sda -d /dev/sdb)
 --no-block              Don't run block events
 --no-sched              Don't run sched events
 -g, --call-graph=MODE   pass --call-graph option to sched events
 --no-syscall            Disable to collect syscall events
 --no-irq                Disable to collect irq events
 -h, --help              This help.

EOF

    exit(1);
}

sub get_kdev($)
{
    my $path = shift;

    # FIXME: there is better portable way?
    open(my $lsblk, "-|", "lsblk -l -n -o MAJ:MIN $path")
	or die "Couldn't run lsblk: $!";
    my $line = <$lsblk>;
    close($lsblk);

    if ($line and $line =~ m!\d+:\d+!) {
	my ($major, $minor) = split(/:/, $line);
	return kmakedev($major, $minor);
    }

    die "Invalid block device: $path\n";
}

# Make map for "partition => whole disk"
sub make_devmap
{
    # Find whole device from partition
    open(my $lsblk, "-|", "lsblk -l -n -o MAJ:MIN,TYPE")
	or die "Couldn't run lsblk: $!";

    my $whole_disk;
    while (<$lsblk>) {
	my ($majmin, $type) = split(" ");
	my ($major, $minor) = split(":", $majmin);

	if ($type eq "disk" or $type eq "rom") {
	    $whole_disk = kmakedev($major, $minor);
	    $devmap{$whole_disk} = $whole_disk;
	} elsif ($whole_disk and $type eq "part") {
	    my $dev = kmakedev($major, $minor);
	    $devmap{$dev} = $whole_disk;
	} else {
	    die "Unknown state: " . kdevname($whole_disk) . " $type\n";
	}
    }

    close($lsblk);

    # create mapping of partition => whole
    open(my $fh, ">", "$perf_block_map")
	or die "Couldn't create $perf_block_map: $!";
    foreach my $dev (keys(%devmap)) {
	print $fh "$dev $devmap{$dev}\n";
    }
    close($fh);
}

sub read_devmap
{
    my $fh;

    # read mapping of partition => whole
    unless (open($fh, "<", "$perf_block_map")) {
	pr_warn("Couldn't open $perf_block_map");
	return;
    }
    while (<$fh>) {
	chomp;
	my ($dev, $whole) = split(/ /);
	$devmap{$dev} = $whole;
    }
    close($fh);
}

sub get_whole_dev($)
{
    my $dev = shift;

    if (!defined($devmap{$dev})) {
	my $devname = kdevname($dev);
	pr_warn("Unknown device: $dev");
	$devmap{$dev} = $dev;
    }

    return $devmap{$dev};
}

use constant FILTER_DEV => "dev";
use constant FILTER_NAME => "name";
use constant FILTER_BDI => "bdi";

sub make_filter_str($@)
{
    my $type = shift;
    my @kdevs = @_;

    my $or_sep = "";
    my $filter;
    foreach my $kdev (@kdevs) {
	$filter .= $or_sep;

	if ($type eq FILTER_DEV) {
	    $filter .= "dev==$kdev";
	} elsif ($type eq FILTER_NAME) {
	    $filter .= sprintf("name==%u:%u", kmajor($kdev), kminor($kdev));
	} elsif ($type eq FILTER_BDI) {
	    $filter .= sprintf("bdi==%u:%u", kmajor($kdev), kminor($kdev));
	} else {
	    die "Unknown filter type: $type";
	}

	$or_sep = "||";
    }

    return $filter;
}

sub get_device_kdev(@)
{
    my @devices = @_;
    my @kdevs;

    make_devmap();

    my $or_sep = "";
    my $filter;
    foreach my $path (@devices) {
	my $dev = get_kdev($path);

	if (!defined($devmap{$dev})) {
	    die "Coundn't find whole device for " . kdevname($dev);
	}
	my $whole_disk = get_whole_dev($dev);

	push(@kdevs, $dev);
	push(@kdevs, $whole_disk);
    }

    return @kdevs;
}

sub copy_kallsyms
{
    # kallsyms is not necessary to work, ignore error
    copy("/proc/kallsyms", $perf_sched_kallsyms);
}

# Check "sched_schedstats" for scheduler stats.
# If sched_schedstats==0, sched_stat_* event can't work.
sub check_schedstats
{
    my $schedstats = "/proc/sys/kernel/sched_schedstats";
    if (-r $schedstats) {
	if (open(my $fh, "<", $schedstats)) {
	    my $val = <$fh>;
	    chomp $val;
	    close($fh);
	    if ($val eq "0") {
		pr_warn("For details of scheduler, echo 1 > $schedstats");
	    }
	}
    }
}

sub run_record
{
    my @kdevs = @_;

    my %block_events = (
#			"block:block_rq_remap" => FILTER_DEV,
#			"block:block_bio_remap" => FILTER_DEV,
			"block:block_split" => FILTER_DEV,
#			"block:block_unplug" => undef,
#			"block:block_plug" => undef,
#			"block:block_sleeprq" => FILTER_DEV,
#			"block:block_getrq" => FILTER_DEV,
			"block:block_bio_queue" => FILTER_DEV,
			"block:block_bio_frontmerge" => FILTER_DEV,
			"block:block_bio_backmerge" => FILTER_DEV,
			"block:block_bio_complete" => FILTER_DEV,
			"block:block_rq_issue" => FILTER_DEV,
#			"block:block_rq_insert" => FILTER_DEV,
			"block:block_rq_complete" => FILTER_DEV,
			"block:block_rq_requeue" => FILTER_DEV,
#			"block:block_rq_abort" => FILTER_DEV,

#			"writeback:writeback_start" => FILTER_NAME,
#			"writeback:writeback_written" => FILTER_NAME,
#			"writeback:writeback_pages_written" => undef,
#			"writeback:global_dirty_state" => undef,
#			"writeback:bdi_dirty_ratelimit" => FILTER_BDI,
#			"writeback:balance_dirty_pages" => FILTER_BDI,
		       );
    my @sched_events = (
			# sched events
#			"sched:sched_pi_setprio",
			"sched:sched_stat_runtime",
			"sched:sched_stat_blocked",
#			"sched:sched_stat_iowait",
			"sched:sched_stat_sleep",
			"sched:sched_stat_wait",
			"sched:sched_process_exec",
			"sched:sched_process_fork",
#			"sched:sched_process_wait",
#			"sched:sched_wait_task",
			"sched:sched_process_exit",
#			"sched:sched_process_free",
			"sched:sched_switch",
#			"sched:sched_migrate_task",
#			"sched:sched_wakeup_new",
#			"sched:sched_wakeup",
#			"sched:sched_kthread_stop_ret",
#			"sched:sched_kthread_stop",

			# workqueue events
			"workqueue:workqueue_execute_end",
			"workqueue:workqueue_execute_start",
#			"workqueue:workqueue_activate_work",
#			"workqueue:workqueue_queue_work",
		       );

    my @syscall_events = (
			  # syscalls
			  "raw_syscalls:sys_enter",
			  "raw_syscalls:sys_exit",
			 );

    my @irq_events = (
		      # irqs
		      "irq:irq_handler_entry",
		      "irq:irq_handler_exit",
		      "irq:softirq_entry",
		      "irq:softirq_exit",
		     );

    my @x86_irq_events = (
			  "irq_vectors:call_function_*",
			  "irq_vectors:call_function_single_*",
			  "irq_vectors:deferred_error_apic_*",
			  "irq_vectors:error_apic_*",
			  # "irq_work_exit" can't use for perf sample,
			  # becase perf itself uses irq_work.
			  #"irq_vectors:irq_work_*",
			  "irq_vectors:local_timer_*",
			  "irq_vectors:reschedule_*",
			  "irq_vectors:spurious_apic_*",
			  "irq_vectors:thermal_apic_*",
			  "irq_vectors:threshold_apic_*",
			  "irq_vectors:x86_platform_ipi_*",
			 );

    my %arch_events = (
		       "i386" => \@x86_irq_events,
		       "x86_64" => \@x86_irq_events,
		      );

    #
    # Run perf record with following like options. Current perf
    # (3.8.x) can't specify "-g" option for each event.
    #
    # So, this separates perf by 2 commands. One is block events without "-g".
    # One is sched events with "-g".
    #
    #     perf record -a -c1 -o perf-block.data \
    #         -e 'block:*' --filter dev==0x800010 -- \
    #         perf record -a -c1 -g -o perf-sched.data -e 'sched:*' -- \
    #         dd if=/mnt/file of=/dev/null bs=4K

    my @cmd;
    # Make block events cmdline
    if (not $opt_no_block) {
	@cmd = ("perf", "record", "-a", "-c1", "-o", $perf_block_data);
	foreach my $event (sort(keys(%block_events))) {
	    push(@cmd, "-e", $event);
	    # Add filter
	    if ($block_events{$event} && @kdevs) {
		my $filter_type = $block_events{$event};
		my $filter = make_filter_str($filter_type, @kdevs);
		push(@cmd, "--filter", $filter);
	    }
	}
	push(@cmd, "--");
    }

    if (not $opt_no_sched) {
	# Check sched_schedstats
	check_schedstats
	# Copy kallsyms for using later
	copy_kallsyms();

	# Make sched events cmdline
	push(@cmd, "perf", "record", "-a", "-c1", "-o", $perf_sched_data);
	if (defined($opt_call_graph)) {
	    if (length($opt_call_graph) == 0) {
		push(@cmd, "-g");
	    } else {
		push(@cmd, "--call-graph", "$opt_call_graph");
	    }
	}
	if (not $opt_no_syscall) {
	    push(@sched_events, @syscall_events);
	}
	if (not $opt_no_irq) {
	    push(@sched_events, @irq_events);
	    # Check uname
	    my $arch = (POSIX::uname)[4];
	    if ($arch_events{$arch}) {
		push(@sched_events, @{$arch_events{$arch}});
	    }
	}
	foreach my $event (@sched_events) {
	    push(@cmd, "-e", $event);
	}
	push(@cmd, "--");
    }

    # Add user cmdline
    push(@cmd, @ARGV);

    safe_system(@cmd);
}

sub cmd_record
{
    my (@opt_device, $help);

    my $ret = GetOptions(
			 "device=s"		=> \@opt_device,
			 "no-block"		=> \$opt_no_block,
			 "no-sched"		=> \$opt_no_sched,
			 "g|call-graph:s"	=> \$opt_call_graph,
			 "no-syscall"		=> \$opt_no_syscall,
			 "no-irq"		=> \$opt_no_irq,
			 "help"			=> \$help,
			);

    record_help() if ($help || !$ret);

    my @kdevs = get_device_kdev(@opt_device);
    run_record(@kdevs);
}

sub report_help
{
    print <<"EOF";
Usage: $0 report <options>

Options:
 -p, --pid=PID                Output Schedule time for PIDs.
                              Accepts multiple times (e.g. -p 1 -p 2 -p 3)
 -w, --work-id=ID             Output Schedule time for Work-id of ID.
                              Accepts multiple times (e.g. -w 1 -w 2 -w 3)
 -s, --seek-threshold=VAL     Seek threshold. If seek distance is smaller than
                              VAL, this seek is ignored.
 -r, --relative-seek          Calculate seek relative distance. I.e. if next
                              access is before last access, use start of last
                              access. Otherwise, use end of last access.
 -k, --kallsyms=PATH          Read kallsyms from specified path.
                              (default $perf_sched_kallsyms)
 --use-sched_switch           Use sched_switch instead of sched_stat_*
                              (sched_stat_* requires CONFIG_SCHEDSTATS.
                               Without SCHEDSTATS, no way to see details.)
 --no-error                   Don't exit even if sanity check found error.
 --no-block                   Don't run block events
 --no-sched                   Don't run sched events
 --graph-only                 Run re-plot graph only
 --debug-event                Logging all perf event to log
 -h, --help                   This help.

EOF

    exit(1);
}

sub cmd_report
{
    my (@opt_pid, @opt_work_id, $help);

    my $ret = GetOptions(
			 "pid=i"		=> \@opt_pid,
			 "work-id=i"		=> \@opt_work_id,
			 "seek-threshold=i"	=> \$opt_seek_threshold,
			 "relative-seek"	=> \$opt_seek_relative,
			 "kallsyms=s"		=> \$opt_kallsyms,
			 "use-sched_switch"	=> \$opt_use_sched_switch,
			 "no-error"		=> \$opt_no_error,
			 "no-block"		=> \$opt_no_block,
			 "no-sched"		=> \$opt_no_sched,
			 "graph-only"		=> \$opt_graph_only,
			 "debug-event"		=> \$opt_debug_event,
			 "help"			=> \$help,
			);

    report_help() if ($help || !$ret);

    # convert values to work-id
    @opt_work_id = map { to_wid($_) } @opt_work_id;

    # Pass parameters as environment variables
    if ($opt_graph_only) {
	$ENV{FSPERF_MODE} = "FSPERF_MODE_REPORT4";
    } elsif ($opt_no_sched) {
	$ENV{FSPERF_MODE} = "FSPERF_MODE_REPORT3";
    } elsif ($opt_no_block) {
	$ENV{FSPERF_MODE} = "FSPERF_MODE_REPORT2";
    } else {
	$ENV{FSPERF_MODE} = "FSPERF_MODE_REPORT";
    }
    $ENV{FSPERF_SCRIPT} = $0;
    $ENV{FSPERF_TARGET_PID} = join(',', @opt_pid);
    $ENV{FSPERF_TARGET_WID} = join(',', @opt_work_id);
    $ENV{FSPERF_NO_ERROR} = $opt_no_error;
    $ENV{FSPERF_SEEK_THRESHOLD} = $opt_seek_threshold;
    $ENV{FSPERF_SEEK_RELATIVE} = $opt_seek_relative;
    $ENV{FSPERF_KALLSYMS} = $opt_kallsyms;
    $ENV{FSPERF_USE_SCHED_SWITCH} = $opt_use_sched_switch;
    $ENV{FSPERF_DEBUG_EVENT} = $opt_debug_event;

    run_cmd();
}

sub cmd_kallsyms
{
    $opt_print_kallsyms = 1;
    kallsyms_load_all();
    exit(0);
}

sub cmd_help
{
    print <<"EOF";
Usage: $0 [record|report|help] <options> -- <cmdline>

    record           Records performance data
    report           Make report from result of recorded data
    kallsyms         Print kallsyms embedded in $perf_sched_data
    debug_run        Run script outputted by "report --debug-event"
    help             This help

EOF

    exit(1);
}

# For performance test, collects all events. Then event handlers with
# collected data without perf.
#
#    $ ./fsperf.pl report --debug-event
#    $ perl -d:NYTProf ./fsperf.pl debug_run
#
my $debug_fh;
sub debug_event(@)
{
    my @args = @_;
    my $str;

    if (not $debug_fh) {
	open($debug_fh, ">>", "$debug_event_fname")
	    or die "open($debug_event_fname): $!";
    }

    # output callchain
    my $callchain = $args[7];
    $str = join(",", map {
	my $s = "{ip => $_->{ip},";
	if (exists($_->{sym})) {
	    $s .= "sym => {start => $_->{sym}{start},end => $_->{sym}{end}," .
		"binding => $_->{sym}{binding},name => \"$_->{sym}{name}\",},";
	}
	if (exists($_->{dso})) {
	    $s .= "dso => \"$_->{dso}\",";
	}
	$s .= "}";
    } @{$callchain});
    print $debug_fh "\@callchain = ($str);\n";

    # output arguments
    my $i = 0;
    $str = join(",", map {
	my $v = "\"$_\"";
	if ($i == 7) { $v = '\@callchain'; };
	$i++;
	$v;
    } @args);
    print $debug_fh "\@args = ($str);\n";

    # output event call
    my $event = $args[0];
    print $debug_fh "$event(\@args);\n";
}

sub cmd_debug_run
{
    require $debug_event_fname;
    exit(0);
}

my %cmd_func = (
		"record"	=> \&cmd_record,
		"report"	=> \&cmd_report,
		"kallsyms"	=> \&cmd_kallsyms,
		"debug_run"	=> \&cmd_debug_run,
		"help"		=> \&cmd_help,
	       );

# Called from perf script?
unless ($ENV{'PERF_EXEC_PATH'}) {
    my $cmd = shift(@ARGV);
    if ($cmd and $cmd_func{$cmd}) {
	$cmd_func{$cmd}(@ARGV);
    } else {
	cmd_help(@ARGV);
    }
}
