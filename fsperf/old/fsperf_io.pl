#!/usr/bin/perl
#
# fsperf.sh -d /dev/foo -o output/foo
# blkparse -i foo -D output | fsperf_io.pl
#

use strict;
use warnings;
use Getopt::Long;
use bignum;

#	case 'R':	/* Requeue */
#	case 'C': 	/* Complete */
#	case 'D': 	/* Issue */
#	case 'I': 	/* Insert */
#	case 'Q': 	/* Queue */
#	case 'B':	/* Bounce */
#	case 'M':	/* Back merge */
#	case 'F':	/* Front merge */
#	case 'G':	/* Get request */
#	case 'S':	/* Sleep request */
#	case 'P':	/* Plug */
#	case 'U':	/* Unplug IO */
#	case 'T': 	/* Unplug timer */
#	case 'A': 	/* remap */
#	case 'X': 	/* Split */
#	case 'm':	/* Message */

my %action_longname = (
		       Q => "Queue",
		       M => "Merge",
		       D => "Issue",
		       C => "Complete"
		      );
my %dir_longname = (
		    "r" => "Read",
		    "w" => "Write",
		    "c" => "Combined"
		   );

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

my %stats;
my $cur_time = 0;

# less than this seek distance will be ignore
my $seek_threshold = 0;
# 0: absolute distance, 1: relative distance
my $seek_relative = 0;

sub output_plot_pre($$$$$@)
{
    my $fh = shift;
    my $dev = shift;
    my $title = shift;
    my $xlabel = shift;
    my $ylabel = shift;
    my @configs = @_;

    # Calculate xrange
    my $xstart = int($stats{$dev}{"start_time"} - 1);
    $xstart = 0 if ($xstart < 10);
    my $xend = int($stats{$dev}{"end_time"} + 2);

    print $fh <<"EOF";
#!/usr/bin/gnuplot

#set term dumb
#set term svg
#set term png
#set output 'example.svg'
unset format
set title '${title}'
set xrange [${xstart}:${xend}]
set xlabel '${xlabel}'
set ylabel '${ylabel}'
set grid
EOF
    foreach my $conf (@configs) {
	print $fh "$conf\n";
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

sub fname_plot_bno($)
{
    my $dev = shift;
    return sprintf("%s_plot_bno.gp", $dev);
}

sub output_plot_bno($)
{
    my $dev = shift;
    my $fname = fname_plot_bno($dev);

    open(my $fh, "> $fname") or die "Couldn't create $fname: $!";
    chmod(0755, $fname);

    output_plot_pre($fh, $dev,
		    "Block Accesses", "Time (secs)", "Disk offset (byte)",
		    "set format y '%.1s %cB'",
		    "set pointsize 0.5",
		    "set bars 0.0",
		    "",
		    "to_byte(blk) = (blk * 512)");

    my $first = 1;
    foreach my $dir ("r", "w") {
	foreach my $type ("Queue", "Issue", "Complete") {
	    my $datafile = sprintf("%s_bno_%s_%s.dat", $dev, lc($type), $dir);

	    print $fh ", \\\n" if (not $first);
	    $first = 0;

	    # Set point at middle of range on a I/O request
#	    print $fh
#		"'$datafile' using 1:(to_byte(\$3 + \$2) / 2) title \"$dir_longname{$dir} $type\" with points pointtype 7";
	    # Set a line of range on a I/O request
	    print $fh
		"'$datafile' using 1:(to_byte(\$2)):(to_byte(\$2)):(to_byte(\$3)) title \"$dir_longname{$dir} $type\" with yerrorbars";
	}
    }
    print $fh "\n";

    output_plot_post($fh);

    close($fh);
}

sub fname_plot_mbps($)
{
    my $dev = shift;
    return sprintf("%s_plot_mbps.gp", $dev);
}

sub output_plot_mbps($)
{
    my $dev = shift;
    my $fname = fname_plot_mbps($dev);
    my $datafile = sprintf("%s_blkps_c.dat", $dev);

    open(my $fh, "> $fname") or die "Couldn't create $fname: $!";
    chmod(0755, $fname);

    output_plot_pre($fh, $dev,
		    "Thoughput", "Time (secs)", "MB/s",
		    "",
		    "to_mb(blk) = (blk * 512) / (1024 * 1024)");

    print $fh <<"EOF";
'$datafile' using 1:(to_mb(\$2)) title "I/O" with lines
EOF

    output_plot_post($fh);

    close($fh);
}

sub fname_plot_iops($)
{
    my $dev = shift;
    return sprintf("%s_plot_iops.gp", $dev);
}

sub output_plot_iops($)
{
    my $dev = shift;
    my $fname = fname_plot_iops($dev);
    my $datafile = sprintf("%s_iops_c.dat", $dev);

    open(my $fh, "> $fname") or die "Couldn't create $fname: $!";
    chmod(0755, $fname);

    output_plot_pre($fh, $dev,
		    "IO/s", "Time (secs)", "IO/s");

    print $fh <<"EOF";
'$datafile' using 1:2 title "I/O" with lines
EOF

    output_plot_post($fh);

    close($fh);
}

sub fname_plot_qdepth($)
{
    my $dev = shift;
    return sprintf("%s_plot_qdepth_c.gp", $dev);
}

sub output_plot_qdepth($)
{
    my $dev = shift;
    my $fname = fname_plot_qdepth($dev);
    my $datafile = sprintf("%s_qdepth_c.dat", $dev);

    open(my $fh, "> $fname") or die "Couldn't create $fname: $!";
    chmod(0755, $fname);

    output_plot_pre($fh, $dev,
		    "Queue Depth",
		    "Time (secs)", "Queue Depth (Number of BIOs)");

    print $fh <<"EOF";
'$datafile' using 1:2 \"%lf %lf\" title \"Depth\" with lines
EOF

    output_plot_post($fh);

    close($fh);
}

sub fname_plot_lat_x2c($$$)
{
    my $dev = shift;
    my $from = shift;
    my $dir = shift;
    return sprintf("%s_plot_lat_%s2c_%s.gp", $dev, lc($from), $dir);
}

sub output_plot_lat_x2c($$$)
{
    my $dev = shift;
    my $from = shift;
    my $dir = shift;
    my $fname = fname_plot_lat_x2c($dev, $from, $dir);
    my $datafile = sprintf("%s_lat_%s2c_%s.dat", $dev, lc($from), $dir);

    open(my $fh, "> $fname") or die "Couldn't create $fname: $!";
    chmod(0755, $fname);

    my $title = sprintf("%s to Complete Latency time - (%s)",
			$action_longname{uc($from)} , $dir_longname{$dir});
    my $label = sprintf("%s2C", uc($from));

    output_plot_pre($fh, $dev,
		    $title, "Time (secs)", "Latency time (secs)");

    print $fh <<"EOF";
'$datafile' using 1:2 \"%lf %lf\" title \"${label}\" with impulses
EOF

    output_plot_post($fh);

    close($fh);
}

sub fname_plot_lat_q2c($$)
{
    my $dev = shift;
    my $dir = shift;
    return fname_plot_lat_x2c($dev, "Q", $dir);
}

sub output_plot_lat_q2c($$)
{
    my $dev = shift;
    my $dir = shift;
    output_plot_lat_x2c($dev, "Q", $dir);
}

sub fname_plot_lat_d2c($$)
{
    my $dev = shift;
    my $dir = shift;
    return fname_plot_lat_x2c($dev, "D", $dir);
}

sub output_plot_lat_d2c($$)
{
    my $dev = shift;
    my $dir = shift;
    output_plot_lat_x2c($dev, "D", $dir);
}

sub fname_plot_seek_nr($$)
{
    my $dev = shift;
    my $dir = shift;
    return sprintf("%s_plot_seek_nr_%s.gp", $dev, $dir);
}

sub output_plot_seek_nr($$)
{
    my $dev = shift;
    my $dir = shift;
    my $fname = fname_plot_seek_nr($dev, $dir);
    my $datafile = sprintf("%s_seek_nr_%s.dat", $dev, $dir);

    open(my $fh, "> $fname") or die "Couldn't create $fname: $!";
    chmod(0755, $fname);

    output_plot_pre($fh, $dev,
		    "Number of Seeks - ($dir_longname{$dir})",
		    "Time (secs)", "Number of Seeks");

    print $fh <<"EOF";
'$datafile' using 1:2 \"%lf %lf\" title \"Seeks\" with lines
EOF

    output_plot_post($fh);

    close($fh);
}

sub fname_plot_seek_step($$)
{
    my $dev = shift;
    my $dir = shift;
    return sprintf("%s_plot_seek_step_%s.gp", $dev, $dir);
}

sub output_plot_seek_step($$)
{
    my $dev = shift;
    my $dir = shift;
    my $fname = fname_plot_seek_step($dev, $dir);
    my $datafile = sprintf("%s_seek_step_%s.dat", $dev, $dir);

    open(my $fh, "> $fname") or die "Couldn't create $fname: $!";
    chmod(0755, $fname);

    output_plot_pre($fh, $dev,
		    "Seek Steps - ($dir_longname{$dir})",
		    "Time (secs)", "Disk offset (byte)",
		    "set format y '%.1s %cB'",
		    "set pointsize 0.5",
		    "",
		    "to_byte(blk) = (blk * 512)");

    print $fh <<"EOF";
'$datafile' using 1:(to_byte(\$2)) title \"D2D seek\" with linespoints
EOF

    output_plot_post($fh);

    close($fh);
}

sub output_plot_summary($)
{
    my $dev = shift;
    my $fname = sprintf("%s_summary.gp", $dev);

    open(my $fh, "> $fname") or die "Couldn't create $fname: $!";

    my @plot_gp = (
		   fname_plot_bno($dev),
		   fname_plot_mbps($dev),
		   fname_plot_iops($dev),
		   fname_plot_qdepth($dev),
		   fname_plot_lat_q2c($dev, "c"),
		   fname_plot_lat_d2c($dev, "c"),
		   fname_plot_seek_nr($dev, "c"),
		   fname_plot_seek_step($dev, "c")
		  );

    my $nr_plots = scalar(@plot_gp);
    my $height = 300 * $nr_plots;
    my $width = 600;

    print $fh <<"EOF";
#!/usr/bin/gnuplot

set term png size ${width}, ${height}
set output '${dev}_summary.png'
set lmargin 15
set multiplot layout ${nr_plots},1 columnsfirst scale 1.0,0.9 offset 0.0,0.0

EOF
    close($fh);

    my $cmd = "cat " . join(" ", @plot_gp) . " | grep -v ^pause >> $fname";
    system($cmd) == 0 or die "Couldn't run command: `$cmd': $?";
    chmod(0755, $fname);

    # Run summary.gp
    $cmd = "./$fname > /dev/null 2>&1";
    system($cmd) == 0 or die "Couldn't run command: `$cmd': $?";
}

sub create_file($$)
{
    my $dev = shift;
    my $name = shift;

    my $filename = sprintf("%s_%s.dat", $dev, $name);
    my $fh;
    open($fh, "> $filename") or die "Couldn't create file: $filename: $!";

    return $fh;
}

# Output block access info
sub add_bno(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;

    my $fname = sprintf("bno_%s_%s", lc($action_longname{$action}), $dir);

    # Create file if need
    if (!defined($stats{$dev}{$fname})) {
	$stats{$dev}{$fname} = create_file($dev, $fname);
    }

    # Output block access per read or write
    my $fh = $stats{$dev}{$fname};
    print $fh "$time $sector_start ", $sector_start + $sector_num, "\n";
}

# Collect completed blocks and IO/s
sub add_io(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;

    $stats{$dev}{"complete_blocks"}[$cur_time] += $sector_num;
    $stats{$dev}{"complete_io"}[$cur_time]++;

    $stats{$dev}{"req_blocks"} += $sector_num;
    $stats{$dev}{"req_nr"}++;
    # Remember maximum sectors on single request
    $stats{$dev}{"req_max"} = max($stats{$dev}{"req_max"}, $sector_num);
    # Remember minimum sectors on single request
    $stats{$dev}{"req_min"} = min($stats{$dev}{"req_min"}, $sector_num);
}

# Update queue depth
sub update_qdepth($$$)
{
    my $dev = shift;
    my $time = shift;
    my $num = shift;

    # Modify queue depth
    $stats{$dev}{"qdepth"} += $num;
    $stats{$dev}{"qdepth_max"} =
	max($stats{$dev}{"qdepth_max"}, $stats{$dev}{"qdepth"});

    my $fname = "qdepth_c";
    # Create file if need
    if (!defined($stats{$dev}{$fname})) {
	$stats{$dev}{$fname} = create_file($dev, $fname);
    }

    my $fh = $stats{$dev}{$fname};
    print $fh "$time " . $stats{$dev}{"qdepth"} . "\n";
}

# Collect Queue(Q) pending I/O
sub add_queue_pending(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;

    # Save pending I/O
    $stats{$dev}{"pending"}{"$sector_start,$sector_num"}{"Q"} = $time;

    # Update queue depth
    update_qdepth($dev, $time, 1);
}

# Update Queue pending I/O for merge
sub update_queue_pending($$$$)
{
    my $dev = shift;
    my $time = shift;
    my $oldkey = shift;
    my $newkey = shift;

    my $start_time = $stats{$dev}{"pending"}{$oldkey}{"Q"};

    # Update pending I/O
    $stats{$dev}{"pending"}{$newkey}{"Q"} = $start_time;
    delete($stats{$dev}{"pending"}{$oldkey});

    # Update queue depth
    update_qdepth($dev, $time, -1);
}

# Collect info of FrontMerge pending I/O
sub add_frontmerge_pending(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;

    my $sector_end = $sector_start + $sector_num;

    foreach my $key (keys($stats{$dev}{"pending"})) {
	my ($s, $n) = split(/,/, $key);

	if ($sector_end == $s) {
	    # Front merge
	    $s = $sector_start;
	    $n += $sector_num;

	    # Remove old pending I/O
	    delete($stats{$dev}{"pending"}{"$sector_start,$sector_num"});

	    update_queue_pending($dev, $time, $key, "$s,$n");
	    return;
	}
    }

    die "Couldn't find FrontMerge: $sector_start, $sector_num\n";
}

# Collect info of merge pending I/O
sub add_backmerge_pending(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;

    foreach my $key (keys($stats{$dev}{"pending"})) {
	my ($s, $n) = split(/,/, $key);

	if ($s + $n == $sector_start) {
	    # Back merge
	    $n += $sector_num;

	    # Remove old pending I/O
	    delete($stats{$dev}{"pending"}{"$sector_start,$sector_num"});

	    update_queue_pending($dev, $time, $key, "$s,$n");
	    return;
	}
    }

    die "Couldn't find BackMerge: $sector_start, $sector_num\n";
}

# Collect Issue(D) pending I/O
sub add_issue_pending(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;

    # Save pending I/O
    $stats{$dev}{"pending"}{"$sector_start,$sector_num"}{"D"} = $time;
}

# Complete(C) pending I/O
sub add_complete_pending(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;

    # Find completed pending I/O (pending I/O may be merged)
    if (!defined($stats{$dev}{"pending"}{"$sector_start,$sector_num"})) {
	die "Found non pending I/O completion\n";
    }
    my $q_time = $stats{$dev}{"pending"}{"$sector_start,$sector_num"}{"Q"};
    my $d_time = $stats{$dev}{"pending"}{"$sector_start,$sector_num"}{"D"};
    delete($stats{$dev}{"pending"}{"$sector_start,$sector_num"});
    my $lat_q2c = $time - $q_time;
    my $lat_d2c = $time - $d_time;

    # Remember stats of I/O latency
    foreach my $d ($dir, "c") {
	$stats{$dev}{"lat_q2c_total_$d"} += $lat_q2c;
	$stats{$dev}{"lat_q2c_max_$d"} =
	    max($stats{$dev}{"lat_q2c_max_$d"}, $lat_q2c);
	$stats{$dev}{"lat_q2c_min_$d"} =
	    min($stats{$dev}{"lat_q2c_min_$d"}, $lat_q2c);
	$stats{$dev}{"lat_q2c_nr_$d"}++;

	$stats{$dev}{"lat_d2c_total_$d"} += $lat_d2c;
	$stats{$dev}{"lat_d2c_nr_$d"}++;
	$stats{$dev}{"lat_d2c_max_$d"} =
	    max($stats{$dev}{"lat_d2c_max_$d"}, $lat_d2c);
	$stats{$dev}{"lat_d2c_min_$d"} =
	    min($stats{$dev}{"lat_d2c_min_$d"}, $lat_d2c);
    }

    for my $d ($dir, "c") {
	my $fname_q2c = sprintf("lat_q2c_%s", $d);
	my $fname_d2c = sprintf("lat_d2c_%s", $d);

	# Create file if need
	if (!defined($stats{$dev}{$fname_q2c})) {
	    $stats{$dev}{$fname_q2c} = create_file($dev, $fname_q2c);
	}
	if (!defined($stats{$dev}{$fname_d2c})) {
	    $stats{$dev}{$fname_d2c} = create_file($dev, $fname_d2c);
	}

	# Output Q2C latency time
	my $fh_q2c = $stats{$dev}{$fname_q2c};
	print $fh_q2c "$time $lat_q2c\n";
	# Output D2C latency time
	my $fh_d2c = $stats{$dev}{$fname_d2c};
	print $fh_d2c "$time $lat_d2c\n";
    }

    # Update queue depth
    update_qdepth($dev, $time, -1);
}

sub seek_distance($$$$)
{
    my ($start, $end, $last_start, $last_end) = @_;

    if (!$seek_relative) {
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
    if (defined($stats{$dev}{"last_end_$dir"})) {
	$distance = seek_distance($start, $end,
				  $stats{$dev}{"last_start_$dir"},
				  $stats{$dev}{"last_end_$dir"});
	if ($distance > $seek_threshold) {
	    $stats{$dev}{"seek_nr_$dir"}[$cur_time]++;
	    $stats{$dev}{"seek_distance_$dir"}[$cur_time] += $distance;
	} else {
	    $distance = 0;
	}
    }
    # Update last location
    $stats{$dev}{"last_time_$dir"} = $time;
    $stats{$dev}{"last_start_$dir"} = $start;
    $stats{$dev}{"last_end_$dir"} = $end;

    return $distance;
}

# Collect issued block
sub add_seek(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;
    my $distance;

    foreach my $d ($dir, "c") {
	my $fname = sprintf("seek_step_%s", $d);
	my $last_time = $stats{$dev}{"last_time_$d"} || 0;
	my $last_end = $stats{$dev}{"last_end_$d"} || 0;

	# Add seek distance
	$distance = add_seek_distance($d, $dev, $time, $sector_start,
				      $sector_start + $sector_num);
	# Create file if need
	if (!defined($stats{$dev}{$fname})) {
	    $stats{$dev}{$fname} = create_file($dev, $fname);
	}
	# Output seek step
	my $fh = $stats{$dev}{$fname};
	my $end = $sector_start + $sector_num;
#	print $fh "#$time $sector_start $end\n";
	if ($distance) {
	    print $fh "\n";
	    print $fh "$last_time $last_end\n";
	    print $fh "$time $sector_start\n";
	}
    }
}

# Queue(Q) data
sub action_queue(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;

    add_bno(@_);
    add_queue_pending(@_);
}

# FrontMerge(F) data
sub action_frontmerge(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;

    add_frontmerge_pending(@_);
}

# BackMerge(M) data
sub action_backmerge(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;

    add_backmerge_pending(@_);
}

# Issue(D) data
sub action_issue(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;

    add_bno(@_);
    add_seek(@_);
    add_issue_pending(@_);
}

# Complete(C) data
sub action_complete(@)
{
    my ($dir, $dev, $cpu, $seq, $time, $pid, $action, $rw,
	$sector_start, $sector_num, $desc) = @_;

    add_bno(@_);
    add_io(@_);
    add_complete_pending(@_);
}

my %action_funcs = (
		    "Q" => \&action_queue,
		    "F" => \&action_frontmerge,
		    "M" => \&action_backmerge,
		    "D" => \&action_issue,
		    "C" => \&action_complete,
		   );

sub main
{
    while (<>) {
	# Parse action line except message(m) action
	if (m!\s+(\d+,\d+)\s+(\d+)\s+(\d+)\s+(\d+\.\d+)\s+(\d+)\s+(\S)\s+(\S+)\s+(\d+)\s+\+\s+(\d+)\s+(.*)!) {
	    my $dev = $1;
	    my $cpu = $2;
	    my $seq = $3;
	    my $time = $4;
	    my $pid = $5;
	    my $action = $6;
	    my $rw = $7;
	    my $sector_start = $8;
	    my $sector_num = $9;
	    my $desc = $10;

	    # Update current time (sec)
	    $cur_time = int($time);

	    # Check direction
	    my $dir;
	    if (index($rw, "R") >= 0) {
		$dir = "r";
	    } elsif (index($rw, "W") >= 0) {
		$dir = "w";
	    } else {
		print "Unknown direction: ", $_;
		next;
	    }

	    # Check action
	    if ($action !~ m!(Q|F|M|D|C)!) {
		next;
	    }

	    if (!defined($stats{$dev}{"start_time"})) {
		$stats{$dev}{"start_time"} = $time;
	    }
	    $stats{$dev}{"end_time"} = $time;

	    # Run callback
	    $action_funcs{$action}->($dir, $dev, $cpu, $seq, $time, $pid,
				     $action, $rw, $sector_start, $sector_num,
				     $desc);
	}
    }

    my %result;

    # Output MB/s and IO/s
    foreach my $dev (keys %stats) {
	my $complete_blocks = $stats{$dev}{"complete_blocks"};
	my $complete_io = $stats{$dev}{"complete_io"};
	my $total_blk = 0;
	my $total_io = 0;

	my $fh_blk = create_file($dev, "blkps_c");
	my $fh_io = create_file($dev, "iops_c");
	for my $t (0..$cur_time) {
	    $total_blk += $complete_blocks->[$t];
	    print $fh_blk "$t.5 ", ($complete_blocks->[$t] || 0), "\n";

	    $total_io += ($complete_io->[$t] || 0);
	    print $fh_io "$t.5 ", ($complete_io->[$t] || 0), "\n";
	}
	close($fh_blk);
	close($fh_io);

	# Remember for short summary
	$result{$dev}{"total_blk"} = $total_blk;
	$result{$dev}{"total_io"} = $total_io;

	# Create plot script
	output_plot_bno($dev);
	output_plot_mbps($dev);
	output_plot_iops($dev);
    }

    # Summary Time
    print <<"EOF";
                      Time
----------------------------------------------------------
  Dev     Start(sec)       End(sec)     Elapse(sec)
EOF
    foreach my $dev (keys %stats) {
	my $elapse = $stats{$dev}{"end_time"} - $stats{$dev}{"start_time"};

	printf " %4s       %8.2f       %8.2f        %8.2f\n",
	    $dev, $stats{$dev}{"start_time"}, $stats{$dev}{"end_time"}, $elapse;

	$result{$dev}{"elapse"} = $elapse;
    }

    # Summary IO
    print <<"EOF";

                      IO (Complete)
-----------------------------------------------------------------
  Dev        MB/s     Total(MB)         IO/s     Total(IO)
EOF
    foreach my $dev (keys %result) {
	my $elapse = $result{$dev}{"elapse"};
	my $total_mb = ($result{$dev}{"total_blk"} * 512) / (1024 * 1024);
	my $total_io = $result{$dev}{"total_io"};

	printf " %4s    %8.2f      %8.2f     %8.2f          %u\n",
	    $dev, $total_mb / $elapse, $total_mb, $total_io / $elapse, $total_io;
    }

    # Summary Request size
    print <<"EOF";

                    Request size (Complete)
-----------------------------------------------------------------
  Dev       Avg       Min       Max    (1 == 512 bytes)
EOF
    foreach my $dev (keys %stats) {
	# Output short summary
	my $avg = $stats{$dev}{"req_blocks"} / $stats{$dev}{"req_nr"};
	printf " %4s  %8.2f  %8u  %8u\n",
	    $dev, $avg, $stats{$dev}{"req_min"}, $stats{$dev}{"req_max"};
    }

    # Summary Q2C/D2C Latency
    print <<"EOF";

                    Queue Depth
-----------------------------------------------------------------
  Dev      Max
EOF
    foreach my $dev (keys %stats) {
	# Output short summary
	my $max = $stats{$dev}{"qdepth_max"} || 0;

	printf " %4s     %4u\n",
	    $dev, $max;

	# Create plot script
	output_plot_qdepth($dev);
    }

    # Summary Q2C/D2C Latency
    print <<"EOF";

                    Latency time
-----------------------------------------------------------------
  Dev   Type  Direction       Avg       Min       Max    (secs)
EOF
    foreach my $dev (keys %stats) {
	foreach my $dir ("r", "w", "c") {
	    foreach my $type ("q2c", "d2c") {
		# Output short summary
		my $total = $stats{$dev}{"lat_${type}_total_${dir}"} || 0;
		my $nr = $stats{$dev}{"lat_${type}_nr_${dir}"} || 0;
		my $max = $stats{$dev}{"lat_${type}_max_${dir}"} || 0;
		my $min = $stats{$dev}{"lat_${type}_min_${dir}"} || 0;
		my $avg = $total / $nr;

		printf " %4s   %4s   %8s  %8.6f  %8.6f  %8.6f\n",
		    $dev, uc($type), $dir_longname{$dir},
		    $avg, $min, $max;

	    }

	    # Create plot script
	    output_plot_lat_q2c($dev, $dir);
	    output_plot_lat_d2c($dev, $dir);
	}
    }

    # Output/Summary Seeks/s
    print <<"EOF";

                      Seeks/s (Issue)
-----------------------------------------------------------------
  Dev   Direction    Seeks/s  Total(Seeks)  Distance(MB)    Total(MB)
EOF
    foreach my $dev (keys %stats) {
	foreach my $dir ("r", "w", "c") {
	    my $fname_nr = "seek_nr_$dir";
	    my $fname_distance = "seek_distance_$dir";
	    my $total_nr = 0;
	    my $total_distance = 0;

	    my $fh = create_file($dev, $fname_nr);
	    for my $t (0..$cur_time) {
		my $nr = $stats{$dev}{$fname_nr}[$t] || 0;
		my $distance = $stats{$dev}{$fname_distance}[$t] || 0;
		$total_nr += $nr;
		$total_distance += $distance;
		print $fh "$t.5 ", $nr, "\n";
	    }
	    close($fh);

	    # Output short summary
	    my $elapse = $result{$dev}{"elapse"};
	    my $avg_nr = $total_nr / $elapse;
	    my $avg_distance = $total_distance / $total_nr;
	    printf " %4s    %8s   %8.2f      %8.2f      %8.2f     %8.2f\n",
		$dev, $dir_longname{$dir}, $avg_nr, $total_nr,
		($avg_distance * 512) / (1024 * 1024),
		($total_distance * 512) / (1024 * 1024);

	    # Create plot script
	    output_plot_seek_nr($dev, $dir);
	    output_plot_seek_step($dev, $dir);
	}
    }

    # Create summary plot
    foreach my $dev (keys %stats) {
	output_plot_summary($dev);
    }

    return 0;
}

sub usage
{
    print <<"EOF";
Usage: $0 [-t|-r] <blktrace log>

-t, --threshold     Seek threshold. If seek distance is smaller than this
                    threshold, this seek is ignored.

-r, --relative      Calculate seek relative distance. I.e. if next access is
                    before last access, use start of last access. Otherwise,
                    use end of last access.
EOF

    exit 0;
}

my %options = (
	       "threshold=i" => \$seek_threshold,
	       "relative"  => \$seek_relative
	      );

Getopt::Long::config("noignorecase");
unless (GetOptions(%options)) {
    usage();
}

main();
