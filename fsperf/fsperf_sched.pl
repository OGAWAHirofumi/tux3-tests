#!/usr/bin/perl
#
# Usage:
#  $ perf record -a -g -c1 -e 'sched:*' <command>
#  $ perf script -k vmlinux -i perf.data -f comm,tid,cpu,time,event,trace,ip,sym,symoff > sched.log
#  $ fsperf_sched.pl sched.log
#
# Useful:
#  $ perf timechart -i perf.data -p <pid> -p <pid>
#

use strict;
use warnings;
use bignum;

my %time;
my %proc;
my @callchain_db;

sub sec_to_ns($)
{
    my $sec = shift;
    return $sec * 1000000000;
}

sub ns_to_sec($)
{
    my $ns = shift;
    return $ns / 1000000000;
}

sub match_callchain($$)
{
    my $a = shift;
    my $b = shift;

    if ($#$a != $#$b) {
	return 0;
    }

    for my $i (0..$#$a) {
	return 0 if $a->[$i] ne $b->[$i];
    }

    return 1;
}

sub add_callchain($)
{
    my $callchain = shift;

    for my $i (0..$#callchain_db) {
	if (match_callchain($callchain_db[$i], $callchain)) {
	    return $i;
	}
    }

    # Register as new callchain
    push(@callchain_db, $callchain);

    return $#callchain_db;
}

sub get_callchain($)
{
    my $id = shift;
    return @{$callchain_db[$id]};
}

sub find_caller(@)
{
    my @callchain = @_;
    my @uninterest = qw(
			 set_task_cpu
			 try_to_wake_up
			 default_wake_function
			 autoremove_wake_function
			 __wake_up_common
			 __wake_up_sync_key

			 update_curr
			 enqueue_task_fair
			 enqueue_task
			 activate_task
			 ttwu_do_activate

			 ttwu_do_wakeup

			 wake_up_process
			 wake_up_worker
			 insert_work
			 __queue_work

			 __schedule
			 schedule_timeout

			 io_schedule
			 sleep_on_page
			 __wait_on_bit
			 wait_on_page_bit
		      );

    # Find interesting function
    foreach my $line (@callchain) {
	my ($addr, $func) = split(/\s/, $line);

	# Normalize function name
	# (ttwu_do_activate.constprop.76 => ttwu_do_activate)
	$func =~ s/([^\.]+)\.\S+/$1/;

	if (!grep(/$func/, @uninterest)) {
	    return $line;
	}
    }

    return "unknown";
}

sub process_event_sched_switch(@)
{
    my ($common_comm, $common_tid, $common_cpu, $common_time,
	$event, $trace, @callchain) = @_;

    if ($trace =~ m!prev_comm=(.+) prev_pid=(\d+) prev_prio=(\d+) prev_state=(\S+) ==> next_comm=(.+) next_pid=(\d+) next_prio=(\d+)!) {
	my $prev_comm = $1;
	my $prev_pid = $2;
	my $prev_prio = $3;
	my $prev_state = $4;
	my $next_comm = $5;
	my $next_pid = $6;
	my $next_prio = $7;

	$proc{$prev_pid}{"comm"} = $prev_comm;
	$proc{$prev_pid}{"state"} = $prev_state;
	$proc{$prev_pid}{"reason"} = \@callchain;
    } else {
	die;
    }
}

sub process_event_sched_process_exec(@)
{
    my ($common_comm, $common_tid, $common_cpu, $common_time,
	$event, $trace, @callchain) = @_;

}

sub process_event_sched_stat_runtime(@)
{
    my ($common_comm, $common_tid, $common_cpu, $common_time,
	$event, $trace, @callchain) = @_;

    if ($trace =~ m!comm=(.*) pid=(\d+) runtime=(\d+) \[ns\] vruntime=(\d+) \[ns\]!) {
	my $comm = $1;
	my $pid = $2;
	my $runtime = $3;
	my $vruntime = $4;

	$proc{$pid}{"runtime_ns"} += $runtime;
    } else {
	die;
    }
}

sub process_event_sched_stat_sleep(@)
{
    my ($common_comm, $common_tid, $common_cpu, $common_time,
	$event, $trace, @callchain) = @_;

    if ($trace =~ m!comm=(.+) pid=(\d+) delay=(\d+) \[ns\]!) {
	my $comm = $1;
	my $pid = $2;
	my $delay = $3;

	# If this is not initial wakeup, counts delay
	if (defined($proc{$pid}{"reason"})) {
	    my $id = add_callchain($proc{$pid}{"reason"});

	    $proc{$pid}{"sleep_nr"}++;
	    $proc{$pid}{"sleep_ns"} += $delay;

	    $proc{$pid}{"sleep"}{$id}{"nr"}++;
	    $proc{$pid}{"sleep"}{$id}{"ns"} += $delay;
	    if (!defined($proc{$pid}{"sleep"}{$id}{"min"})) {
		$proc{$pid}{"sleep"}{$id}{"min"} = $delay;
	    } else {
		if ($proc{$pid}{"sleep"}{$id}{"min"} > $delay) {
		    $proc{$pid}{"sleep"}{$id}{"min"} = $delay;
		}
	    }
	    if (!defined($proc{$pid}{"sleep"}{$id}{"max"})) {
		$proc{$pid}{"sleep"}{$id}{"max"} = $delay;
	    } else {
		if ($proc{$pid}{"sleep"}{$id}{"max"} < $delay) {
		    $proc{$pid}{"sleep"}{$id}{"max"} = $delay;
		}
	    }
	}
    } else {
	die;
    }
}

sub process_event_sched_stat_blocked(@)
{
    my ($common_comm, $common_tid, $common_cpu, $common_time,
	$event, $trace, @callchain) = @_;

    if ($trace =~ m!comm=(.+) pid=(\d+) delay=(\d+) \[ns\]!) {
	my $comm = $1;
	my $pid = $2;
	my $delay = $3;

	# If this is not initial wakeup, counts delay
	if (defined($proc{$pid}{"reason"})) {
	    my $id = add_callchain($proc{$pid}{"reason"});

	    $proc{$pid}{"block_nr"}++;
	    $proc{$pid}{"block_ns"} += $delay;

	    $proc{$pid}{"block"}{$id}{"nr"}++;
	    $proc{$pid}{"block"}{$id}{"ns"} += $delay;
	    if (!defined($proc{$pid}{"block"}{$id}{"min"})) {
		$proc{$pid}{"block"}{$id}{"min"} = $delay;
	    } else {
		if ($proc{$pid}{"block"}{$id}{"min"} > $delay) {
		    $proc{$pid}{"block"}{$id}{"min"} = $delay;
		}
	    }
	    if (!defined($proc{$pid}{"block"}{$id}{"max"})) {
		$proc{$pid}{"block"}{$id}{"max"} = $delay;
	    } else {
		if ($proc{$pid}{"block"}{$id}{"max"} < $delay) {
		    $proc{$pid}{"block"}{$id}{"max"} = $delay;
		}
	    }
	}
    } else {
	die;
    }
}

my %process_event_funcs =
    (
     "sched:sched_switch" => \&process_event_sched_switch,
     "sched:sched_process_exec" => \&process_event_sched_process_exec,
     "sched:sched_stat_runtime" => \&process_event_sched_stat_runtime,
     "sched:sched_stat_sleep" => \&process_event_sched_stat_sleep,
     "sched:sched_stat_blocked" => \&process_event_sched_stat_blocked,
    );

sub process_event(@)
{
    my ($common_comm, $common_tid, $common_cpu, $common_time,
	$event, $trace, @callchain) = @_;

    # Save start time of profile
    if (!defined($time{"start"})) {
	$time{"start"} = sec_to_ns($common_time);
    }

    if ($process_event_funcs{$event}) {
	$process_event_funcs{$event}->(@_);
    }

    # Save end time of profile
    $time{"end"} = sec_to_ns($common_time);
}

sub sort_by_ns($)
{
    my $ref = shift;

    return sort { $ref->{$b}{"ns"} <=> $ref->{$a}{"ns"} } keys(%$ref);
}

while (<>) {
    # Skip empty line
    next if (m!^$!);
    # Skip comment
    next if (m!^#!);

    # Find trace event
    if (m!^(\S+)\s+(\d+)\s+\[(\d+)\]\s+(\d+\.\d+):\s+(\S+):\s+(.*)!) {
	my $common_comm = $1;
	my $common_tid = $2;
	my $common_cpu = $3;
	my $common_time = $4;
	my $event = $5;
	my $trace = $6;

	# Parse callchain
	my @callchain;
	while (<>) {
	    if (m!^\t(.*)!) {
		push(@callchain, $1);
		next;
	    }
	    last;
	}
	#print "$common_comm, $common_tid, $common_cpu, $common_time, $event, $trace\n";
	#print find_caller(@callchain), "\n";
	#print join("\n", @callchain), "\n";

	process_event($common_comm, $common_tid, $common_cpu, $common_time,
		      $event, $trace, @callchain);
    }
}

my $estimate = ns_to_sec($time{"end"} - $time{"start"});
print "estimate time: $estimate\n";

foreach my $pid (sort(keys(%proc))) {
    my $comm = $proc{$pid}{"comm"};
    my $runtime_ns = ns_to_sec($proc{$pid}{"runtime_ns"} || 0);
    my $sleep_nr = $proc{$pid}{"sleep_nr"};
    my $sleep_ns = $proc{$pid}{"sleep_ns"};
    my $block_nr = $proc{$pid}{"block_nr"};
    my $block_ns = $proc{$pid}{"block_ns"};

    print "$comm ($pid): runtime $runtime_ns\n";

    if ($proc{$pid}{"sleep_nr"}) {
	my $sleep = $proc{$pid}{"sleep"};

	print "$comm ($pid): nr $sleep_nr, sleep ", ns_to_sec($sleep_ns), "\n";
	foreach my $id (sort_by_ns($sleep)) {
	    my $nr = $sleep->{$id}{"nr"};
	    my $ns = $sleep->{$id}{"ns"};
	    my $min = ns_to_sec($sleep->{$id}{"min"});
	    my $max = ns_to_sec($sleep->{$id}{"max"});
	    my $avg = ns_to_sec(int($ns / $nr));
	    my $sec = ns_to_sec($ns);

	    printf("\t$comm ($pid): %.02f%%, nr $nr, sleep $sec (avg $avg, min $min, max $max)\n",
		   $ns * 100 / $sleep_ns);
	    print "\t\t", join("\n\t\t", get_callchain($id));
	    print "\n";
	}
    }

    if ($proc{$pid}{"block_nr"}) {
	my $block = $proc{$pid}{"block"};

	print "$comm ($pid): nr $block_nr, block ", ns_to_sec($block_ns), "\n";
	foreach my $id (sort_by_ns($block)) {
	    my $nr = $block->{$id}{"nr"};
	    my $ns = $block->{$id}{"ns"};
	    my $min = ns_to_sec($block->{$id}{"min"});
	    my $max = ns_to_sec($block->{$id}{"max"});
	    my $avg = ns_to_sec(int($ns / $nr));
	    my $sec = ns_to_sec($ns);

	    printf("\t$comm ($pid): %.02f%%, nr $nr, block $sec (avg $avg, min $min, max $max)\n",
		   $ns * 100 / $block_ns);
	    print "\t\t", join("\n\t\t", get_callchain($id));
	    print "\n";
	}
    }
}
