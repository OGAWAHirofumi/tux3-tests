#!/usr/bin/perl
#
# parse-syscalltbl.pl x86_64 linux/usr/include/asm/unistd*.h
#

use strict;
use warnings;
use File::Basename;

if ($#ARGV < 1) {
    die "Usage: syscalltbl.pl <arch> path/to/unistd*.h\n";
}
my $arch = $ARGV[0];
my $path = $ARGV[1];
open(my $fh, "<", $path) or die "Couldn't read $path: $!";

print <<"EOF";
\# Generated from $path
my \%${arch}_syscalltbl = (
EOF
while (<$fh>) {
    if ($arch eq "x86_64" and
	m!#define __NR_(\S+)\s+(\d+)!) {
	print "    $2 => \"$1\",\n";
    }
    elsif ($arch eq "i386" and
	   m!#define __NR_(\S+)\s+(\d+)!) {
	print "    $2 => \"$1\",\n";
    }
    elsif ($arch eq "arm" and
	   m!#define __NR_(\S+)\s+\(__NR_SYSCALL_BASE\+\s*(\d+)\)!) {
	print "    $2 => \"$1\",\n";
    }
    elsif ($arch eq "arm64" and
	   m!#define __NR_(\S+)\s+(\d+)!) {
	print "    $2 => \"$1\",\n";
    }
}
print <<"EOF";
);
EOF
