#!/bin/bash -e

PATH=`pwd`/bin:$PATH

check_core()
{
    COMMAND="tux3"
    HANDLED=0

    for core  in core*; do
	if echo $core | egrep '^core(\.[0-9]+)?$' > /dev/null; then
	    if [ -r $core ]; then
		echo "==== $core ===="
		gdb --batch --eval-command=bt $COMMAND $core

		HANDLED=$(($HANDLED + 1))
	    fi
	fi
    done

    if [ $HANDLED -ne 0 ]; then
	exit 1
    fi
}

run_test()
{
    BACKGROUND=0
    if [ "$1" = "-b" ]; then
	BACKGROUND=1
	shift
    fi

    echo "==== $@ ===="
    if [ $BACKGROUND = 0 ]; then
	time "$@"
    else
	TEST_INTERVAL=10
	TEST_KEEPALIVE=300

	time "$@" &
	pid=$!

	N=0
	while jobs %1 > /dev/null 2>&1; do
	    # Output keepalive message
	    if [ $N != 0 -a $((N % TEST_KEEPALIVE)) = 0 ]; then
		echo "== $N secs == \"$@\" is still running"
		df -h $TUX3_MNT
		vmstat $TEST_INTERVAL 2 | sed -e '3d'
	    else
		sleep $TEST_INTERVAL
	    fi
	    N=$((N + TEST_INTERVAL))
	done

	wait $pid
    fi

    sleep 5
}

mount_tux3()
{
    run_test mount -t tux3 $TUX3_DEV $TUX3_MNT
}

umount_tux3()
{
    run_test df -h $TUX3_MNT
    run_test umount $TUX3_MNT
    run_test -b tux3 fsck $TUX3_DEV
}

trap "check_core" EXIT

TOPDIR=$(pwd)

TUX3_CMD="$1"
TUX3_DEV="$2"

if [ ! -x "$TUX3_CMD" ]; then
    echo "Couldn't find tux3 command: $TUX3_CMD"
    exit 1
fi
if [ ! -b "$TUX3_DEV" ]; then
    echo "Couldn't find block device: $TUX3_DEV"
    exit 1
fi

echo "==== lscpu ===="
lscpu
echo "==== free ===="
free
echo "==== lsblk -iftDm ===="
lsblk -iftDm
echo "==== hdparm -I $TUX3_DEV ===="
hdparm -I $TUX3_DEV || true
echo "==== sdparm -l -i $TUX3_DEV ===="
sdparm -l -i $TUX3_DEV || true
echo "==== uname -a ===="
uname -a
echo "==== id -a ===="
id -a
echo "==== ls -al ===="
ls -al
echo "==== ulimit -a ===="
ulimit -c unlimited
ulimit -a

# Adjust kernel behavior
old_panic_on_oops=$(cat /proc/sys/kernel/panic_on_oops)
echo 1 > /proc/sys/kernel/panic_on_oops

# Copy command to dir in PATH
cp $TUX3_CMD bin

TUX3_MNT="$TOPDIR/mnt"

mkdir $TUX3_MNT

for blocksize in 512 4096; do
    run_test tux3 mkfs -b $blocksize $TUX3_DEV

    # fsx-linux without some operations
    # -W no mmap write
    # -R no mmap read
    # -F no fallocate
    # -H no punch hole
    SIZE=$((200 * 1024 * 1024))
#    NUM_OP=$((1500000 * 24))
    NUM_OP=$((1500000 * 2))

    mount_tux3
    run_test -b fsx-linux	\
	-S 0			\
	-WRFH			\
	-N $NUM_OP		\
	-l $SIZE		\
	$TUX3_MNT/testfile
    umount_tux3

    # fsstress
#    NUM_LOOPS=12
    NUM_LOOPS=2
    for i in $(seq $NUM_LOOPS); do
	# Too big number can be ENOSPC
	NUM_OPS=$((150000 * 2))
	NUM_PROCESS=3

	echo "======== fsstress ($i) ========"
	mount_tux3
	run_test -b rm -rf $TUX3_MNT/fsstress
	run_test -b fsstress		\
	    -n $NUM_OPS			\
	    -p $NUM_PROCESS		\
	    -d $TUX3_MNT/fsstress
	umount_tux3
    done
done

echo $old_panic_on_oops > /proc/sys/kernel/panic_on_oops
