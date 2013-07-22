#!/bin/bash

PATH=$PATH:/devel/git/blktrace:/home/hirofumi/bin/perf/bin

usage()
{
    cat <<EOF
Usage: $0 -d <dev> -o <output> <command line>

    -d      device to watch
    -o      output filename
EOF

    exit
}

while getopts "d:o:h" opt; do
    case "$opt" in
	d)
	    BLKTRACE_OPTS="$BLKTRACE_OPTS -d $OPTARG"
	    ;;
	o)
	    BLKTRACE_DIR=$(dirname $OPTARG)
	    BLKTRACE_OUTPUT=$(basename $OPTARG)
	    PERF_OUTPUT=$OPTARG
	    ;;
	*|h)
	    usage;
	    ;;
    esac
done

shift $(($OPTIND - 1))

if [ -z "$BLKTRACE_DIR" -o -z "$BLKTRACE_OUTPUT" -o -z "$PERF_OUTPUT" ]; then
    usage
fi

BLKTRACE_PID=$BLKTRACE_DIR/$BLKTRACE_OUTPUT.blktrace.pid

trap "kill -INT %1" EXIT INT TERM

# run blktrace
blktrace $BLKTRACE_OPTS -D $BLKTRACE_DIR -o $BLKTRACE_OUTPUT &
sleep 1

# run perf
perf record -e 'sched:*' -a -c 1 -R -g -o $PERF_OUTPUT.perf.data "$@"
