/*
 * Copyright (C) 1991, NeXT Computer, Inc.  All Rights Reserverd.
 * Copyright (c) 1998-2001 Apple Computer, Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 *
 * The contents of this file constitute Original Code as defined in and
 * are subject to the Apple Public Source License Version 1.1 (the
 * "License").  You may not use this file except in compliance with the
 * License.  Please obtain a copy of the License at
 * http://www.apple.com/publicsource and read it before using this file.
 *
 * This Original Code and all software distributed under the License are
 * distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.  Please see the
 * License for the specific language governing rights and limitations
 * under the License.
 *
 * @APPLE_LICENSE_HEADER_END@
 *
 *	File:	fsx.c
 *	Author:	Avadis Tevanian, Jr.
 *
 *	File system exerciser.
 *
 *	Rewrite and enhancements 1998-2001 Conrad Minshall -- conrad@mac.com
 *
 *	Various features from Joe Sokol, Pat Dirks, and Clark Warner.
 *
 *	Small changes to work under Linux -- davej@suse.de
 *
 *	Sundry porting patches from Guy Harris 12/2001
 * $FreeBSD: src/tools/regression/fsx/fsx.c,v 1.1 2001/12/20 04:15:57 jkh Exp $
 *
 *	Add multi-file testing feature -- Zach Brown <zab@clusterfs.com>
 */

#include <sys/types.h>
#include <sys/stat.h>
#if defined(_UWIN) || defined(__linux__)
# include <sys/param.h>
# include <limits.h>
# include <time.h>
# include <strings.h>
# include <sys/time.h>
#endif
#include <fcntl.h>
#include <sys/mman.h>
#ifndef MAP_FILE
# define MAP_FILE 0
#else
#ifndef __linux__
# include <sys/dirent.h>
#endif
#endif
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdarg.h>
#include <errno.h>
#ifdef AIO
#include <libaio.h>
#endif
#ifdef FALLOCATE
#include <linux/falloc.h>
#endif
#include "slacker.h"

/*
 *	A log entry is an operation and a bunch of arguments.
 */

struct log_entry {
	int	operation;
	struct timeval tv;
	int	args[3];
};

#define	LOGSIZE	1000

struct log_entry	oplog[LOGSIZE];	/* the log */
int			logptr = 0;	/* current position in log */
int			logcount = 0;	/* total ops */

/*
 * The operation matrix is complex due to conditional execution of different
 * features. Hence when we come to deciding what operation to run, we need to
 * be careful in how we select the different operations. The active operations
 * are mapped to numbers as follows:
 *
 *		lite	!lite
 * READ:	0	0
 * WRITE:	1	1
 * MAPREAD:	2	2
 * MAPWRITE:	3	3
 * TRUNCATE:	-	4
 * FALLOCATE:	-	5
 * PUNCH HOLE:	-	6
 *
 * When mapped read/writes are disabled, they are simply converted to normal
 * reads and writes. When fallocate/fpunch calls are disabled, they are
 * converted to OP_SKIPPED. Hence OP_SKIPPED needs to have a number higher than
 * the operation selction matrix, as does the OP_CLOSEOPEN which is an
 * operation modifier rather than an operation in itself.
 *
 * Because of the "lite" version, we also need to have different "maximum
 * operation" defines to allow the ops to be selected correctly based on the
 * mode being run.
 */

/* common operations */
#define	OP_READ		0
#define OP_WRITE	1
#define OP_MAPREAD	2
#define OP_MAPWRITE	3
#define OP_MAX_LITE	4

/* !lite operations */
#define OP_TRUNCATE	4
#define OP_FALLOCATE	5
#define OP_PUNCH_HOLE	6
#define OP_MAX_FULL	7

/* operation modifiers */
#define OP_CLOSEOPEN	100
#define OP_SKIPPED	101

int page_size;
int page_mask;
int mmap_mask;

char	*original_buf;			/* a pointer to the original data */
char	*good_buf, *good_buf_save;	/* a pointer to the correct data */
char	*temp_buf, *temp_buf_save;	/* a pointer to the current data */
char	*fname;				/* name of our test file */
char	logfile[1024];			/* name of our log file */
char	goodfile[1024];			/* name of our test file */

off_t		file_size = 0;
off_t		biggest = 0;
char		state[256];
unsigned long	testcalls = 0;		/* calls to function "test" */

unsigned long	simulatedopcount = 0;	/* -b flag */
int	closeprob = 0;			/* -c flag */
int	debug = 0;			/* -d flag */
unsigned long	debugstart = 0;		/* -D flag */
int	do_fsync = 0;			/* -f flag */
int	flush = 0;			/* -y flag */
unsigned long	maxfilelen = 256 * 1024;	/* -l flag */
int	sizechecks = 1;			/* -n flag disables them */
int	maxoplen = 64 * 1024;		/* -o flag */
int	quiet = 0;			/* -q flag */
unsigned long progressinterval = 0;	/* -p flag */
int	readbdy = 1;			/* -r flag */
int	style = 0;			/* -s flag */
int	truncbdy = 1;			/* -t flag */
int	writebdy = 1;			/* -w flag */
long	monitorstart = -1;		/* -m flag */
long	monitorend = -1;		/* -m flag */
int	lite = 0;			/* -L flag */
long	numops = -1;			/* -N flag */
int	randomoplen = 1;		/* -O flag disables it */
int	seed = 1;			/* -S flag */
int	fallocate_calls = 1;            /* -F flag disables */
int	punch_hole_calls = 1;           /* -H flag disables */
int     mapped_writes = 1;              /* -W flag disables */
int 	mapped_reads = 1;		/* -R flag disables it */
int	o_direct = 0;			/* -Z */
int	no_truncate = 0;		/* -T */
int	aio = 0;
int	fsxgoodfd = 0;
FILE *	fsxlogf = NULL;
int badoff = -1;

#ifdef AIO
int aio_rw(int rw, int fd, char *buf, unsigned len, unsigned offset);
#define READ 0
#define WRITE 1
#define fsxread(a,b,c,d)	aio_rw(READ, a,b,c,d)
#define fsxwrite(a,b,c,d)	aio_rw(WRITE, a,b,c,d)
#else
#define fsxread(a,b,c,d)	read(a,b,c)
#define fsxwrite(a,b,c,d)	write(a,b,c)
#endif

static int ftruncate_or_write(int fd, off_t length)
{
	if (no_truncate) {
		struct stat statbuf;
		off_t offset;

		if (fstat(fd, &statbuf) < 0) {
			perror("ftruncate: fstat");
			return -1;
		}
		if (length > statbuf.st_size) {
			char c = 0;

			offset = lseek(fd, 0, SEEK_CUR);
			if (offset < 0) {
				perror("ftruncate: lseek");
				return -1;
			}
			if (lseek(fd, length - 1, SEEK_SET) < 0) {
				perror("ftruncate: lseek");
				return -1;
			}
			if (write(fd, &c, 1) != 1) {
				perror("ftruncate: write");
				return -1;
			}
			if (lseek(fd, offset, SEEK_SET) < 0) {
				perror("ftruncate: lseek");
				return -1;
			}
			return 0;
		}
	}
	return ftruncate(fd, length);
}
#define ftruncate(a1, a2)	ftruncate_or_write(a1, a2)

static void *round_up(void *ptr, unsigned long align, unsigned long offset)
{
	unsigned long ret = (unsigned long)ptr;

	ret = ((ret + align - 1) & ~(align - 1));
	ret += offset;
	return (void *)ret;
}

void
vwarnc(int code, const char *fmt, va_list ap)
{
	fprintf(stderr, "fsx: ");
	if (fmt != NULL) {
		vfprintf(stderr, fmt, ap);
		fprintf(stderr, ": ");
	}
	fprintf(stderr, "%s\n", strerror(code));
}


void
warn(const char * fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	vwarnc(errno, fmt, ap);
	va_end(ap);
}

void
__attribute__((format(printf, 1, 2)))
prt(char *fmt, ...)
{
	va_list args;

	va_start(args, fmt);
	vfprintf(stdout, fmt, args);
	va_end(args);

	if (fsxlogf) {
		va_start(args, fmt);
		vfprintf(fsxlogf, fmt, args);
		va_end(args);
	}
}

void
prterr(char *prefix)
{
	prt("%s%s%s\n", prefix, prefix ? ": " : "", strerror(errno));
}


void
log4(int operation, int arg0, int arg1, int arg2, struct timeval *tv)
{
	struct log_entry *le;

	le = &oplog[logptr];
	le->tv = *tv;
	le->operation = operation;
	le->args[0] = arg0;
	le->args[1] = arg1;
	le->args[2] = arg2;
	logptr++;
	logcount++;
	if (logptr >= LOGSIZE)
		logptr = 0;
}


void
logdump(void)
{
	int	i, count, down;
	struct log_entry	*lp;
	char *falloc_type[3] = {"PAST_EOF", "EXTENDING", "INTERIOR"};

	prt("LOG DUMP (%d total operations):\n", logcount);
	if (logcount < LOGSIZE) {
		i = 0;
		count = logcount;
	} else {
		i = logptr;
		count = LOGSIZE;
	}
	for ( ; count > 0; count--) {
		int opnum;

		opnum = i+1 + (logcount/LOGSIZE)*LOGSIZE;
		lp = &oplog[i];
		prt("%d: %lu.%06lu ", opnum,
		    lp->tv.tv_sec, lp->tv.tv_usec);

		switch (lp->operation) {
		case OP_MAPREAD:
			prt("MAPREAD  0x%x thru 0x%x (0x%x bytes)",
			    lp->args[0], lp->args[0] + lp->args[1] - 1,
			    lp->args[1]);
			if (badoff >= lp->args[0] && badoff <
						     lp->args[0] + lp->args[1])
				prt("\t***RRRR***");
			break;
		case OP_MAPWRITE:
			prt("MAPWRITE 0x%x thru 0x%x (0x%x bytes)",
			    lp->args[0], lp->args[0] + lp->args[1] - 1,
			    lp->args[1]);
			if (badoff >= lp->args[0] && badoff <
						     lp->args[0] + lp->args[1])
				prt("\t******WWWW");
			break;
		case OP_READ:
			prt("READ     0x%x thru 0x%x (0x%x bytes)",
			    lp->args[0], lp->args[0] + lp->args[1] - 1,
			    lp->args[1]);
			if (badoff >= lp->args[0] &&
			    badoff < lp->args[0] + lp->args[1])
				prt("\t***RRRR***");
			break;
		case OP_WRITE:
			prt("WRITE    0x%x thru 0x%x (0x%x bytes)",
			    lp->args[0], lp->args[0] + lp->args[1] - 1,
			    lp->args[1]);
			if (lp->args[0] > lp->args[2])
				prt(" HOLE");
			else if (lp->args[0] + lp->args[1] > lp->args[2])
				prt(" EXTEND");
			if ((badoff >= lp->args[0] || badoff >=lp->args[2]) &&
			    badoff < lp->args[0] + lp->args[1])
				prt("\t***WWWW");
			break;
		case OP_TRUNCATE:
			down = lp->args[0] < lp->args[1];
			prt("TRUNCATE %s\tfrom 0x%x to 0x%x",
			    down ? "DOWN" : "UP", lp->args[1], lp->args[0]);
			if (badoff >= lp->args[!down] &&
			    badoff < lp->args[!!down])
				prt("\t******WWWW");
			break;
		case OP_FALLOCATE:
			/* 0: offset 1: length 2: where alloced */
			prt("FALLOC   0x%x thru 0x%x (0x%x bytes) %s",
				lp->args[0], lp->args[0] + lp->args[1],
				lp->args[1], falloc_type[lp->args[2]]);
			if (badoff >= lp->args[0] &&
			    badoff < lp->args[0] + lp->args[1])
				prt("\t******FFFF");
			break;
		case OP_PUNCH_HOLE:
			prt("PUNCH    0x%x thru 0x%x (0x%x bytes)",
			    lp->args[0], lp->args[0] + lp->args[1] - 1,
			    lp->args[1]);
			if (badoff >= lp->args[0] && badoff <
						     lp->args[0] + lp->args[1])
				prt("\t******PPPP");
			break;
		case OP_CLOSEOPEN:
			prt("CLOSE/OPEN");
			break;
		case OP_SKIPPED:
			prt("SKIPPED (no operation)");
			break;
		default:
			prt("BOGUS LOG ENTRY (operation code = %d)!",
			    lp->operation);
		}
		prt("\n");
		i++;
		if (i == LOGSIZE)
			i = 0;
	}
}


void
save_buffer(char *buffer, off_t bufferlength, int fd)
{
	off_t ret;
	ssize_t byteswritten;

	if (fd <= 0 || bufferlength == 0)
		return;

	if (bufferlength > SSIZE_MAX) {
		prt("fsx flaw: overflow in save_buffer\n");
		exit(67);
	}
	if (lite) {
		off_t size_by_seek = lseek(fd, (off_t)0, SEEK_END);
		if (size_by_seek == (off_t)-1)
			prterr("save_buffer: lseek eof");
		else if (bufferlength > size_by_seek) {
			warn("save_buffer: .fsxgood file too short... will"
				"save 0x%llx bytes instead of 0x%llx\n",
				(unsigned long long)size_by_seek,
				(unsigned long long)bufferlength);
			bufferlength = size_by_seek;
		}
	}

	ret = lseek(fd, (off_t)0, SEEK_SET);
	if (ret == (off_t)-1)
		prterr("save_buffer: lseek 0");

	byteswritten = write(fd, buffer, (size_t)bufferlength);
	if (byteswritten != bufferlength) {
		if (byteswritten == -1)
			prterr("save_buffer write");
		else
			warn("save_buffer: short write, 0x%x bytes instead"
				"of 0x%llx\n",
			     (unsigned)byteswritten,
			     (unsigned long long)bufferlength);
	}
}


void
report_failure(int status)
{
	logdump();

	if (fsxgoodfd) {
		if (good_buf) {
			save_buffer(good_buf, file_size, fsxgoodfd);
			prt("Correct content saved for comparison\n");
			prt("(maybe hexdump \"%s\" vs \"%s\")\n",
			    fname, goodfile);
		}
		close(fsxgoodfd);
	}
	exit(status);
}


#define short_at(cp) ((unsigned short)((*((unsigned char *)(cp)) << 8) | \
				        *(((unsigned char *)(cp)) + 1)))

void
check_buffers(unsigned offset, unsigned size)
{
	unsigned char c, t;
	unsigned i = 0;
	unsigned n = 0;
	unsigned op = 0;
	unsigned bad = 0;

	if (memcmp(good_buf + offset, temp_buf, size) != 0) {
		prt("READ BAD DATA: offset = 0x%x, size = 0x%x\n",
		    offset, size);
		prt("OFFSET\tGOOD\tBAD\tRANGE\n");
		while (size > 0) {
			c = good_buf[offset];
			t = temp_buf[i];
			if (c != t) {
			        if (n == 0) {
					bad = short_at(&temp_buf[i]);
				        prt("%#07x\t%#06x\t%#06x", offset,
				            short_at(&good_buf[offset]), bad);
					op = temp_buf[offset & 1 ? i+1 : i];
				}
				n++;
				badoff = offset;
			}
			offset++;
			i++;
			size--;
		}
		if (n) {
		        prt("\t%#7x\n", n);
			if (bad)
				prt("operation# (mod 256) for the bad data"
					"may be %u\n", ((unsigned)op & 0xff));
			else
				prt("operation# (mod 256) for the bad data"
					"unknown, check HOLE and EXTEND ops\n");
		} else
		        prt("????????????????\n");
		report_failure(110);
	}
}

struct test_file {
	char *path;
	int fd;
} *test_files = NULL;

int num_test_files = 0;
enum fd_iteration_policy {
	FD_SINGLE,
	FD_ROTATE,
	FD_RANDOM,
};
int fd_policy = FD_RANDOM;
int fd_last = 0;

struct test_file *
get_tf(void)
{
	unsigned index = 0;

	switch (fd_policy) {
		case FD_ROTATE:
			index = fd_last++;
			break;
		case FD_RANDOM:
			index = random();
			break;
		case FD_SINGLE:
			index = 0;
			break;
		default:
			prt("unknown policy");
			exit(1);
			break;
	}
	return &test_files[ index % num_test_files ];
}

void
assign_fd_policy(char *policy)
{
	if (!strcmp(policy, "random"))
		fd_policy = FD_RANDOM;
	else if (!strcmp(policy, "rotate"))
		fd_policy = FD_ROTATE;
	else {
		prt("unknown -I policy: '%s'\n", policy);
		exit(1);
	}
}

int
get_fd(void)
{
	struct test_file *tf = get_tf();
	return tf->fd;
}

void
open_test_files(char **argv, int argc)
{
	struct test_file *tf;
	int i;

	num_test_files = argc;
	if (num_test_files == 1)
		fd_policy = FD_SINGLE;

	test_files = calloc(num_test_files, sizeof(*test_files));
	if (test_files == NULL) {
		prterr("reallocating space for test files");
		exit(1);
	}

	for (i = 0, tf = test_files; i < num_test_files; i++, tf++) {

		tf->path = argv[i];
		tf->fd = open(tf->path,
			      O_RDWR|(lite ? 0 : O_CREAT|O_TRUNC)|o_direct,
			      0666);
		if (tf->fd < 0) {
			prterr(tf->path);
			exit(91);
		}
	}

	if (quiet || fd_policy == FD_SINGLE)
		return;

	for (i = 0, tf = test_files; i < num_test_files; i++, tf++)
		prt("fd %d: %s\n", i, tf->path);
}

void
close_test_files(void)
{
	int i;
	struct test_file *tf;

	for (i = 0, tf = test_files; i < num_test_files; i++, tf++) {
		if (close(tf->fd)) {
			prterr("close");
			report_failure(99);
		}
	}
}


void
check_size(void)
{
	struct stat	statbuf;
	off_t	size_by_seek;
	int fd = get_fd();

	if (fstat(fd, &statbuf)) {
		prterr("check_size: fstat");
		statbuf.st_size = -1;
	}
	size_by_seek = lseek(fd, (off_t)0, SEEK_END);
	if (file_size != statbuf.st_size || file_size != size_by_seek) {
		prt("Size error: expected 0x%llx stat 0x%llx seek 0x%llx\n",
		    (unsigned long long)file_size,
		    (unsigned long long)statbuf.st_size,
		    (unsigned long long)size_by_seek);
		report_failure(120);
	}
}


void
check_trunc_hack(void)
{
	struct stat statbuf;
	int fd = get_fd();

	ftruncate(fd, (off_t)0);
	ftruncate(fd, (off_t)100000);
	if (fstat(fd, &statbuf)) {
		prterr("trunc_hack: fstat");
		statbuf.st_size = -1;
	}
	if (statbuf.st_size != (off_t)100000) {
		prt("no extend on truncate! not posix!\n");
		exit(130);
	}
	ftruncate(fd, 0);
}

static char *tf_buf = NULL;
static int max_tf_len = 0;

void
alloc_tf_buf(void)
{
	char dummy = '\0';
	int highest = num_test_files - 1;
	int len;

	len = snprintf(&dummy, 0, "%u ", highest);
	if (len < 1) {
		prterr("finding max tf_buf");
		exit(1);
	}
	len++;
	tf_buf = malloc(len);
	if (tf_buf == NULL) {
		prterr("allocating tf_buf");
		exit(1);
	}
	max_tf_len = snprintf(tf_buf, len, "%u ", highest);
	if (max_tf_len < 1) {
		prterr("fiding max_tv_len\n");
		exit(1);
	}
	if (max_tf_len != len - 1) {
		warn("snprintf() gave %d instead of %d?\n",
				max_tf_len, len - 1);
		exit(1);
	}
}

char *
fill_tf_buf(struct test_file *tf)
{
	if (tf_buf == NULL)
		alloc_tf_buf();

	sprintf(tf_buf,"%lu ", (unsigned long)(tf - test_files));
	return tf_buf;
}

void
output_line(struct test_file *tf, int op, unsigned offset,
		unsigned size, struct timeval *tv)
{
	char *tf_num = "";
	char *str;

	char *ops[] = {
		[OP_READ] = "read",
		[OP_WRITE] = "write",
		[OP_TRUNCATE] = "trunc from",
		[OP_MAPREAD] = "mapread",
		[OP_MAPWRITE] = "mapwrite",
		[OP_FALLOCATE] = "falloc",
		[OP_PUNCH_HOLE] = "punch",
	};

	/* W. */
	if (!(!quiet && ((progressinterval &&
			testcalls % progressinterval == 0) ||
		       (debug &&
		        (monitorstart == -1 ||
			 (offset + size > monitorstart &&
			  (monitorend == -1 || offset <= monitorend)))))))
		return;

	if (fd_policy != FD_SINGLE)
		tf_num = fill_tf_buf(tf);

	switch (op) {
	case OP_TRUNCATE:
	case OP_FALLOCATE:
		str = "to";
		break;
	default:
		str = "thru";
		break;
	}
	prt("%06lu %lu.%06lu %.*s%-10s %#08x %s %#08x\t(0x%x bytes)\n",
		testcalls, tv->tv_sec, tv->tv_usec, max_tf_len,
		tf_num, ops[op],
		offset, str,
		offset + size - 1, size);
}

void
doread(unsigned offset, unsigned size)
{
	struct timeval t;
	off_t ret;
	unsigned iret;
	struct test_file *tf = get_tf();
	int fd = tf->fd;

	offset -= offset % readbdy;
	if (o_direct)
		size -= size % readbdy;
	gettimeofday(&t, NULL);
	if (size == 0) {
		if (!quiet && testcalls > simulatedopcount && !o_direct)
			prt("skipping zero size read\n");
		log4(OP_SKIPPED, OP_READ, offset, size, &t);
		return;
	}
	if (size + offset > file_size) {
		if (!quiet && testcalls > simulatedopcount)
			prt("skipping seek/read past end of file\n");
		log4(OP_SKIPPED, OP_READ, offset, size, &t);
		return;
	}

	log4(OP_READ, offset, size, 0, &t);

	if (testcalls <= simulatedopcount)
		return;

	output_line(tf, OP_READ, offset, size, &t);

	ret = lseek(fd, (off_t)offset, SEEK_SET);
	if (ret == (off_t)-1) {
		prterr("doread: lseek");
		report_failure(140);
	}
	iret = fsxread(fd, temp_buf, size, offset);
	if (!quiet && (debug > 1 &&
		        (monitorstart == -1 ||
			 (offset + size > monitorstart &&
			  (monitorend == -1 || offset <= monitorend))))) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu read done\n", t.tv_sec, t.tv_usec);
	}
	if (iret != size) {
		if (iret == -1)
			prterr("doread: read");
		else
			prt("short read: 0x%x bytes instead of 0x%x\n",
			    iret, size);
		report_failure(141);
	}
	check_buffers(offset, size);
}

void
check_eofpage(char *s, unsigned offset, char *p, int size)
{
	unsigned long last_page, should_be_zero;

	if (offset + size <= (file_size & ~page_mask))
		return;
	/*
	 * we landed in the last page of the file
	 * test to make sure the VM system provided 0's
	 * beyond the true end of the file mapping
	 * (as required by mmap def in 1996 posix 1003.1)
	 */
	last_page = ((unsigned long)p + (offset & page_mask) + size) & ~page_mask;

	for (should_be_zero = last_page + (file_size & page_mask);
	     should_be_zero < last_page + page_size;
	     should_be_zero++)
		if (*(char *)should_be_zero) {
			prt("Mapped %s: non-zero data past EOF (0x%llx) "
			    "page offset 0x%lx is 0x%04x\n",
			    s, (unsigned long long)(file_size - 1),
			    should_be_zero & page_mask,
			    short_at(should_be_zero));
			report_failure(205);
		}
}


void
domapread(unsigned offset, unsigned size)
{
	struct timeval t;
	unsigned pg_offset;
	unsigned map_size;
	char    *p;
	struct test_file *tf = get_tf();
	int fd = tf->fd;

	offset -= offset % readbdy;
	gettimeofday(&t, NULL);
	if (size == 0) {
		if (!quiet && testcalls > simulatedopcount)
			prt("skipping zero size read\n");
		log4(OP_SKIPPED, OP_MAPREAD, offset, size, &t);
		return;
	}
	if (size + offset > file_size) {
		if (!quiet && testcalls > simulatedopcount)
			prt("skipping seek/read past end of file\n");
		log4(OP_SKIPPED, OP_MAPREAD, offset, size, &t);
		return;
	}

	log4(OP_MAPREAD, offset, size, 0, &t);

	if (testcalls <= simulatedopcount)
		return;

	output_line(tf, OP_MAPREAD, offset, size, &t);

	pg_offset = offset & page_mask;
	map_size  = pg_offset + size;

	if ((p = mmap(0, map_size, PROT_READ, MAP_FILE | MAP_SHARED, fd,
		      (off_t)(offset - pg_offset))) == MAP_FAILED) {
	        prterr("domapread: mmap");
		report_failure(190);
	}
	if (!quiet && (debug > 1 &&
		        (monitorstart == -1 ||
			 (offset + size > monitorstart &&
			  (monitorend == -1 || offset <= monitorend))))) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu mmap done\n", t.tv_sec, t.tv_usec);
	}
	memcpy(temp_buf, p + pg_offset, size);
	if (!quiet && (debug > 1 &&
		        (monitorstart == -1 ||
			 (offset + size > monitorstart &&
			  (monitorend == -1 || offset <= monitorend))))) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu memcpy done\n", t.tv_sec, t.tv_usec);
	}

	check_eofpage("Read", offset, p, size);

	if (munmap(p, map_size) != 0) {
		prterr("domapread: munmap");
		report_failure(191);
	}
	if (!quiet && (debug > 1 &&
		        (monitorstart == -1 ||
			 (offset + size > monitorstart &&
			  (monitorend == -1 || offset <= monitorend))))) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu munmap done\n", t.tv_sec, t.tv_usec);
	}

	check_buffers(offset, size);
}


void
gendata(char *original_buf, char *good_buf, unsigned offset, unsigned size)
{
	while (size--) {
		good_buf[offset] = testcalls % 256;
		if (offset % 2)
			good_buf[offset] += original_buf[offset];
		offset++;
	}
}

void
doflush(int fd, unsigned offset, unsigned size)
{
	unsigned pg_offset;
	unsigned map_size;
	char    *p;

	if (o_direct == O_DIRECT)
		return;

	pg_offset = offset & mmap_mask;
	map_size  = pg_offset + size;

	if ((p = (char *)mmap(0, map_size, PROT_READ | PROT_WRITE,
			      MAP_FILE | MAP_SHARED, fd,
			      (off_t)(offset - pg_offset))) == (char *)-1) {
		prterr("doflush: mmap");
		report_failure(202);
	}
	if (msync(p, map_size, MS_INVALIDATE) != 0) {
		prterr("doflush: msync");
		report_failure(203);
	}
	if (munmap(p, map_size) != 0) {
		prterr("doflush: munmap");
		report_failure(204);
	}
}

void
dowrite(unsigned offset, unsigned size)
{
	struct timeval t;
	off_t ret;
	unsigned iret;
	struct test_file *tf = get_tf();
	int fd = tf->fd;

	offset -= offset % writebdy;
	if (o_direct)
		size -= size % writebdy;
	gettimeofday(&t, NULL);
	if (size == 0) {
		if (!quiet && testcalls > simulatedopcount && !o_direct)
			prt("skipping zero size write\n");
		log4(OP_SKIPPED, OP_WRITE, offset, size, &t);
		return;
	}

	log4(OP_WRITE, offset, size, file_size, &t);

	gendata(original_buf, good_buf, offset, size);
	if (file_size < offset + size) {
		if (file_size < offset)
			memset(good_buf + file_size, '\0', offset - file_size);
		file_size = offset + size;
		if (lite) {
			warn("Lite file size bug in fsx!");
			report_failure(149);
		}
	}

	if (testcalls <= simulatedopcount)
		return;

	output_line(tf, OP_WRITE, offset, size, &t);

	ret = lseek(fd, (off_t)offset, SEEK_SET);
	if (ret == (off_t)-1) {
		prterr("dowrite: lseek");
		report_failure(150);
	}
	iret = fsxwrite(fd, good_buf + offset, size, offset);
	if (!quiet && (debug > 1 &&
		        (monitorstart == -1 ||
			 (offset + size > monitorstart &&
			  (monitorend == -1 || offset <= monitorend))))) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu write done\n", t.tv_sec, t.tv_usec);
	}
	if (iret != size) {
		if (iret == -1)
			prterr("dowrite: write");
		else
			prt("short write: 0x%x bytes instead of 0x%x\n",
			    iret, size);
		report_failure(151);
	}
	if (do_fsync) {
		if (fsync(fd)) {
			prt("fsync() failed: %s\n", strerror(errno));
			report_failure(152);
		}
	}
	if (flush) {
		doflush(fd, offset, size);
	}
}


void
domapwrite(unsigned offset, unsigned size)
{
	struct timeval t;
	unsigned pg_offset;
	unsigned map_size;
	off_t    cur_filesize;
	char    *p;
	struct test_file *tf = get_tf();
	int fd = tf->fd;

	offset -= offset % writebdy;
	gettimeofday(&t, NULL);
	if (size == 0) {
		if (!quiet && testcalls > simulatedopcount)
			prt("skipping zero size write\n");
		log4(OP_SKIPPED, OP_MAPWRITE, offset, size, &t);
		return;
	}
	cur_filesize = file_size;

	log4(OP_MAPWRITE, offset, size, 0, &t);

	gendata(original_buf, good_buf, offset, size);
	if (file_size < offset + size) {
		if (file_size < offset)
			memset(good_buf + file_size, '\0', offset - file_size);
		file_size = offset + size;
		if (lite) {
			warn("Lite file size bug in fsx!");
			report_failure(200);
		}
	}

	if (testcalls <= simulatedopcount)
		return;

	output_line(tf, OP_MAPWRITE, offset, size, &t);

	if (file_size > cur_filesize) {
	        if (ftruncate(fd, file_size) == -1) {
		        prterr("domapwrite: ftruncate");
			exit(201);
		}
		if (!quiet && (debug > 1 &&
			       (monitorstart == -1 ||
				(offset + size > monitorstart &&
				 (monitorend == -1 || offset <= monitorend))))) {
			gettimeofday(&t, NULL);
			prt("       %lu.%06lu truncate done\n", t.tv_sec, t.tv_usec);
	}
	}
	pg_offset = offset & page_mask;
	map_size  = pg_offset + size;

	if ((p = mmap(0, map_size, PROT_READ | PROT_WRITE, MAP_FILE|MAP_SHARED,
		      fd, (off_t)(offset - pg_offset))) == MAP_FAILED) {
	        prterr("domapwrite: mmap");
		report_failure(202);
	}
	if (!quiet && (debug > 1 &&
		        (monitorstart == -1 ||
			 (offset + size > monitorstart &&
			  (monitorend == -1 || offset <= monitorend))))) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu mmap done\n", t.tv_sec, t.tv_usec);
	}
	memcpy(p + pg_offset, good_buf + offset, size);
	if (!quiet && (debug > 1 &&
		        (monitorstart == -1 ||
			 (offset + size > monitorstart &&
			  (monitorend == -1 || offset <= monitorend))))) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu memcpy done\n", t.tv_sec, t.tv_usec);
	}
	if (msync(p, map_size, MS_SYNC) != 0) {
		prterr("domapwrite: msync");
		report_failure(203);
	}
	if (!quiet && (debug > 1 &&
		        (monitorstart == -1 ||
			 (offset + size > monitorstart &&
			  (monitorend == -1 || offset <= monitorend))))) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu msync done\n", t.tv_sec, t.tv_usec);
	}

	check_eofpage("Write", offset, p, size);

	if (munmap(p, map_size) != 0) {
		prterr("domapwrite: munmap");
		report_failure(204);
	}
	if (!quiet && (debug > 1 &&
		        (monitorstart == -1 ||
			 (offset + size > monitorstart &&
			  (monitorend == -1 || offset <= monitorend))))) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu munmap done\n", t.tv_sec, t.tv_usec);
	}
}


void
dotruncate(unsigned size)
{
	struct timeval t;
	int oldsize = file_size;
	struct test_file *tf = get_tf();
	int fd = tf->fd;

	size -= size % truncbdy;
	gettimeofday(&t, NULL);
	if (size > biggest) {
		biggest = size;
		if (!quiet && testcalls > simulatedopcount)
			prt("truncating to largest ever: 0x%x\n", size);
	}

	log4(OP_TRUNCATE, size, (unsigned)file_size, 0, &t);

	if (size > file_size)
		memset(good_buf + file_size, '\0', size - file_size);
	file_size = size;

	if (testcalls <= simulatedopcount)
		return;

	output_line(tf, OP_TRUNCATE, oldsize, size, &t);

	if (ftruncate(fd, (off_t)size) == -1) {
	        prt("ftruncate1: %x\n", size);
		prterr("dotruncate: ftruncate");
		report_failure(160);
	}
	if (!quiet && debug > 1) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu trunc done\n", t.tv_sec, t.tv_usec);
	}
}

#ifdef FALLOC_FL_PUNCH_HOLE
void
do_punch_hole(unsigned offset, unsigned length)
{
	struct timeval t;
	int max_offset = 0;
	int max_len = 0;
	struct test_file *tf = get_tf();
	int fd = tf->fd;
	int mode = FALLOC_FL_PUNCH_HOLE | FALLOC_FL_KEEP_SIZE;

	gettimeofday(&t, NULL);
	if (length == 0) {
		if (!quiet && testcalls > simulatedopcount)
			prt("skipping zero length punch hole\n");
		log4(OP_SKIPPED, OP_PUNCH_HOLE, offset, length, &t);
		return;
	}

	if (file_size <= (loff_t)offset) {
		if (!quiet && testcalls > simulatedopcount)
			prt("skipping hole punch off the end of the file\n");
		log4(OP_SKIPPED, OP_PUNCH_HOLE, offset, length, &t);
		return;
	}

	log4(OP_PUNCH_HOLE, offset, length, 0, &t);

	if (testcalls <= simulatedopcount)
		return;

	output_line(tf, OP_PUNCH_HOLE, offset, length, &t);

	if (fallocate(fd, mode, (loff_t)offset, (loff_t)length) == -1) {
		prt("punch hole: %x to %x\n", offset, length);
		prterr("do_punch_hole: fallocate");
		report_failure(161);
	}
	if (!quiet && (debug > 1 &&
		        (monitorstart == -1 ||
			 (offset + length > monitorstart &&
			  (monitorend == -1 || offset <= monitorend))))) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu punch done\n", t.tv_sec, t.tv_usec);
	}

	max_offset = offset < file_size ? offset : file_size;
	max_len = max_offset + length <= file_size ? length :
			file_size - max_offset;
	memset(good_buf + max_offset, '\0', max_len);
}
#else /* !FALLOC_FL_PUNCH_HOLE */
void
do_punch_hole(unsigned offset, unsigned length)
{
	return;
}
#endif /* !FALLOC_FL_PUNCH_HOLE */

#ifdef FALLOCATE
/* fallocate is basically a no-op unless extending, then a lot like a
 * truncate */
void
do_preallocate(unsigned offset, unsigned length)
{
	struct timeval t;
	struct test_file *tf = get_tf();
	int fd = tf->fd;
	unsigned end_offset;
	int keep_size;

	gettimeofday(&t, NULL);
        if (length == 0) {
                if (!quiet && testcalls > simulatedopcount)
                        prt("skipping zero length fallocate\n");
                log4(OP_SKIPPED, OP_FALLOCATE, offset, length, &t);
                return;
        }

	keep_size = random() % 2;

	end_offset = keep_size ? 0 : offset + length;

	if (end_offset > biggest) {
		biggest = end_offset;
		if (!quiet && testcalls > simulatedopcount)
			prt("fallocating to largest ever: 0x%x\n", end_offset);
	}

	/*
	 * last arg matches fallocate string array index in logdump:
	 * 	0: allocate past EOF
	 * 	1: extending prealloc
	 * 	2: interior prealloc
	 */
	log4(OP_FALLOCATE, offset, length,
	     (end_offset > file_size) ? (keep_size ? 0 : 1) : 2, &t);

	if (end_offset > file_size) {
		memset(good_buf + file_size, '\0', end_offset - file_size);
		file_size = end_offset;
	}

	if (testcalls <= simulatedopcount)
		return;

	output_line(tf, OP_FALLOCATE, offset, length, &t);

	if (fallocate(fd, keep_size ? FALLOC_FL_KEEP_SIZE : 0, (loff_t)offset, (loff_t)length) == -1) {
	        prt("fallocate: %x to %x\n", offset, length);
		prterr("do_preallocate: fallocate");
		report_failure(161);
	}
	if (!quiet && (debug > 1 &&
		        (monitorstart == -1 ||
			 (offset + length > monitorstart &&
			  (monitorend == -1 || offset <= monitorend))))) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu falloc done\n", t.tv_sec, t.tv_usec);
	}
}
#else /* !FALLOCATE */
void
do_preallocate(unsigned offset, unsigned length)
{
	return;
}
#endif /* !FALLOCATE */

void
writefileimage()
{
	ssize_t iret;
	int fd = get_fd();

	if (lseek(fd, (off_t)0, SEEK_SET) == (off_t)-1) {
		prterr("writefileimage: lseek");
		report_failure(171);
	}
	iret = write(fd, good_buf, file_size);
	if ((off_t)iret != file_size) {
		if (iret == -1)
			prterr("writefileimage: write");
		else
			prt("short write: 0x%lx bytes instead of 0x%llx\n",
			    (unsigned long)iret,
			    (unsigned long long)file_size);
		report_failure(172);
	}
	if (lite ? 0 : ftruncate(fd, file_size) == -1) {
	        prt("ftruncate2: %llx\n", (unsigned long long)file_size);
		prterr("writefileimage: ftruncate");
		report_failure(173);
	}
}


void
docloseopen(void)
{
	struct timeval t;
	struct test_file *tf = get_tf();

	if (testcalls <= simulatedopcount)
		return;

	gettimeofday(&t, NULL);
	log4(OP_CLOSEOPEN, file_size, (unsigned)file_size, 0, &t);

	if (debug)
		prt("%06lu %lu.%06lu close/open\n", testcalls, t.tv_sec,
		    t.tv_usec);
	if (close(tf->fd)) {
		prterr("docloseopen: close");
		report_failure(180);
	}
	if (!quiet && debug > 1) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu close done\n", t.tv_sec, t.tv_usec);
	}
	tf->fd = open(tf->path, O_RDWR|o_direct, 0);
	if (tf->fd < 0) {
		prterr("docloseopen: open");
		report_failure(181);
	}
	if (!quiet && debug > 1) {
		gettimeofday(&t, NULL);
		prt("       %lu.%06lu open done\n", t.tv_sec, t.tv_usec);
	}
}

#define TRIM_OFF_LEN(off, len, size)	\
do {					\
	if (size)			\
		(off) %= (size);	\
	else				\
		(off) = 0;		\
	if ((off) + (len) > (size))	\
		(len) = (size) - (off);	\
} while (0)

void
test(void)
{
	unsigned long	offset;
	unsigned long	size = maxoplen;
	unsigned long	rv = random();
	unsigned long	op;

	if (simulatedopcount > 0 && testcalls == simulatedopcount)
		writefileimage();

	testcalls++;

	if (debugstart > 0 && testcalls >= debugstart)
		debug = 1;

	if (!quiet && testcalls < simulatedopcount && testcalls % 100000 == 0)
		prt("%lu...\n", testcalls);

	offset = random();
	if (randomoplen)
		size = random() % (maxoplen + 1);

	/* calculate appropriate op to run */
	if (lite)
		op = rv % OP_MAX_LITE;
	else
		op = rv % OP_MAX_FULL;

	switch (op) {
	case OP_MAPREAD:
		if (!mapped_reads)
			op = OP_READ;
		break;
	case OP_MAPWRITE:
		if (!mapped_writes)
			op = OP_WRITE;
		break;
	case OP_FALLOCATE:
		if (!fallocate_calls) {
			struct timeval t;
			gettimeofday(&t, NULL);
			log4(OP_SKIPPED, OP_FALLOCATE, offset, size, &t);
			goto out;
		}
		break;
	case OP_PUNCH_HOLE:
		if (!punch_hole_calls) {
			struct timeval t;
			gettimeofday(&t, NULL);
			log4(OP_SKIPPED, OP_PUNCH_HOLE, offset, size, &t);
			goto out;
		}
		break;
	}

	switch (op) {
	case OP_READ:
		TRIM_OFF_LEN(offset, size, file_size);
		doread(offset, size);
		break;

	case OP_WRITE:
		TRIM_OFF_LEN(offset, size, maxfilelen);
		dowrite(offset, size);
		break;

	case OP_MAPREAD:
		TRIM_OFF_LEN(offset, size, file_size);
		domapread(offset, size);
		break;

	case OP_MAPWRITE:
		TRIM_OFF_LEN(offset, size, maxfilelen);
		domapwrite(offset, size);
		break;

	case OP_TRUNCATE:
		if (!style)
			size = random() % maxfilelen;
		dotruncate(size);
		break;

	case OP_FALLOCATE:
		TRIM_OFF_LEN(offset, size, maxfilelen);
		do_preallocate(offset, size);
		break;

	case OP_PUNCH_HOLE:
		TRIM_OFF_LEN(offset, size, file_size);
		do_punch_hole(offset, size);
		break;
	default:
		prterr("test: unknown operation");
		report_failure(42);
		break;
	}

out:
	if (sizechecks && testcalls > simulatedopcount)
		check_size();
	if (closeprob && (rv >> 3) < (1 << 28) / closeprob)
		docloseopen();
}


void
cleanup(sig)
	int	sig;
{
	if (sig)
		prt("signal %d\n", sig);
	prt("testcalls = %lu\n", testcalls);
	exit(sig);
}


void
usage(void)
{
	fprintf(stdout, "usage: %s",
		"fsx [-dfynqLAFHOWZT] [-b opnum] [-c Prob] [-l flen] "
"[-m start:end] [-o oplen] [-p progressinterval] [-r readbdy] [-s style] "
"[-t truncbdy] [-w writebdy] [-D startingop] [-N numops] [-P dirpath] "
"[-S seed] [ -I random|rotate ] fname [additional paths to fname..]\n"
"	-b opnum: beginning operation number (default 1)\n"
"	-c P: 1 in P chance of file close+open at each op (default infinity)\n"
"	-d: debug output for all operations [-d -d = more debugging]\n"
"	-f: fsync() after each write calls\n"
"	-y flush and invalidate cache after I/O\n"
"	-l flen: the upper bound on file size (default 262144)\n"
"	-m start:end: monitor (print debug) specified byte range (default 0:infinity)\n"
"	-n: no verifications of file size\n"
"	-o oplen: the upper bound on operation size (default 65536)\n"
"	-p progressinterval: debug output at specified operation interval\n"
"	-q: quieter operation\n"
"	-r readbdy: 4096 would make reads page aligned (default 1)\n"
"	-s style: 1 gives smaller truncates (default 0)\n"
"	-t truncbdy: 4096 would make truncates page aligned (default 1)\n"
"	-w writebdy: 4096 would make writes page aligned (default 1)\n"
"	-D startingop: debug output starting at specified operation\n"
"	-L: fsxLite - no file creations & no file size changes\n"
"	-A: Use the AIO system calls\n"
#ifdef FALLOCATE
"	-F: Do not use fallocate (preallocation) calls\n"
#endif
#ifdef FALLOC_FL_PUNCH_HOLE
"	-H: Do not use punch hole calls\n"
#endif
"	-N numops: total # operations to do (default infinity)\n"
"	-O: use oplen (see -o flag) for every op (default random)\n"
"	-P: save .fsxlog and .fsxgood files in dirpath (default ./)\n"
"	-S seed: for random # generator (default 1) 0 gets timestamp\n"
"	-W: mapped write operations DISabled\n"
"	-R: read() system calls only (mapped reads disabled)\n"
"	-I: When multiple paths to the file are given each operation uses\n"
"	    a different path.  Iterate through them in order with 'rotate'\n"
"	    or chose then at 'random'.  (defaults to random)\n"
"	-Z: O_DIRECT (use -R, -W, -r and -w too)\n"
"	-T: use lseek()+write() instead of truncate()\n"
"	fname: this filename is REQUIRED (no default)\n");
	exit(90);
}


int
getnum(char *s, char **e)
{
	int ret = -1;

	*e = (char *) 0;
	ret = strtol(s, e, 0);
	if (*e)
		switch (**e) {
		case 'b':
		case 'B':
			ret *= 512;
			*e = *e + 1;
			break;
		case 'k':
		case 'K':
			ret *= 1024;
			*e = *e + 1;
			break;
		case 'm':
		case 'M':
			ret *= 1024*1024;
			*e = *e + 1;
			break;
		case 'w':
		case 'W':
			ret *= 4;
			*e = *e + 1;
			break;
		}
	return (ret);
}

#ifdef AIO
#define QSZ     1024
io_context_t	io_ctx;
struct iocb 	iocb;

int aio_setup()
{
	int ret;
	ret = io_queue_init(QSZ, &io_ctx);
	if (ret != 0) {
		fprintf(stderr, "aio_setup: io_queue_init failed: %s\n",
                        strerror(ret));
                return(-1);
        }
        return(0);
}

int
__aio_rw(int rw, int fd, char *buf, unsigned len, unsigned offset)
{
	struct io_event event;
	static struct timespec ts;
	struct iocb *iocbs[] = { &iocb };
	int ret;
	long res;

	if (rw == READ) {
		io_prep_pread(&iocb, fd, buf, len, offset);
	} else {
		io_prep_pwrite(&iocb, fd, buf, len, offset);
	}

	ts.tv_sec = 30;
	ts.tv_nsec = 0;
	ret = io_submit(io_ctx, 1, iocbs);
	if (ret != 1) {
		fprintf(stderr, "errcode=%d\n", ret);
		fprintf(stderr, "aio_rw: io_submit failed: %s\n",
				strerror(ret));
		goto out_error;
	}

	ret = io_getevents(io_ctx, 1, 1, &event, &ts);
	if (ret != 1) {
		if (ret == 0)
			fprintf(stderr, "aio_rw: no events available\n");
		else {
			fprintf(stderr, "errcode=%d\n", -ret);
			fprintf(stderr, "aio_rw: io_getevents failed: %s\n",
				 	strerror(-ret));
		}
		goto out_error;
	}
	if (len != event.res) {
		/*
		 * The b0rked libaio defines event.res as unsigned.
		 * However the kernel strucuture has it signed,
		 * and it's used to pass negated error value.
		 * Till the library is fixed use the temp var.
		 */
		res = (long)event.res;
		if (res >= 0)
			fprintf(stderr, "bad io length: %lu instead of %u\n",
					res, len);
		else {
			fprintf(stderr, "errcode=%ld\n", -res);
			fprintf(stderr, "aio_rw: async io failed: %s\n",
					strerror(-res));
			ret = res;
			goto out_error;
		}
	}
	return event.res;

out_error:
	/*
	 * The caller expects error return in traditional libc
	 * convention, i.e. -1 and the errno set to error.
	 */
	errno = -ret;
	return -1;
}

int aio_rw(int rw, int fd, char *buf, unsigned len, unsigned offset)
{
	int ret;

	if (aio) {
		ret = __aio_rw(rw, fd, buf, len, offset);
	} else {
		if (rw == READ)
			ret = read(fd, buf, len);
		else
			ret = write(fd, buf, len);
	}
	return ret;
}
#endif /* !AIO */

void
test_fallocate()
{
#ifdef FALLOCATE
	if (!lite && fallocate_calls) {
		int fd = get_fd();

		if (fallocate(fd, 0, 0, 1) && errno == EOPNOTSUPP) {
			if(!quiet)
				warn("main: filesystem does not support fallocate, disabling\n");
			fallocate_calls = 0;
		} else {
			ftruncate(fd, 0);
		}
	}
#else /* ! FALLOCATE */
	fallocate_calls = 0;
#endif

}

void
test_punch_hole()
{
#ifdef FALLOC_FL_PUNCH_HOLE
	if (!lite && punch_hole_calls) {
		int fd = get_fd();

		if (fallocate(fd, FALLOC_FL_PUNCH_HOLE | FALLOC_FL_KEEP_SIZE,
				0, 1) && errno == EOPNOTSUPP) {
			if(!quiet)
				warn("main: filesystem does not support fallocate punch hole, disabling");
			punch_hole_calls = 0;
		} else
			ftruncate(fd, 0);
	}
#else /* ! PUNCH HOLE */
	punch_hole_calls = 0;
#endif
}

int
main(int argc, char **argv)
{
	int	i, style, ch;
	char	*endp;
	int  dirpath = 0;

	goodfile[0] = 0;
	logfile[0] = 0;

	page_size = getpagesize();
	page_mask = page_size - 1;
	mmap_mask = page_mask;

	setvbuf(stdout, (char *)0, _IOLBF, 0); /* line buffered stdout */

	while ((ch = getopt(argc, argv,
			    "b:c:dfyl:m:no:p:qr:s:t:w:AD:I:FHLN:OP:RS:WZT"))
	       != EOF)
		switch (ch) {
		case 'b':
			simulatedopcount = getnum(optarg, &endp);
			if (!quiet)
				fprintf(stdout, "Will begin at operation"
					"%ld\n",
					simulatedopcount);
			if (simulatedopcount == 0)
				usage();
			simulatedopcount -= 1;
			break;
		case 'c':
			closeprob = getnum(optarg, &endp);
			if (!quiet)
				fprintf(stdout,
					"Chance of close/open is 1 in %d\n",
					closeprob);
			if (closeprob <= 0)
				usage();
			break;
		case 'd':
			debug++;
			break;
		case 'f':
			do_fsync = 1;
			break;
		case 'y':
			flush = 1;
			break;
		case 'l':
			maxfilelen = getnum(optarg, &endp);
			if (maxfilelen <= 0)
				usage();
			break;
		case 'm':
			monitorstart = getnum(optarg, &endp);
			if (monitorstart < 0)
				usage();
			if (!endp || *endp++ != ':')
				usage();
			monitorend = getnum(endp, &endp);
			if (monitorend < 0)
				usage();
			if (monitorend == 0)
				monitorend = -1; /* aka infinity */
			debug = 1;
		case 'n':
			sizechecks = 0;
			break;
		case 'o':
			maxoplen = getnum(optarg, &endp);
			if (maxoplen <= 0)
				usage();
			break;
		case 'p':
			progressinterval = getnum(optarg, &endp);
			if (progressinterval < 0)
				usage();
			break;
		case 'q':
			quiet = 1;
			break;
		case 'r':
			readbdy = getnum(optarg, &endp);
			if (readbdy <= 0)
				usage();
			break;
		case 's':
			style = getnum(optarg, &endp);
			if (style < 0 || style > 1)
				usage();
			break;
		case 't':
			truncbdy = getnum(optarg, &endp);
			if (truncbdy <= 0)
				usage();
			break;
		case 'w':
			writebdy = getnum(optarg, &endp);
			if (writebdy <= 0)
				usage();
			break;
		case 'A':
		        aio = 1;
			break;
		case 'D':
			debugstart = getnum(optarg, &endp);
			if (debugstart < 1)
				usage();
			break;
		case 'I':
			assign_fd_policy(optarg);
			break;
		case 'F':
			fallocate_calls = 0;
			break;
		case 'H':
			punch_hole_calls = 0;
			break;
		case 'L':
		        lite = 1;
			break;
		case 'N':
			numops = getnum(optarg, &endp);
			if (numops < 0)
				usage();
			break;
		case 'O':
			randomoplen = 0;
			break;
		case 'P':
			strncpy(goodfile, optarg, sizeof(goodfile));
			strcat(goodfile, "/");
			strncpy(logfile, optarg, sizeof(logfile));
			strcat(logfile, "/");
			dirpath = 1;
			break;
                case 'R':
                        mapped_reads = 0;
                        break;
		case 'S':
                        seed = getnum(optarg, &endp);
			if (seed == 0)
				seed = time(0) % 10000;
			if (!quiet)
				fprintf(stdout, "Seed set to %d\n", seed);
			if (seed < 0)
				usage();
			break;
		case 'W':
		        mapped_writes = 0;
			if (!quiet)
				fprintf(stdout, "mapped writes DISABLED\n");
			break;
		case 'Z':
			o_direct = O_DIRECT;
			break;
		case 'T':
		        no_truncate = 1;
			break;

		default:
			usage();
			/* NOTREACHED */
		}
	argc -= optind;
	argv += optind;
	if (argc < 1)
		usage();
	fname = argv[0];

	signal(SIGHUP,	cleanup);
	signal(SIGINT,	cleanup);
	signal(SIGPIPE,	cleanup);
	signal(SIGALRM,	cleanup);
	signal(SIGTERM,	cleanup);
	signal(SIGXCPU,	cleanup);
	signal(SIGXFSZ,	cleanup);
	signal(SIGVTALRM,	cleanup);
	signal(SIGUSR1,	cleanup);
	signal(SIGUSR2,	cleanup);

	initstate(seed, state, 256);
	setstate(state);

	open_test_files(argv, argc);

	strncat(goodfile, dirpath ? basename(fname) : fname, 256);
	strcat (goodfile, ".fsxgood");
	fsxgoodfd = open(goodfile, O_RDWR|O_CREAT|O_TRUNC, 0666);
	if (fsxgoodfd < 0) {
		prterr(goodfile);
		exit(92);
	}
	strncat(logfile, dirpath ? basename(fname) : fname, 256);
	strcat (logfile, ".fsxlog");
	fsxlogf = fopen(logfile, "w");
	if (fsxlogf == NULL) {
		prterr(logfile);
		exit(93);
	}

#ifdef AIO
	if (aio)
		aio_setup();
#endif

	if (lite) {
		off_t ret;
		int fd = get_fd();
		file_size = maxfilelen = lseek(fd, (off_t)0, SEEK_END);
		if (file_size == (off_t)-1) {
			prterr(fname);
			warn("main: lseek eof");
			exit(94);
		}
		ret = lseek(fd, (off_t)0, SEEK_SET);
		if (ret == (off_t)-1) {
			prterr(fname);
			warn("main: lseek 0");
			exit(95);
		}
	}
	original_buf = (char *) malloc(maxfilelen);
	if (original_buf == NULL)
		exit(96);
	for (i = 0; i < maxfilelen; i++)
		original_buf[i] = random() % 256;

	good_buf_save = good_buf = (char *) malloc(maxfilelen + writebdy);
	if (good_buf == NULL)
		exit(97);
	good_buf = round_up(good_buf, writebdy, 0);
	memset(good_buf, '\0', maxfilelen);

	temp_buf_save = temp_buf = (char *) malloc(maxoplen + readbdy);
	if (temp_buf == NULL)
		exit(99);
	temp_buf = round_up(temp_buf, readbdy, 0);
	memset(temp_buf, '\0', maxoplen);

	if (lite) {	/* zero entire existing file */
		ssize_t written;
		int fd = get_fd();

		written = write(fd, good_buf, (size_t)maxfilelen);
		if (written != maxfilelen) {
			if (written == -1) {
				prterr(fname);
				warn("main: error on write");
			} else
				warn("main: short write, 0x%x bytes instead"
					"of 0x%lx\n",
				     (unsigned)written, maxfilelen);
			exit(98);
		}
	} else
		check_trunc_hack();

	test_fallocate();
	test_punch_hole();

	while (numops == -1 || numops--)
		test();

	close_test_files();
	prt("All operations completed A-OK!\n");

	if (tf_buf)
		free(tf_buf);

	free(original_buf);
	free(good_buf_save);
	free(temp_buf_save);

	return 0;
}
