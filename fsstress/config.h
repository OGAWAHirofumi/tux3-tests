#ifndef CONFIG_H
#define CONFIG_H

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <dirent.h>
#include <getopt.h>
#include <malloc.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <unistd.h>

#include <attr/attributes.h>

#define HAVE_ATTR_XATTR_H	1
#define HAVE_ATTR_ATTRIBUTES_H	1
#define HAVE_LINUX_FIEMAP_H	1
#define FALLOCATE		1
#define HAVE_ATTR_LIST		1
#define HAVE_SYS_PRCTL_H	1

#ifdef NO_XFS
#include "xfscompat.h"
#endif

#endif /* !CONFIG_H */
