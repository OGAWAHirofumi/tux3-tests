#include <fcntl.h>
#include <sys/ioctl.h>

#ifndef O_DIRECT
#if defined(__i386__)
#define O_DIRECT        040000  /* direct disk access hint */
#elif defined(__powerpc__)
#define O_DIRECT        0400000
#else
#error please define O_DIRECT
#endif
#endif

#ifndef BLKBSZSET
#define BLKBSZSET  _IOW(0x12,113,size_t)
#endif
