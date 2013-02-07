#ifndef XFSCOMPAT_H
#define XFSCOMPAT_H

#include <stdint.h>
#include <sys/ioctl.h>
#include <linux/ioctl.h>

typedef signed short		__s16;
typedef unsigned short		__u16;
typedef signed int		__s32;
typedef unsigned int		__u32;
typedef signed long long	__s64;
typedef unsigned long long	__u64;

#define MIN(a,b) ((a)<(b) ? (a):(b))
#define MAX(a,b) ((a)>(b) ? (a):(b))

typedef struct xfs_fsop_geom {
	__u32		blocksize;	/* filesystem (data) block size */
	__u32		rtextsize;	/* realtime extent size		*/
	__u64		rtblocks;
	__u64		datablocks;
} xfs_fsop_geom_t;

typedef struct xfs_flock64 {
	__s16		l_whence;
	__s64		l_start;
	__s64		l_len;
} xfs_flock64_t;

typedef struct xfs_bstat {
} xfs_bstat_t;

typedef struct xfs_fsop_bulkreq {
//	ino64_t	*lastip;
	__u64	*lastip;
	__int32_t	icount;
	xfs_bstat_t	*ubuffer;
	__int32_t	*ocount;
} xfs_fsop_bulkreq_t;

struct fsxattr {
	__u32		fsx_xflags;	/* xflags field value (get/set) */
	__u32		fsx_extsize;	/* extsize field value (get/set)*/
};

typedef struct xfs_error_injection {
	__s32		fd;
	__s32		errtag;
} xfs_error_injection_t;

struct dioattr {
	__u32		d_miniosz;
	__u32		d_maxiosz;
	__u32		d_mem;
};

#define XFS_XFLAG_REALTIME		1

#define XFS_IOC_ALLOCSP64		0xaaa1
#define XFS_IOC_FREESP64		0xaaa2
#define XFS_IOC_RESVSP64		0xaaa3
#define XFS_IOC_UNRESVSP64		0xaaa4
#define XFS_IOC_FSBULKSTAT		0xaaa5
#define XFS_IOC_FSBULKSTAT_SINGLE	0xaaa6
#define XFS_IOC_FSGETXATTR		0xaaa7
#define XFS_IOC_DIOINFO			0xaaa8
#define XFS_IOC_FSGEOMETRY		0xaaa9
#define XFS_IOC_FSSETXATTR		0xaaaa
#define XFS_IOC_ERROR_INJECTION		0xaaab
#define XFS_IOC_ERROR_CLEARALL		0xaaac

static inline int xfsctl(const char *path, int fd, int cmd, void *p)
{
	abort();
	return -ENOSYS;
}

#endif /* !XFSCOMPAT_H */
