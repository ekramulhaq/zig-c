/* posix.h - POSIX file I/O declarations for SimpleDB */

#ifndef POSIX_H
#define POSIX_H

int open(char *path, int flags, int mode);
int read(int fd, void *buf, int count);
int write(int fd, void *buf, int count);
int close(int fd);
int lseek(int fd, int offset, int whence);
int unlink(char *path);
int fchmod(int fd, int mode);

/* macOS open() flags */
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#define O_CREAT 512
#define O_TRUNC 1024
#define O_APPEND 8

/* lseek() whence */
#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

/* O_RDWR | O_CREAT | O_TRUNC  (create fresh writable file) */
#define O_RDWR_CREAT_TRUNC 1538

/* O_RDWR only (open existing file for read+write) */
#define O_RDWR_ONLY 2

/* mode 0644 in decimal */
#define MODE_0644 420

/* mode 0600 in decimal */
#define MODE_0600 384

#endif
