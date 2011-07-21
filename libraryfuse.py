#!/usr/bin/env python

#    Copyright (C) 2001  Jeff Epler  <jepler@unpythonic.dhs.org>
#    Copyright (C) 2006  Csaba Henk  <csaba.henk@creo.hu>
#
#    This program can be distributed under the terms of the GNU LGPL.
#    See the file COPYING.
#

import os, sys
from errno import *
from stat import *
import fcntl
import fuse
from fuse import Fuse
import os.path

from logbook import FileHandler, debug, DEBUG
log_handler = FileHandler('/tmp/libraryfuse.log', level=DEBUG)
log_handler.push_application()

debug('Starting')

fuse.fuse_python_api = (0, 2)
fuse.feature_assert('stateful_files', 'has_init')

directories_to_merge = ['/var', '/usr']

def flag2mode(flags):
    md = {os.O_RDONLY: 'r', os.O_WRONLY: 'w', os.O_RDWR: 'w+'}
    m = md[flags & (os.O_RDONLY | os.O_WRONLY | os.O_RDWR)]

    if flags | os.O_APPEND:
        m = m.replace('w', 'a', 1)

    return m


class LibraryFuse(Fuse):

    def __init__(self, *args, **kw):
        Fuse.__init__(self, *args, **kw)
        self.directories_to_merge = directories_to_merge

    def getattr(self, path):
        debug('getattr with %s' % path)
        for library_part in self.directories_to_merge:
            real_path = library_part + path
            debug('trying %s' % real_path)
            if os.path.exists(real_path):
                lstat = os.lstat(real_path)
                return lstat
            else:
                debug('%s not found, checking next' % real_path)

    def readlink(self, path):
        debug('readlink called with {}'.format(path))
        return os.readlink("." + path)

    def readdir(self, path, offset):
        debug('readdir called')
        elements = set()
        for library_part in self.directories_to_merge:
            for e in os.listdir(library_part + path):
                elements.add(e)
        #yield fuse.Direntry(repr(elements))
        for element in elements:
            yield fuse.Direntry(element)

    def unlink(self, path):
        debug('unlink called')
        return -ENOSYS
        os.unlink("." + path)

    def rmdir(self, path):
        debug('rmdir')
        return -ENOSYS
        os.rmdir("." + path)

    def symlink(self, path, path1):
        debug('symlink')
        return -ENOSYS
        os.symlink(path, "." + path1)

    def rename(self, path, path1):
        debug('rename')
        return -ENOSYS
        os.rename("." + path, "." + path1)

    def link(self, path, path1):
        debug('link')
        return -ENOSYS
        os.link("." + path, "." + path1)

    def chmod(self, path, mode):
        debug('chmod')
        return -ENOSYS
        os.chmod("." + path, mode)

    def chown(self, path, user, group):
        debug('chown')
        return -ENOSYS
        os.chown("." + path, user, group)

    def truncate(self, path, len):
        debug('truncate')
        return -ENOSYS
        f = open("." + path, "a")
        f.truncate(len)
        f.close()

    def mknod(self, path, mode, dev):
        debug('mknod')
        return -ENOSYS
        os.mknod("." + path, mode, dev)

    def mkdir(self, path, mode):
        debug('mkdir')
        return -ENOSYS
        os.mkdir("." + path, mode)

    def utime(self, path, times):
        debug('utime')
        return -ENOSYS
        os.utime("." + path, times)

#    The following utimens method would do the same as the above utime method.
#    We can't make it better though as the Python stdlib doesn't know of
#    subsecond preciseness in acces/modify times.
#  
#    def utimens(self, path, ts_acc, ts_mod):
#      os.utime("." + path, (ts_acc.tv_sec, ts_mod.tv_sec))

    def access(self, path, mode):
        debug('access')
        for library_part in self.directories_to_merge:
            if not os.access(library_part + path, mode):
                return -EACCESS

    def statfs(self):
        """
        Should return an object with statvfs attributes (f_bsize, f_frsize...).
        Eg., the return value of os.statvfs() is such a thing (since py 2.2).
        If you are not reusing an existing statvfs object, start with
        fuse.StatVFS(), and define the attributes.

        To provide usable information (ie., you want sensible df(1)
        output, you are suggested to specify the following attributes:

            - f_bsize - preferred size of file blocks, in bytes
            - f_frsize - fundamental size of file blcoks, in bytes
                [if you have no idea, use the same as blocksize]
            - f_blocks - total number of blocks in the filesystem
            - f_bfree - number of free blocks
            - f_files - total number of file inodes
            - f_ffree - nunber of free file inodes
        """
        debug('statvfs')
        return os.statvfs(".")

    class XmpFile(object):

        def __init__(self, path, flags, *mode):
            self.file = os.fdopen(os.open("." + path, flags, *mode),
                                  flag2mode(flags))
            self.fd = self.file.fileno()

        def read(self, length, offset):
            self.file.seek(offset)
            return self.file.read(length)

        def write(self, buf, offset):
            self.file.seek(offset)
            self.file.write(buf)
            return len(buf)

        def release(self, flags):
            self.file.close()

        def _fflush(self):
            if 'w' in self.file.mode or 'a' in self.file.mode:
                self.file.flush()

        def fsync(self, isfsyncfile):
            self._fflush()
            if isfsyncfile and hasattr(os, 'fdatasync'):
                os.fdatasync(self.fd)
            else:
                os.fsync(self.fd)

        def flush(self):
            self._fflush()
            # cf. xmp_flush() in fusexmp_fh.c
            os.close(os.dup(self.fd))

        def fgetattr(self):
            return os.fstat(self.fd)

        def ftruncate(self, len):
            self.file.truncate(len)

        def lock(self, cmd, owner, **kw):
            # The code here is much rather just a demonstration of the locking
            # API than something which actually was seen to be useful.

            # Advisory file locking is pretty messy in Unix, and the Python
            # interface to this doesn't make it better.
            # We can't do fcntl(2)/F_GETLK from Python in a platfrom independent
            # way. The following implementation *might* work under Linux. 
            #
            # if cmd == fcntl.F_GETLK:
            #     import struct
            # 
            #     lockdata = struct.pack('hhQQi', kw['l_type'], os.SEEK_SET,
            #                            kw['l_start'], kw['l_len'], kw['l_pid'])
            #     ld2 = fcntl.fcntl(self.fd, fcntl.F_GETLK, lockdata)
            #     flockfields = ('l_type', 'l_whence', 'l_start', 'l_len', 'l_pid')
            #     uld2 = struct.unpack('hhQQi', ld2)
            #     res = {}
            #     for i in xrange(len(uld2)):
            #          res[flockfields[i]] = uld2[i]
            #  
            #     return fuse.Flock(**res)

            # Convert fcntl-ish lock parameters to Python's weird
            # lockf(3)/flock(2) medley locking API...
            op = { fcntl.F_UNLCK : fcntl.LOCK_UN,
                   fcntl.F_RDLCK : fcntl.LOCK_SH,
                   fcntl.F_WRLCK : fcntl.LOCK_EX }[kw['l_type']]
            if cmd == fcntl.F_GETLK:
                return -EOPNOTSUPP
            elif cmd == fcntl.F_SETLK:
                if op != fcntl.LOCK_UN:
                    op |= fcntl.LOCK_NB
            elif cmd == fcntl.F_SETLKW:
                pass
            else:
                return -EINVAL

            fcntl.lockf(self.fd, op, kw['l_start'], kw['l_len'])


    def main(self, *a, **kw):

        #self.file_class = self.XmpFile

        return Fuse.main(self, *a, **kw)


def main():
    server = LibraryFuse()

    #server.parser.add_option(mountopt="root", metavar="PATH", default='/',
    #                         help="mirror filesystem from under PATH [default: %default]")
    server.parse(values=server, errex=1)

    server.main()


if __name__ == '__main__':
    main()
