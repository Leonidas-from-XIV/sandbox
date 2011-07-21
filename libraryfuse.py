#!/usr/bin/env python2
# Copyright (C) 2001 Jeff Epler <jepler@unpythonic.dhs.org>
# Copyright (C) 2006 Csaba Henk <csaba.henk@creo.hu>
# Copyright (C) 2011 Marek Kubica <marek@xivilization.net>
#
# This program can be distributed under the terms of the GNU LGPLv3.

import os, sys
from errno import *
from stat import *
import fcntl
import fuse

from fuse import Fuse
import os.path
import errno
from logbook import FileHandler, debug, DEBUG
log_handler = FileHandler('/tmp/libraryfuse.log', level=DEBUG)
log_handler.push_application()

debug('Starting')

fuse.fuse_python_api = (0, 2)
fuse.feature_assert('stateful_files', 'has_init')

directories_to_merge = ['/var', '/usr']

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
                return os.lstat(real_path)

    def readlink(self, path):
        debug('readlink called with {}'.format(path))
        for library_part in self.directories_to_merge:
            real_path = library_part + path
            if os.path.exists(real_path):
                return os.readlink(real_path)

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
        debug('access {0} in mode {1}'.format(path, mode))
        for library_part in self.directories_to_merge:
            real_path = library_part + path
            if not os.access(real_path, mode):
                return -errno.EACCESS

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

    def main(self, *a, **kw):
        return Fuse.main(self, *a, **kw)

def main():
    server = LibraryFuse()

    #server.parser.add_option(mountopt="root", metavar="PATH", default='/',
    #                         help="mirror filesystem from under PATH [default: %default]")
    server.parse(values=server, errex=1)

    server.main()


if __name__ == '__main__':
    main()
