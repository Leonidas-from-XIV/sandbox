#!/usr/bin/env python
# -*- coding: UTF-8 -*-
"""
A module which creates a binding for the TRE regular expression engine.
You can find the home page of TRE at http://laurikari.net/tre/

Don't be surprised about many very easy and obvious documentation comments
as this module is also intended as a documentation on how to wrap C 
libraries with ctypes. ctypes was put into the stdlib for Python 2.5 so it
does not count as external dependency anymore.
"""

# import important ctypes functions
from ctypes import cdll, Structure, pointer, POINTER, byref, ARRAY
# inport the built-in C datatypes
from ctypes import c_int, c_size_t, c_void_p, c_char_p, c_wchar_p

# constants
REG_NOMATCH = 1
REG_EXTENDED = 1

try:
    # first try to import the library by it's unixish name
    libtre = cdll.LoadLibrary('libtre.so.4')
except (WindowsError, OSError):
    # the unix lib is not available,
    # try the windows one
    libtre = cdll.LoadLibrary('tre4.dll')

# create the custom types needed for TRE
# not all types are really custom, TRE uses a lot of standard C types
# but gives them only new names

# a regoff_t is just an ordinary c_int
regoff_t = c_int

class regex_t(Structure):
    """This is the regex_t structure as defined by TRE.
    The exact field information was taken from the header files
    of TRE - it was found in regex.h"""
    _fields_ = [
        ('re_nsub', c_size_t),
        ('value', c_void_p),
    ]

# define a pointer type to regex_t structure
regex_p = POINTER(regex_t)

class regmatch_t(Structure):
    """A regmatch_t structure"""
    _fields_ = [
        ('rm_so', regoff_t),
        ('rm_eo', regoff_t),
    ]

# a pointer type to the regmatch_t structure. This ist just the
# same thing as needed for the regex_p type
regmatch_p = POINTER(regmatch_t)

# function definitions

# the regcomp() functions
libtre.regcomp.argtypes = [regex_p, c_char_p, c_int]
libtre.regcomp.restype = c_int
libtre.regncomp.argtypes = [regex_p, c_char_p, c_size_t, c_int]
libtre.regwcomp.argtypes = [regex_p, c_wchar_p, c_int]
libtre.regwncomp.argtypes = [regex_p, c_wchar_p, c_size_t, c_int]

libtre.regfree.restype = None
libtre.regfree.argtypes = [regex_p]

# regexec() functions
# whereas the 4th argument should be an array there seems to be no 
# possibility yet to create an array type with variable length fields
libtre.regexec.argtypes = [regex_p, c_char_p, c_size_t, c_void_p, c_int]

# tre_version()
libtre.tre_version.argtypes = []
libtre.tre_version.restype = c_char_p

class TREPattern(object):
    """
    This class represents a compiled regular expression
    """
    def __init__(self, pattern, flags=0):
        """
        Constructor - see, the signature is the same as of re.compile
        that can be very useful to retain API compatibility.
        Note, the flags aren't yet implemented - REG_EXTENDED is used
        for everything instead.
        """
        # the real compiled regex - a regex_t instance
        self.preg = byref(regex_t())
        
        result = libtre.regcomp(self.preg, pattern, REG_EXTENDED)
        if result != 0:
            raise Exception('Parse error, code %s' % result)
        
        # how much memory to reserve
        # refer to the re_nsub field of the regex_t
        self.match_buffers = self.preg._obj.re_nsub + 1

    def findall(self, string, pos=None, endpos=None):
        """
        Finds all non overlapping matches of...
        pos and endpos are not implemented yet, just
        provided to be compatible with sre
        """
        pmatch = (regmatch_t * self.match_buffers)()
        nmatch = c_size_t(self.match_buffers)
        
        result = libtre.regexec(self.preg, string, nmatch, byref(pmatch), 0)
        if result != 0:
            raise Exception('Exec error, status %s' % result)
        
        matches = list()
        for match in pmatch:
            match_offsets = (match.rm_so, match.rm_eo)
            chunk = string[match.rm_so:match.rm_eo]
            matches.append(chunk)
        return matches
            

# convenient, isn't it?
compile = TREPattern

if __name__ == '__main__':
    # this module is not meant to run stand-alone, so just display
    # the version of TRE it uses.
    # This can also be seen as a small self-test which shows whether
    # TRE can be called at all
    print libtre.tre_version()
