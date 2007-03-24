#!/usr/bin/env python
# -*- coding: UTF-8 -*-
"""
A module which creates a binding for the TRE regular expression engine.

Don't be surprised about many very easy and obvious documentation comments
as this module is also intended as a documentation on how to wrap C 
libraries with ctypes. ctypes was put into the stdlib for Python 2.5 so it
does not count as external dependency anymore
"""

# import important ctypes functions
from ctypes import cdll, Structure, pointer, POINTER, byref, ARRAY
# inport the built-in C datatypes
from ctypes import c_int, c_size_t, c_void_p, c_char_p, c_wchar_p

# constants
REG_NOMATCH = 1

try:
    # first try to import the library by it's unixish name
    libtre = cdll.LoadLibrary('libtre.so.4')
except WindowsError, OSError:
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

class CompiledRegex(object):
    """This class represents a compiled regular expression"""
    def __init__(self, pattern, flags):
        pass

if __name__ == '__main__':
    # this module is not meant to run stand-alone, so just display
    # the version of TRE it uses.
    # This can also be seen as a small self-test which shows whether
    # TRE can be called at all
    print libtre.tre_version()
