#!/usr/bin/env python
# -*- coding: UTF-8 -*-
"""
A module which creates a binding for the TRE regular expression engine.
"""

from ctypes import cdll, Structure, pointer, POINTER, byref, ARRAY
# inport the built in datatypes
from ctypes import c_int, c_size_t, c_void_p, c_char_p, c_wchar_p

# constants
REG_NOMATCH = 1

try:
    libtre = cdll.LoadLibrary('libtre.so.4')
except WindowsError, OSError:
    # the unix lib is not available,
    # try the windows one
    libtre = cdll.LoadLibrary('tre4.dll')

regoff_t = c_int

class regex_t(Structure):
    """regex_t stuct"""
    _fields_ = [
        ('re_nsub', c_size_t),
        ('value', c_void_p),
    ]

regex_p = POINTER(regex_t)

class regmatch_t(Structure):
    _fields_ = [
        ('rm_so', regoff_t),
        ('rm_eo', regoff_t),
    ]

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
#libtre.regexec.argtypes = [regex_p, c_char_p, c_size_t, POINTER(regmatch_p), c_int]

# tre_version
libtre.tre_version.restype = c_char_p

if __name__ == '__main__':
    print libtre.tre_version()
