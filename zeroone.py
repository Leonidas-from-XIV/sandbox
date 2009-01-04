#!/usr/bin/env python
# A program that checks occurences of 01 in binary words 
import sys

def gen_bitpattern(n, start=''):
    """Generates a bit pattern like 010001 etc."""
    if not n:
        return [start]
    else:
        return (gen_bitpattern(n - 1, start + '0') +
                gen_bitpattern(n - 1, start + '1'))

def is_valid(bitpattern):
    """Predicate: Checks whether 01 occurs exactly 3 times"""
    return bitpattern.count('01') == 3

try:
    n = int(sys.argv[1])
except (IndexError, ValueError):
    print >>sys.stderr, 'Specify a number n on the command line'
else:
    # 1 for every valid pattern of get_bitpattern(n), sum all ones up
    found = sum(1 for pattern in gen_bitpattern(n) if is_valid(pattern))
    print 'Found %d numbers of length %d where 01 occurs three times.' % (
            found, n)
