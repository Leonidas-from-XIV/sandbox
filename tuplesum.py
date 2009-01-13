#!/usr/bin/env python
# A program that checks for the sum
import itertools
from optparse import OptionParser

parser = OptionParser()
parser.add_option('-r', '--range', dest='max_value',
        help='specify numbers from 0..RANGE', metavar='RANGE',
        type='int', default=5)
parser.add_option('-l', '--length', dest='length',
        help='length of tuple', metavar='LENGTH',
        type='int', default=5)
parser.add_option('-s', '--sum', dest='sum',
        help='the desired sum that the numbers should add up to',
        metavar='SUM', type='int', default=21)

options, args = parser.parse_args()

matching = sum(1 for variation in itertools.product(
                xrange(options.max_value + 1), repeat=options.length)
            if sum(variation) == options.sum)

print "Found %d matching sums (=%d) of %d-tuples consisting of \
numbers between 0 and %d" % (matching, options.sum, options.length,
        options.max_value)
