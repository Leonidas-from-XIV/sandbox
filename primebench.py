#!/usr/bin/env python
# -*- encoding: latin-1 -*- 
"""Prime benchmark - and small library
You can use it in your own programs.

Copyright (C) 2003-2005 Marek 'Leonidas' Kubica <pythonmailing AT web DOT de>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
"""

import sys, time, optparse, math

__version__ = "0.1.4"

def isPrime_mk(number):
    """This is the core def, it checks whether
    the number is a prime or not.
    This is the absolute core, so most tuning is needed here."""
    if number < 2:
        return False
    elif number == 2:
        return True
    for dividor in xrange(2, number):
        if number % dividor == 0:
            return False
    # we got until there, so it must be a prime
    return True

def isPrime_ag(number):
    """Tuned code by Adam Gurno (LGPL)
    http://gurno.com/adam/ taken from mathfun
    Rewritten to fit into primebench"""
    if number % 2 == 0 and not number == 2: 
        return False
    else:
        maxval = int(math.sqrt(number)) + 1
        for count in range(3, maxval, 2):
            if number % count == 0: 
                return False
        return True

isPrime = isPrime_ag

def gennextprime(start):
    """Generates the next prime of that one given.
    Is a generator, so you simply can call next() and
    this will produce the next bigger prime"""
    while True:
        if isPrime(start):
            yield start
        start += 1

def rangeprime(start, end):
    """A generator yielding a range of primes"""
    while True:
        if start > end:
            raise StopIteration
        if isPrime(start):
            yield start
        start += 1

def findNumberOfPrimes(primes_to_find):
    """Generates a lot of primes"""
    gen = gennextprime(1)
    for i in range(primes_to_find):
        print "Prime==%d" % gen.next()

def findPrimesUntil(until_number):
    """Finds primes until number N is reached"""
    return rangeprime(2, until_number)

def main():
    """The main def, which makes this script executable"""
    parser = optparse.OptionParser()
    
    parser.add_option("-v", "--version",
        dest="version",
        default=False,
        action="store_true", 
        help="print program versions and exit")
    parser.add_option("-b", "--benchmark",
        dest="benchmark", 
        default=False,
        action="store_true", 
        help="benchmark results")
    parser.add_option("-n", "--next",
        dest="next", 
        default=False, 
        metavar="NUMBER",
        type="int",
        action="store", help="get next prime which is bigger than NUMBER")
    parser.add_option("-t", "--times", 
        dest="times", 
        default=False, 
        metavar="NUMBER",
        type="int",
        action="store", 
        help="find NUMBER primes")
    parser.add_option("-u", "--until", 
        dest="until", 
        default=False, 
        metavar="NUMBER",
        type="int",
        action="store", help="find primes until NUMBER")
    parser.add_option("-c", "--check", 
        dest="check", 
        default=False, 
        metavar="NUMBER",
        type="int",
        action="store", 
        help="check whether NUMBER is a prime")
    
    (options, args) = parser.parse_args()
    
    if options.version:
        print 'primebench.py (%s) - Small benchmark with primes (Free Software - GPLed)' % __version__
        sys.exit(0)
        
    if options.benchmark:
        print 'Benchmark enabled'
        
    if options.next:
        if options.benchmark:
            print "There is too less to benchmark"
        prime_found = gennextprime(options.next + 1).next()
        print "Prime==%d" % prime_found
        sys.exit(0)
    
    if options.times:
        if options.benchmark: 
            startTime = time.time()
        primes = findNumberOfPrimes(options.times)
        if options.benchmark:
            stopTime = time.time()
            taken = stopTime - startTime
        if options.benchmark: 
            print "Seconds==%G" % taken
    
    if options.until:
        if options.benchmark: 
            startTime = time.time()
        primes = findPrimesUntil(options.until)
        if options.benchmark:
            stopTime = time.time()
            taken = stopTime - startTime
        for prime in primes:
            print "Prime==%d" % prime
        if options.benchmark: 
            print "Seconds==%G" % taken
    
    if options.check:
        if options.benchmark:
            print "There is too less to benchmark"
        result = isPrime(options.check)
        if result == True:
            print "Prime==True"
        else:
            print "Prime==False"
        
def longxrange(start,stop=None,step=1):
    """myxrange([start=0],stop,[step=1]) --> iterator object like xrange for longs
    coded by Milan:
    http://www.python-forum.de/viewtopic.php?t=1366"""
    if stop == None:
        stop = start
        start = 0
    if step > 0:
        while start < stop:
            yield start
            start += step
    elif step < 0:
        while start > stop:
            yield start
            start += step
    else:
        raise ValueError, "step must be != 0"
            
if __name__ == '__main__':
    main()
