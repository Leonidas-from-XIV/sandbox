#!/usr/bin/env python
# -*- encoding: latin-1 -*- 
"""Prime benchmark - and small library
You can use it in your own programs"""
# Copyright (C) 2003-2005  Leonidas <leonidas AT projectdream DOT org>

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

import sys, time

__version__ = "0.1.0"
args = """bench.py (%s) - Small benchmark with primes (Free Software - GPLed)
Commandline arguments:
bench.py [-n<number>|-t<number>|-u<number>|-i<number>] [-b|--benchmark]
  -n<number> : get next prime which is bigger than <number>
  -t<number> : find <number> primes
  -u<number> : find primes until <number>
  -i<number> : check whether <number> is a prime
  -b --benchmark : benchmark results"""

def checkPrime(number):
    """This is the core def, it checks whether
    the number is a prime or not."""
    prime_found = False
    # Some hacks, to return right values
    if number == 0:
        return False
    if number == 2:
        return True
    if number < 0:
        # If number is smaller than zero so use absolute value
        number = abs(number)
    for dividor in xrange(2, number):
        if number % dividor is 0:
            #print "No Prime: %d" % lastprime
            prime_found = False
            return False
        else:
            prime_found = True
    if prime_found is True:
        #print "Prime: %d" % lastprime
        return True
        
def nextPrime(prime_to_test):
    """Returns the next prime"""
    while True:
        prime_to_test = prime_to_test + 1
        #print "ptt==%d" % prime_to_test
        if checkPrime(prime_to_test):
            # Found prime
            #print "Prime==%d" % prime_to_test
            return prime_to_test
            #break
            
def findNumberOfPrimes(primes_to_find):
    """Returns a list of primes
    You have to wait long time without output
    cause it produces no output to stdout
    ToDo: make a starting value"""
    primes_found = 0
    lastnumber = 2
    primes_list = []
    while True:
        if checkPrime(lastnumber):
            primes_list.append(lastnumber)
            primes_found = primes_found + 1
        if primes_found == primes_to_find:
            return primes_list
        lastnumber = lastnumber + 1

def findPrimesUntil(until_number):
    """Finds primes until number N is reached"""
    primes_list = []
    for number in xrange(2, until_number):
        if checkPrime(number):
            primes_list.append(number)
    return primes_list

def main():
    """The main def, which makes this script executable
    PyLint: this def has to many branches, but what to do?"""
    benchmark = False
    if len(sys.argv) == 1: 
        # if no arguments display help
        print args % __version__
        # and exit
        sys.exit(1)
    elif len(sys.argv) == 3:
        # benchmarking on?
        if sys.argv[2][:2] == "-b" or sys.argv[2][:11] == "--benchmark":
            print "Benchmark enabled"
            benchmark = True
        else:
            print "There is no such commandline argument!"
    if sys.argv[1][:2] == "-n":
        # Simply get the next prime
        if benchmark:
            print "There is too less to benchmark"
        prime_to_test = int(sys.argv[1][2:])
        prime_found = nextPrime(prime_to_test)
        print "Prime==%d" % prime_found
    elif sys.argv[1][:2] == "-t":
        until_prime = int(sys.argv[1][2:])
        if benchmark: 
            startTime = time.time()
        primes = findNumberOfPrimes(until_prime)
        if benchmark:
            stopTime = time.time()
            taken = stopTime - startTime
        for prime in primes:
            print "Prime==%d" % prime
        if benchmark: 
            print "Seconds==%G" % taken

    elif sys.argv[1][:2] == "-u":
        until_number = int(sys.argv[1][2:])
        if benchmark: 
            startTime = time.time()
        primes = findPrimesUntil(until_number)
        if benchmark:
            stopTime = time.time()
            taken = stopTime - startTime
        for prime in primes:
            print "Prime==%d" % prime
        if benchmark: 
            print "Seconds==%G" % taken
            
    elif sys.argv[1][:2] == "-i":
        prime_to_check = int(sys.argv[1][2:])
        if benchmark:
            print "There is too less to benchmark"
        result = checkPrime(prime_to_check)
        if result == True:
            print "Prime==True"
        else:
            print "Prime==False"
        
def longxrange(start,stop=None,step=1):
    """myxrange([start=0],stop,[step=1]) --> iterator object like xrange for longs
    coded by Milan:
    http://python.sandtner.org/viewtopic.php?t=1366"""
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