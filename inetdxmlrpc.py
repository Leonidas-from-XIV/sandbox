#!/usr/bin/env python2.4
# -*- encoding: latin-1 -*-
import sys, xmlrpclib

def sumAndDifference(a, b):
    return a + b

funcs = {"sumAndDifference": sumAndDifference}

def inetdcall():
    while True:
        line = sys.stdin.readline().splitlines()[0]
        if "Content-Length:" in line:
            cl = line.split()[1]
            cl = int(cl)
            sys.stdin.readline()
            break 
    
    request = sys.stdin.read(cl)
    
    params, method = xmlrpclib.loads(request)
    
    result = funcs[method](*params)
    
    response = xmlrpclib.dumps((result,), methodresponse=True)
    sys.stdout.write(response)

if __name__ == '__main__':
    inetdcall()