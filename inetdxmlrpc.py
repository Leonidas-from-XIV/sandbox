#!/usr/bin/env python2.4
# -*- encoding: latin-1 -*-
"""A small XML-RPC Server running under control
of the internet superserver inetd.

Configuring:
Add this line to your inetd.conf
embedxmlrpc     stream  tcp     nowait  user   /usr/sbin/tcpd  inetdxmlrpc.py
Where user is the user to execute the script and 
inetdxmlprc.py the path to the script.

and this line to your services.conf
embedxmlrpc     7373/tcp                        # standalone XML-RPC server
there 7373 will be the port

You have to restart your inetd.
"""
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