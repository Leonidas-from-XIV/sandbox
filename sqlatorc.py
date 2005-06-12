#!/usr/bin/env python
# -*- encoding: latin-1 -*-
import sys, xmlrpclib

server = xmlrpclib.Server("http://localhost:8080/")

doc = """SQLator remote shell"""

while True:
    query = raw_input('sqlite> ')
    if query == '.quit':
        sys.exit()
    else:
        print server.execute(query)

def main():
    pass

if __name__ == '__main__':
    main()