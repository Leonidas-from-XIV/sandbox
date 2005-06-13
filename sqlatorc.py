#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""SQLatorC - the SQLator client
A client for the SQLator Daemon, for using SQLite over network"""

import sys, xmlrpclib, optparse, textwrap

#defaultport = 8080
server = xmlrpclib.Server("http://localhost:8080/")

intro = """SQLator remote shell
Enter ".help" for instructions"""

def dotparse(command):
    if command == 'quit':
        sys.exit()
    elif command == 'help':
        help()
    else:
        print 'No such command'

def help():
    helptext = {'.help' : 'Show this message', 
        '.quit' : 'Exit this program'}
    for key in helptext.keys():
        print key, helptext[key]
    #print textwrap.wrap(helptext)

def main():
    print intro
    while True:
        query = raw_input('sqlator> ')
        if query.startswith('.'):
            # parse, without the dot
            dotparse(query[1:])
        else:
            print server.execute(query)


if __name__ == '__main__':
    main()