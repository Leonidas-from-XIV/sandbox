#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""SQLatorC - the SQLator client
A client for the SQLator Daemon, for using SQLite over network"""

import sys, xmlrpclib, optparse, textwrap, socket

intro = """SQLator remote shell (XML-RPC and async)
Enter ".help" for instructions"""

class PseudoXMLRPCClient(object):
    """This class represents a remote object.
    Just needed for async-connections."""
    def __init__(self, host, port):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect((host, port))
    
    def execute(self, query):
        line = xmlrpclib.dumps((query, ))
        self.sock.sendall(line + '\r\n')
        
        response = ''
        while True:
            data = self.sock.recv(512)
            response += data
            if len(data) < 512:
                break
            
        params, methodname = xmlrpclib.loads(response)
        return params

class SQLClient(object):
    def __init__(self, method, host):
        if method == 'xmlrpc':
            port = 8080
            uri = 'http://%s:%d/' % (host, port)
        
            self.server = xmlrpclib.Server(uri)
        elif method == 'async':
            self.server = PseudoXMLRPCClient(host, 5888)

    def dotparse(self, command):
        if command == 'quit':
            sys.exit()
        elif command == 'help':
            self.help()
        else:
            print 'No such command'

    def help(self):
        helptext = {'.help' : 'Show this message', 
            '.quit' : 'Exit this program'}
        for key in helptext.keys():
            print key, helptext[key]
        #print textwrap.wrap(helptext)

    def main(self):
        print intro
        while True:
            query = raw_input('sqlator> ')
            if query.startswith('.'):
                # The user wanted some internal sqlatorc function
                # parse, without the dot
                self.dotparse(query[1:])
            else:
                print self.server.execute(query)


if __name__ == '__main__':
    method = 'async' # or xmlrpc
    SQLClient(method=method, host='localhost').main()