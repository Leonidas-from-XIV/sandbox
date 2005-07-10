#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""SQLatorD - the SQLator daemon
A XML-RPC server for the SQLite database"""
from sqlatorb import LayerServer
from SimpleXMLRPCServer import SimpleXMLRPCServer
import Pyro.core

def main():
    print 'Booting:'
    print ' - Creating a gateway to the backbone'
    sqls = LayerServer()
    print ' - Creating a XML-RPC server on port 8080'
    server = SimpleXMLRPCServer(('', 8080))
    print ' - Registering functions'
    server.register_function(sqls.execute)
    print ' - Serving: Booting done.'
    server.serve_forever()

if __name__ == '__main__':
    main()