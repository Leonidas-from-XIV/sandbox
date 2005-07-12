#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""SQLatorD - the SQLator daemon - Twisted version of Async
This server is not really part of SQLator, it is a version
just for fun to test twisted."""

import xmlrpclib
from twisted.internet import reactor, protocol
from twisted.protocols import basic
from sqlatord_async import LayerServer
    
class ClientProtocol(basic.LineReceiver):
    def lineReceived(self, data):
        ret = self.factory.storage.execute(data)
        self.transport.write(ret + '\r\n')

class ConnectionFactory(protocol.ServerFactory):
    protocol = ClientProtocol
    storage = LayerServer()

def main():
    reactor.listenTCP(5888, ConnectionFactory())
    reactor.run()

if __name__ == '__main__':
    main()
