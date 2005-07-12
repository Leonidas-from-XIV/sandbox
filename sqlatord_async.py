#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""SQLatorD - the SQLator daemon
2005-07-10: A more advanced event driven XMP-RPC server by Leonidas
2005-06-24: A quick dirty TCP-Socket server for SQLite by Tabellar
2005-06-13: A XML-RPC server for the SQLite database by Leonidas
"""

import socket, asynchat, asyncore, xmlrpclib
from sqlatorb import LayerServer as GenericLayer

class LayerServer(GenericLayer):
    def execute(self, query):
        """Gets the query (XML-RPC request),
        decodes it to SQL, executes it, encodes the result
        into XML-RPC and returns it"""
        query = self.rpc_decode(query)
        result = self.storage.execute(query)
        return self.rpc_encode(result)
    
    def rpc_encode(self, data):
        return xmlrpclib.dumps((data,), methodresponse=True)
    
    def rpc_decode(self, data):
        params, methodname = xmlrpclib.loads(data)
        return params[0]
    

class ConnectionFactory(asyncore.dispatcher):
    def __init__(self):
        print 'Booting:'
        self.port = 5888
        print ' - Creating server instance'
        self.server = LayerServer()
        print ' - Calling dispatcher'
        asyncore.dispatcher.__init__(self)
        print ' - Creating socket'
        self.create_socket(socket.AF_INET, socket.SOCK_STREAM)
        print ' - Binding socket to port'
        self.bind(('', self.port))
        print ' - Listening: Booting done.'
        self.listen(5)
    
    def handle_accept(self):
        newSocket, address = self.accept()
        print 'Connected by %s:%d' % address
        ClientProtocol(socket=newSocket, server=self.server)


class ClientProtocol(asynchat.async_chat):
    def __init__(self, socket, server):
        print 'Connection delegated'
        self.server = server
        asynchat.async_chat.__init__(self, socket)
        self.set_terminator('\r\n')
        self.data = ""
    
    def collect_incoming_data(self, data):
        """Collects data got through the socket"""
        self.data += data

    def found_terminator(self):
        print self.data
        result = self.server.execute(self.data)
        self.push(result)
        #self.push(''.join(self.data))
        self.data = ""


def main():
    ConnectionFactory()
    asyncore.loop()

if __name__ == '__main__':
    main()
