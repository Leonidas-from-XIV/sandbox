#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""The SQLite backend - a pyro server without nameserver
"""

import Pyro.core
from pysqlite2 import dbapi2 as sqlite

class SQLiteBackbone(Pyro.core.ObjBase):
    def __init__(self):
        Pyro.core.ObjBase.__init__(self)
        self.dbfile = 'achromatic.sqlite'
        
    def execute(self, sql):
        """This opens the database and executes the actions.
        The problem is, that this could be slow, but there's 
        no other way, as PySQLite just works from the same
        thead where the database was opened."""
        con = sqlite.connect(self.dbfile)
        cur = con.cursor()
        cur.execute(sql)
        #con.commit()
        return cur.fetchall()

class LayerServer(object):
    """This is just a small layer between the servers to the 'outside'
    like XML-RPC or other ways like asyncore and the Pyro server.
    This layer should be imported in servers communicating to the
    outside world, it is not used in the backbone."""
    
    def __init__(self):
        """Find and connect to the Pyro server. You can find a
        reference to the pyro-storage in self.storage."""
        Pyro.core.initClient(banner=False)
        uri = 'PYROLOC://localhost:7766/Storage'
        self.storage = Pyro.core.getProxyForURI(uri)
    
    def execute(self, sql):
        """Execute the SQL statement and return the results"""
        ret = self.storage.execute(sql)
        return ret

def main():
    Pyro.core.initServer(banner=False)
    print 'SQLator Backbone initialized. Using Pyro connections.'
    daemon = Pyro.core.Daemon()
    objectName = 'Storage'
    uri = daemon.connect(SQLiteBackbone(), objectName)
    daemon.requestLoop()

if __name__ == '__main__':
    main()