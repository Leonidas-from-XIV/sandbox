#!/usr/bin/env python
# -*- encoding: latin-1 -*-
from SimpleXMLRPCServer import SimpleXMLRPCServer
from pysqlite2 import dbapi2 as sqlite

#dbfile = 'sqlator.sqlite'
dbfile = 'achromatic.sqlite'

class SQLServer(object):
    def __init__(self):
        self.con = sqlite.connect(dbfile)
        self.cur = self.con.cursor()
        
    def execute(self, sql):
        self.cur.execute(sql)
        return self.cur.fetchall()

def main():
    sqls = SQLServer()
    server = SimpleXMLRPCServer(("localhost", 8080))
    server.register_function(sqls.execute)
    server.serve_forever()

if __name__ == '__main__':
    main()