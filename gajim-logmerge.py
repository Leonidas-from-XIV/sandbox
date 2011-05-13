#!/usr/bin/env python

import sys
from sqlalchemy import create_engine, MetaData, Table, Column, ForeignKey
from sqlalchemy.orm import mapper, sessionmaker, relationship, backref

meta_src = MetaData()
meta_dst = MetaData()

source = create_engine('sqlite:///{}'.format(sys.argv[1]))
destination = create_engine('sqlite:///{}'.format(sys.argv[2]))

logs_src = Table('logs', meta_src,
        Column('jid_id', ForeignKey('jids.jid_id')),
        autoload=True, autoload_with=source)
jids_src = Table('jids', meta_src, autoload=True, autoload_with=source)
logs_dst = Table('logs', meta_dst,
        Column('jid_id', ForeignKey('jids.jid_id')),
        autoload=True, autoload_with=source)
jids_dst = Table('jids', meta_dst, autoload=True, autoload_with=source)

class JID(object):
    pass

class SourceJID(JID):
    pass

class DestinationJID(JID):
    pass

class Message(object):
    pass

class SourceMessage(Message):
    pass

class DestinationMessage(Message):
    pass

mapper(SourceJID, jids_src)
mapper(SourceMessage, logs_src, properties={
    'jid' : relationship(SourceJID)
})
mapper(DestinationJID, jids_dst)
mapper(DestinationMessage, logs_dst, properties={
    'jid' : relationship(DestinationJID)
})

SourceSession = sessionmaker(bind=source)
DestinationSession = sessionmaker(bind=destination)

session_src = SourceSession()
session_dst = DestinationSession()

for message in session_src.query(SourceMessage).limit(1):
    print(dir(message))
    print(message.message)
    print(message.time)
    print(message.jid_id)
    print(message.jid)
    print(dir(message.jid))
    print(message.jid.jid)
