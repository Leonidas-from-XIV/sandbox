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
    def __init__(self, jid, time, message):
        self.time = time
        self.message = message
        self.jid = jid
        self.contact_name = ''
        self.kind = 4
        self.subject = ''

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

for message_src in session_src.query(SourceMessage).limit(1):
    print(dir(message_src))
    print(message_src.message)
    print(message_src.time)
    print(message_src.jid_id)
    print(message_src.jid)
    print(dir(message_src.jid))
    print(message_src.jid.jid)

    # TODO: create JID if it doesn't exist
    sender_dst = session_dst.query(DestinationJID).filter_by(jid=message_src.jid.jid).one()
    print(sender_dst)

    message_dst = DestinationMessage(jid=sender_dst, time=message_src.time, message=message_src.message)
    # TODO: check whether message does not exist already in destination DB
    print(message_dst)
    session_dst.add(message_dst)
