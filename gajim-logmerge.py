#!/usr/bin/env python

import sys
from sqlalchemy import create_engine, MetaData, Table, Column, ForeignKey
from sqlalchemy.orm import mapper, sessionmaker, relationship, backref
from sqlalchemy.orm.exc import NoResultFound

# copied from Gajim common.logger
JID_NORMAL_TYPE = 0
JID_ROOM_TYPE = 1

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
    def __init__(self, jid):
        self.jid = jid
        self.type = JID_NORMAL

class SourceJID(JID):
    pass

class DestinationJID(JID):
    pass

class Message(object):
    def __init__(self, jid, time, message, kind):
        self.time = time
        self.message = message
        self.jid = jid
        self.contact_name = ''
        self.kind = kind
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

    # exclude conference messages
    if message_src.jid.type != JID_NORMAL_TYPE:
        print('rooms not supported')
        continue

    try:
        sender_dst = session_dst.query(DestinationJID).filter_by(jid=message_src.jid.jid+"def").one()
    except NoResultFound:
        print("no such jid, creating")
        sender_dst = DestinationJID(message_src.jid.jid+"def")
        session_dst.add(sender_dst)

    print(sender_dst)

    message_dst = DestinationMessage(jid=sender_dst, time=message_src.time, message=message_src.message, kind=message_src.kind)
    print(message_dst)

    duplicates = session_dst.query(DestinationMessage).\
            filter_by(message=message_src.message).\
            filter_by(time=message_src.time)
    if duplicates.all():
        print('Message already in DB, skipping')
        #continue


    session_dst.add(message_dst)
    print(session_dst.new)
    #session_dst.commit()
