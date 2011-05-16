#!/usr/bin/env python

import sys
import argparse
from sqlalchemy import create_engine, MetaData, Table, Column, ForeignKey, func
from sqlalchemy.orm import mapper, sessionmaker, relationship, backref
from sqlalchemy.orm.exc import NoResultFound
from sqlalchemy.sql import exists

# copied from Gajim common.logger
JID_NORMAL_TYPE = 0
JID_ROOM_TYPE = 1

class JID(object):
    def __init__(self, jid):
        self.jid = jid
        self.type = JID_NORMAL_TYPE

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

def print_state(state):
    print(state, end='', file=sys.stderr)
    sys.stderr.flush()

def get_arguments():
    parser = argparse.ArgumentParser(description="Merges Gajim chat history")
    parser.add_argument('--source', '-s', action='store', required=True)
    parser.add_argument('--destination', '-d', action='store', required=True)
    return parser.parse_args()

def main():
    args = get_arguments()

    meta_src = MetaData()
    meta_dst = MetaData()

    source = create_engine('sqlite:///{}'.format(args.source))
    destination = create_engine('sqlite:///{}'.format(args.destination))

    logs_src = Table('logs', meta_src,
            Column('jid_id', ForeignKey('jids.jid_id')),
            autoload=True, autoload_with=source)
    jids_src = Table('jids', meta_src, autoload=True, autoload_with=source)
    logs_dst = Table('logs', meta_dst,
            Column('jid_id', ForeignKey('jids.jid_id')),
            autoload=True, autoload_with=destination)
    jids_dst = Table('jids', meta_dst, autoload=True, autoload_with=destination)

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

    # cache the known JIDs
    jid_cache = {}

    items = session_src.query(SourceMessage)
    print("Processing {} message(s)".format(items.count()))
    for message_src in items.all():
        # exclude conference messages
        if message_src.jid.type != JID_NORMAL_TYPE:
            print_state('U')
            continue

        if message_src.jid not in jid_cache:
            try:
                sender_dst = session_dst.query(DestinationJID).\
                        filter_by(jid=message_src.jid.jid).one()
            except NoResultFound:
                # no such sender in DB, create a new one
                sender_dst = DestinationJID(message_src.jid.jid)
                session_dst.add(sender_dst)
            jid_cache[message_src.jid] = sender_dst
        else:
            sender_dst = jid_cache[message_src.jid]

        # check whether such a message already exists in the DB
        #dup = exists().where(DestinationMessage.message == message_src.message).\
        #        where(DestinationMessage.time == message_src.time)

        #duplicates = session_dst.query(DestinationMessage).\
        #        filter(dup)
        #        filter_by(message=message_src.message).\
        #        filter_by(time=message_src.time)
        duplicates = session_dst.query(func.count(DestinationMessage.log_line_id)).\
                filter_by(message=message_src.message).\
                filter_by(time=message_src.time)

        if duplicates.one()[0] != 0:
            # we found a duplicate, skip it
            print_state('S')
            continue

        message_dst = DestinationMessage(jid=sender_dst, time=message_src.time,
                message=message_src.message, kind=message_src.kind)

        # all checks went OK, add the message to the DB
        session_dst.add(message_dst)
        print_state('.')
        session_dst.commit()

    # new-line on exit
    print('', file=sys.stderr)

if __name__ == '__main__':
    main()
