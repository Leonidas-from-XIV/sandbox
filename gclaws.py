#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Script that logins to Google, grabs the contacts and returns an XML file
suitable for Claws Mail

(Unfortunately, in my setup, some contacts are doubled, I suppose something
fucked up)"""

import getpass
import random, itertools
from lxml.builder import ElementMaker
from lxml.etree import tostring
import gdata.contacts.client

def get_contacts(email, password):
    client = gdata.contacts.client.ContactsClient(source='xivilization-gclaws-0')
    client.ClientLogin(email, password, client.source)
    query = gdata.contacts.client.ContactsQuery()
    # GAH! why do I need such nonsense? isn't there an unlimited way?
    query.max_results = 1000
    feed = client.GetContacts(q=query)

    for entry in feed.entry:
        if entry.name.given_name is not None:
            first_name = entry.name.given_name.text
        else:
            first_name = ''

        if entry.name.family_name is not None:
            last_name = entry.name.family_name.text
        else:
            last_name = ''

        cn = entry.name.full_name.text
        mails = [mail.address for mail in entry.email]

        yield {
                'first-name' : first_name,
                'last-name' : last_name,
                'cn' : cn,
                'address-list' : mails
            }

def get_uid():
    unique = uuid.uuid4()
    resized = unique.int % (10**8)
    return str(int(resized))

def construct_abook(contacts):
    e = ElementMaker()
    people = []
    for contact in contacts:
        # skip non-email
        if not contact['address-list']:
            continue

        adresses = []
        for mail in contact['address-list']:
            adresses.append(e.address(alias='', remarks='', uid=get_uid(),
                email=mail))

        person = e.person(getattr(e, 'address-list')(*adresses),
                getattr(e, 'attribute-list')(),
                **{
                    'uid' : get_uid(),
                    'first-name' : contact['first-name'],
                    'last-name' : contact['last-name'],
                    'cn' : contact['cn']
                })
        people.append(person)

    abook = getattr(e, 'address-book')(name="Gmail", *people)
    return abook

email = raw_input("Enter Google Account: ")
password = getpass.getpass("Enter account password: ")
abook = construct_abook(get_contacts(email, password))
print """<?xml version="1.0" encoding="UTF-8" ?>"""
print tostring(abook, pretty_print=True)
