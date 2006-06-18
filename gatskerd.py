#!/usr/bin/env python
# -*- encoding: latin-1 -*- 
"""Gatskerd - some kind of IRC-Jabber MUC mirror

Dependencies:
Python 2.4
Twisted 2.4
"""

from twisted.words.protocols import irc
from twisted.words.protocols.jabber import client, jid
from twisted.words.xish import domish
from twisted.internet import reactor, protocol