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

class IRCSide(irc.IRCClient):
    """A gateway bot"""
    
    def connectionMade(self):
        self.nickname = self.factory.nick
        irc.IRCClient.connectionMade(self)

    def signedOn(self):
        """Called when bot has succesfully signed on to server."""
        print 'joining'
        self.join(self.factory.channel)

    def privmsg(self, user, channel, msg):
        """This will get called when the bot receives a message."""
        user = user.split('!', 1)[0]
        print user, channel, msg
        
        # Check to see if they're sending me a private message
        if channel == self.nickname:
            msg = "Sorry Sai, I'm just a mirror bot."
            self.msg(user, msg)
            return
        
        self.msg(channel, '<%s> %s' % (user, msg))
    
    def noticed(self, user, channel, message):
        pass

    def action(self, user, channel, msg):
        """This will get called when the bot sees someone do an action."""
        user = user.split('!', 1)[0]

    def irc_NICK(self, prefix, params):
        """Called when an IRC user changes their nickname."""
        old_nick = prefix.split('!')[0]
        new_nick = params[0]

class IRCMirrorFactory(protocol.ClientFactory):
    # the class of the protocol to build when new connection is made
    protocol = IRCSide

    def __init__(self, nick, channel):
        self.channel = channel
        self.nick = nick

    def clientConnectionLost(self, connector, reason):
        """If we get disconnected, reconnect to server."""
        connector.connect()

    def clientConnectionFailed(self, connector, reason):
        print "connection failed:", reason
        reactor.stop()

def main():
    # create factory protocol and application
    irc_factory = IRCMirrorFactory('notwist24', '#nosuchchannel')

    # connect factory to this host and port
    reactor.connectTCP("irc.freenode.net", 6667, irc_factory)

    # run bot
    reactor.run()

if __name__ == '__main__':
    main()