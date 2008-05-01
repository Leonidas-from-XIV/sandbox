#!/usr/bin/env python
# -*- encoding: latin-1 -*- 
"""Gatskerd - some kind of IRC-Jabber MUC mirror

Dependencies:
Python 2.4
Twisted 2.4
"""
from optparse import OptionParser

from twisted.internet import reactor, protocol
from twisted.words.protocols import irc
from twisted.words.protocols.jabber import client, jid
from twisted.words.xish import domish

class IRCSide(irc.IRCClient):
    """A gateway bot"""
    
    def connectionMade(self):
        self.nickname = self.factory.nick
        irc.IRCClient.connectionMade(self)

    def signedOn(self):
        """Called when bot has succesfully signed on to server."""
        #print 'joining'
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
        """Called on notices - silences all notices"""
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

class JabberSide(object):

    def __init__(self, own_jid, nick, channel):
        self.jid = own_jid
        self.nick = nick
        self.channel = channel
        self.xmlstream = None
    
    def authd(self, xmlstream):
        """Called when the JID is authentificated to the server"""
        self.xmlstream = xmlstream
        presence = domish.Element(('jabber:client','presence'))
        self.xmlstream.send(presence)
        
        xmlstream.addObserver('/message', self.got_message)
        xmlstream.addObserver('/presence', self.got_presence)
        xmlstream.addObserver('/iq', self.debug)
        
        reactor.callLater(5, self.join_groupchat)

    def send_message(self, to, text, msg_type='chat'):
        #print 'sending message'
        message = domish.Element(('jabber:client','message'))
        message['to'] = to
        message['type'] = msg_type
        
        #message.addElement('subject', None, subject)
        message.addElement('body', None, text)
        
        self.xmlstream.send(message)
    
    def join_groupchat(self):#, nick, channel):
        #self.nick =  nick
        #self.channel = channel
        presence = domish.Element(('','presence'))
        presence.addElement(('http://jabber.org/protocol/muc', 'x'))
        #presence.x.addElement(('', 'history'))
        #presence.x.history['maxstanzas'] = '0'
        presence['from'] = self.jid.full()
        presence['to'] = '%s/%s' % (self.channel, self.nick)
        #print presence.toXml()
        self.xmlstream.send(presence)
    
    def got_presence(self, presence):
        self.debug(presence)
    
    def got_message(self, message):
        if message.getAttribute('type') == 'groupchat':
            self.got_groupchat_message(message)
        else:
            self.debug(message)
    
    def is_new(self, message):
        if message.x == None:
            return True
        else:
            if message.x.hasAttribute('stamp'):
                return False
            else:
                return True
    
    def got_groupchat_message(self, message):
        #print 'GC!'
        if not self.is_new(message):
            return
        
        text = message.body
        recived_from = jid.JID(message.getAttribute('from'))
        nick = recived_from.resource
        channel = '%s@%s' % (recived_from.user, recived_from.host)
        #print channel, nick
        
        own = message.getAttribute('from') == '%s/%s' % (self.channel, self.nick)
        #print own
        if not own and nick != None:
            self.send_message(
                channel, 
                '<%s> %s' % (nick, text), 
                msg_type='groupchat'
            )

    def debug(self, element):
        silent = True
        
        if not silent:
            print element.toXml().encode('utf-8')
            print '-' * 20

def main():
    resource = 'gatskerd'
    
    parser = OptionParser()
    parser.add_option('--jid', dest='jabber_id',
            help='set the JID', metavar='JID')
    parser.add_option('--pass', dest='jabber_password',
            help='set the password for that JID', metavar='PASSWORD')
    parser.add_option('--jnick', dest='jabber_nick',
            help='set the Jabber MUC nick', metavar='NICK', default=resource)
    parser.add_option('--muc', dest='jabber_channel',
            help='set the Jabber MUC', metavar='MUC')
    parser.add_option('--server', dest='irc_server',
            help='set the IRC server', metavar='SERVER')
    parser.add_option('--channel', dest='irc_channel',
            help='set the IRC channel', metavar='CHANNEL')
    parser.add_option('--nick', dest='irc_nick',
            help='set the IRC nick', metavar='NICK', default=resource)
    
    options, args = parser.parse_args()
    
    # create factory protocol and application
    irc_factory = IRCMirrorFactory(options.irc_nick, options.irc_channel)
    
    current_jid = jid.JID('%s/%s' % (options.jabber_id, resource))
    jabber_factory = client.basicClientFactory(current_jid, options.jabber_password)

    jabber = JabberSide(current_jid, options.jabber_nick, options.jabber_channel)

    jabber_factory.addBootstrap('//event/stream/authd', jabber.authd)
    jabber_factory.addBootstrap("//event/client/basicauth/invaliduser", jabber.debug)
    jabber_factory.addBootstrap("//event/client/basicauth/authfailed", jabber.debug)
    jabber_factory.addBootstrap("//event/stream/error", jabber.debug)

    # connect factory to this host and port
    reactor.connectTCP(options.irc_server, 6667, irc_factory)
    reactor.connectTCP(current_jid.host, 5222, jabber_factory)

    # run bot
    reactor.run()

if __name__ == '__main__':
    main()
