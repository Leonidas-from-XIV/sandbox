#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import urllib

def splitver(version):
    """Splits the string representation of the version into a tuple form,
    so it's easier to parse"""
    return tuple(version.split('.'))

class StationBase(object):
    """The base class for each radio station parser
    provides already some rough tools like the HTMLParser.
    The defined methods should be overloaded to provide a
    consistent interface for all derived station parsers
    
    The constructor loads the page content into self.pagecontent,
    and also cares about timeout handling. So in child classes
    you need to call StationBase.__init__ for doing the most work.
    
    All child classes also have a feed() method, where the page content
    is parsed and all values initialized. After that, curenttrack() can
    be called to get the currently playing track."""
    __station__ = 'StationBase'
    __version__ = '1.0.0'
    __versiontuple__ = splitver(__version__)
    
    pagecontent = None
    
    def __init__(self, url):
        """Initialize some values.
        This method should be always called, to initalize the HTMLParser."""
        self.crawlerurl = url
    
    def feed(self):
        timeout = urllib.socket.getdefaulttimeout()
        urllib.socket.setdefaulttimeout(10.0)
        parsepage = urllib.urlopen(self.crawlerurl)
        self.pagecontent = parsepage.read()
        parsepage.close()
        urllib.socket.setdefaulttimeout(timeout)
    
    def currenttrack(self):
        """Return the current track in the format
        ARTIST - TITLE as string, unicode is also ok"""
        raise NotImplementedError("Abstract class")
    
    def alltitles():
        """Return all parsed titles..
        format unknown, as nowhere used"""
        raise NotImplementedError("Abstract class")
    
    def capstext(self, text):
        """A helper method to make the texts look consistent
        [...]"""
        chunks = text.split()
        
        if len(chunks) > 1:
            return reduce(lambda x, y: x.capitalize() + ' ' + y.capitalize(), chunks)
        else:
            return text.capitalize()

Parser = None