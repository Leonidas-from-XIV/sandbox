#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import urllib, re

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
        """Initialize some values."""
        self.crawlerurl = url
    
    def feed(self):
        """Loads"""
        timeout = urllib.socket.getdefaulttimeout()
        urllib.socket.setdefaulttimeout(10.0)
        parsepage = urllib.urlopen(self.crawlerurl)
        self.pagecontent = parsepage.read()
        parsepage.close()
        urllib.socket.setdefaulttimeout(timeout)
    
    def current_track(self):
        """Return the current track in the format
        ARTIST - TITLE as string, unicode is also ok"""
        raise NotImplementedError("Abstract class")
    
    def capstext(self, text):
        """A helper method to make the texts look consistent
        [...]"""
        chunks = text.split()
        
        if len(chunks) > 1:
            return reduce(lambda x, y: x.capitalize() + ' ' + y.capitalize(), chunks)
        else:
            return text.capitalize()
    
    def create_regexp(self, start, stop):
        reg_exp_code = r'(?<=%s).*(?=%s)' % (start, stop)
        compiled = re.compile(reg_exp_code)
        return compiled

Parser = None

def test_parser(parser, filename):
    """This is used to test newly written parsers.
    Import this module from a plugin using
    import base
    and then call base.test_parser(parser, filename).
    It starts the parser, sets it up and tries to get the current title.
    
    Your parsers should pass this test, means they should print the 
    correct data.
    """
    p = parser()
    f = file(filename, 'r')
    p.pagecontent = f.read()
    f.close()
    p.parse()
    print p.current_track()