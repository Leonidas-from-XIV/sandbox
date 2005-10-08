#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class FFHParser(base.StationBase):
    """Parser for Hitradio FFH"""
    
    __station__ = 'FFH'
    __version__ = '0.2.1'
    __versiontuple__ = base.splitver(__version__)
    
    def __init__(self, url='http://www.ffh.de/api/webradio.php'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        artists_rex = self.create_regexp('<h5>', '</h5>')
        artists = artists_rex.findall(self.pagecontent)
        self.artist = artists[0]
        
        title_rex = self.create_regexp('<h6>', '</h6>')
        titles = title_rex.findall(self.pagecontent)
        self.title = titles[0]
    
    def current_track(self):
        return "%s - %s" % (self.capstext(self.artist), self.capstext(self.title))

Parser = FFHParser

if __name__ == '__main__':
    base.test_parser(Parser, 'ffh.html')
