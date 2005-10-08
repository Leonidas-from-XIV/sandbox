#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class EnergySaxParser(base.StationBase):
    """Energy in Saxonia"""
    
    __station__ = 'EnergySaxonia'
    __version__ = '0.3.1'
    __versiontuple__ = base.splitver(__version__)
    
    def __init__(self, url='http://www.energy.de/static/ticker/ticker.php?sender=sachsen'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        title_rex = self.create_regexp('&song_sac=', '&&artist_sac=')
        self.title = title_rex.findall(self.pagecontent)[0]
        
        artist_rex = self.create_regexp('&&artist_sac=', '&')
        self.artist = artist_rex.findall(self.pagecontent)[0]
    
    def current_track(self):
        return "%s - %s" % (self.capstext(self.artist), self.capstext(self.title))

Parser = EnergySaxParser

if __name__ == '__main__':
    base.test_parser(Parser, 'sach.html')
    