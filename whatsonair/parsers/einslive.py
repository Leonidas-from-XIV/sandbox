#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class EinsLiveParser(base.StationBase):
    """Parser for EinsLive"""
    
    __station__ = 'EinsLive'
    __version__ = '0.1.0'
    __versiontuple__ = base.splitver(__version__)
    
    def __init__(self, url='http://www.einslive.de/diemusik/dieplaylists/die_letzten_12_titel/index.phtml'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        titles_rex = self.create_regexp('<TD valign="top" class="cont">', '&nbsp;</TD>')
        titles = titles_rex.findall(self.pagecontent)
        titles = titles[1::2]
        
        artists_rex = self.create_regexp('<TD valign="top" class="contbold">', '</TD>')
        artists = artists_rex.findall(self.pagecontent)
        
        both = zip(artists, titles)
        self.artist, self.title = both[0]
    
    def current_track(self):
        return "%s - %s" % (self.capstext(self.artist), self.capstext(self.title))

Parser = EinsLiveParser

if __name__ == '__main__':
    base.test_parser(Parser, 'einslive.html')
