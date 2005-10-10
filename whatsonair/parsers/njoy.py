#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class NJoyParser(base.StationBase):
    """NJoy"""
    
    __station__ = 'NJoy'
    __version__ = '0.1.1'
    
    def __init__(self, url='http://www1.n-joy.de/pages_special/0,,SPM2156,00.html'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        artists = self.cut_content('<td headers="headerB">', '\n</td>')
        titles = self.cut_content('<td headers="headerC">', '</td>')
        both = zip(artists, titles)
        print both
        
        self.artist, self.title = both[-1]
    
    def current_track(self):
        return "%s - %s" % (self.capstext(self.artist), self.title)

Parser = NJoyParser

if __name__ == '__main__':
    base.test_parser(Parser, 'nhack.html')
    