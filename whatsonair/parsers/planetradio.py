#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class PlanetRadioParser(base.StationBase):
    """Parser for PlanetRadio"""
    
    __station__ = 'PlanetRadio'
    __version__ = '0.1.0'
    
    def __init__(self, url='http://www.planetradio.de/p_mt.php'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        # get a list of titles
        titles = self.cut_content('target=_blank>', '</a></td><td>')
        # get a list of titles
        artists = self.cut_content('</a></td><td>', '</td></tr>')
        # connect them
        both = zip(artists, titles)
        
        # use the first entry in the list
        self.artist, self.title = both[0]
    
    def current_track(self):
        return "%s - %s" % (self.capstext(self.artist), self.capstext(self.title))

Parser = PlanetRadioParser

if __name__ == '__main__':
    base.test_parser(Parser, 'planetradio.html')
