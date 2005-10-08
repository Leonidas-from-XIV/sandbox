#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class PSRParser(base.StationBase):
    """Parser for PSR"""
    
    __station__ = 'PSR'
    __version__ = '0.3.1'
    __versiontuple__ = base.splitver(__version__)
    
    def __init__(self, url='http://www.radiopsr.de/www/webradio/e98cb037f376fa53b314c166766ef55e.php'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        
        track_rex = self.create_regexp('<td>', '</td>')
        chunks = track_rex.findall(self.pagecontent)
        tracks = [track for track in chunks if not track.startswith('<')]
        current = tracks[0]
        
        try:
            self.artist, self.title = current.split(' - ')
        except ValueError, e:
            self.artist = current
            self.title = None
    
    def current_track(self):
        if self.title != None:
            return "%s - %s" % (self.artist, self.title)
        else:
            # no title - means "News" or things like this
            return self.artist

Parser = PSRParser

if __name__ == '__main__':
    base.test_parser(Parser, 'psr_ad.html')
