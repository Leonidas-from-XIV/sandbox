#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class GongParser(base.StationBase):
    """The parser for Gong"""
    
    __station__ = 'Gong'
    __version__ = '0.9.2'
    __versiontuple__ = base.splitver(__version__)
    
    def __init__(self, url='http://web1.beamgate.com/Gong/getPlaylist.jsp'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        track_rex = self.create_regexp('<td class="liedertext">', '</td>')
        track = track_rex.findall(self.pagecontent)[1]
        self.artist, self.title = track.split(': ')
    
    def current_track(self):
        return "%s - %s" % (self.capstext(self.artist), self.capstext(self.title))

Parser = GongParser

if __name__ == '__main__':
    base.test_parser(Parser, 'gong.html')
    