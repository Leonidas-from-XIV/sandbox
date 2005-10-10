#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class GongParser(base.StationBase):
    """The parser for Gong"""
    
    __station__ = 'Gong'
    __version__ = '0.9.2'
    
    def __init__(self, url='http://web1.beamgate.com/Gong/getPlaylist.jsp'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        track = self.cut_content('<td class="liedertext">', '</td>')[1]
        try:
            self.artist, self.title = track.split(': ')
        except ValueError, e:
            # sometimes the site does not provide the tracks' title,
            #+so set it to None
            self.artist = track
            self.title = None
    
    def current_track(self):
        if self.title != None:
            return "%s - %s" % (self.capstext(self.artist), self.capstext(self.title))
        else:
            return self.capstext(self.artist)

Parser = GongParser

if __name__ == '__main__':
    base.test_parser(Parser, 'justone.html')
    