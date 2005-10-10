#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class HR3Parser(base.StationBase):
    """HR3"""
    
    __station__ = 'HR3'
    __version__ = '0.1.2'
    
    def __init__(self, url='http://www3.admin.hr-online.de/playlist/playlist.php?tpl=hr3neu'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        both = self.cut_content('<td bgcolor="#ffffff">', '</td>')
        # every second hit is an artist
        artists = both[::2]
        # te reast are titles
        tracks = both[1::2]
        bound = zip(artists, tracks)
        
        self.artist, self.title = bound[0]
    
    def current_track(self):
        return "%s - %s" % (self.artist, self.title)

Parser = HR3Parser

if __name__ == '__main__':
    base.test_parser(Parser, 'hr3.html')
    