#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class VirginParser(base.StationBase):
    """Parser for Virgin Radio: the music we all love"""
    
    __station__ = 'Virgin'
    __version__ = '0.1.0'
    
    def __init__(self, url='http://mangle.smgradio.com/vr.js'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        info_string = self.cut_content('var gimpdata="', '"')[0]
        informations = info_string.split('~')
        self.artist = informations[0]
        self.title = informations[2]
    
    def current_track(self):
        return "%s - %s" % (self.capstext(self.artist), self.capstext(self.title))

Parser = VirginParser

if __name__ == '__main__':
    base.test_parser(Parser, 'vr.js')
