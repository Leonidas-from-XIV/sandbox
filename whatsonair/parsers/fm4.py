#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class FM4Parser(base.StationBase):
    """The Parser for the austrian sidestream radio station
    FM4, which is part of ORF.
    Look at it's homepage http://fm4.orf.at
    
    Maybe besser use this songlist?
    http://fm4.orf.at/trackservicepopup/main
    But then we loose the ability to parse OE3 as well"""
    
    __station__ = 'FM4'
    __version__ = '0.9.2'
    
    def __init__(self, url='http://hop.orf.at/img-trackservice/fm4.html'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        artists = self.cut_content('<span class="artist">', '</span><br/>')
        self.artist = artists[-1]
        
        titles = self.cut_content('class="tracktitle">', '</span> <span class="separator">')
        self.title = titles[-1]
    
    def current_track(self):
        return "%s - %s" % (self.artist, self.title)

Parser = FM4Parser

if __name__ == '__main__':
    base.test_parser(Parser, 'fm4.html')
    