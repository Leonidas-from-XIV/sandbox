#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class FM4Parser(base.StationBase):
    """The Parser for the austrian sidestream radio station
    FM4, which is part of ORF.
    Look at it's homepage http://fm4.orf.at"""
    
    __station__ = 'FM4'
    __version__ = '0.9.1'
    __versiontuple__ = base.splitver(__version__)
    
    def __init__(self, url='http://fm4.orf.at/trackservicepopup/stream'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        artists_rex = self.create_regexp('<span class="artist">', '</span><br/>')
        artists = artists_rex.findall(self.pagecontent)
        self.artist = artists[-1]
        
        title_rex = self.create_regexp('class="tracktitle">', '</span> <span class="separator">')
        titles = title_rex.findall(self.pagecontent)
        self.title = titles[-1]
    
    def current_track(self):
        return "%s - %s" % (self.artist, self.title)

Parser = FM4Parser

if __name__ == '__main__':
    p = Parser()
    #p.feed()
    p.pagecontent = file('stream.htm', 'r').read()
    #print p.pagecontent
    p.parse()
    p.current_track()
    