#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base, re

class SWR3Parser(base.StationBase):
    """Parser for SWR 3"""
    
    __station__ = 'SWR3'
    __version__ = '0.1.0'
    
    def __init__(self, url='http://www.swr3.de/musik/musikrecherche/last13.html'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        
        # first: remove all that nasty spaces and linebreaks
        self.pagecontent = self.pagecontent.replace('\n', '')
        self.pagecontent = self.pagecontent.replace('          ', '')
        self.pagecontent = self.pagecontent.replace('  ', '')
        
        # split the content
        light_tracks = self.cut_content("<td valign='top' bgcolor='#E8E8E8'>", "</td>")
        self.artist, self.title = light_tracks[0:2]
            
        # remove eventual html tags here
        rex = re.compile("<.*?>")
        self.title = rex.sub('', self.title)
        
    
    def current_track(self):
        if self.artist != '':
            return "%s - %s" % (self.artist, self.title)
        else:
            # no title - means "News" or things like this
            return self.title

Parser = SWR3Parser

if __name__ == '__main__':
    base.test_parser(Parser, 'last13html.html')
