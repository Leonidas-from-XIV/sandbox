#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base, re

class Bayern3Parser(base.StationBase):
    """Parser for Bayern 3"""
    
    __station__ = 'Bayern3'
    __version__ = '0.6.6'
    
    def __init__(self, url='http://www.br-online.de/bayern3/global/utils/homepage/nowonair.jsp'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        
        titles = self.cut_content('&#8222;', '&#8220;')
        self.title = titles[0]
        
        artists_rex = re.compile(r'(?<=<br/>).*(?=\t</div>)', re.DOTALL)
        artist = artists_rex.findall(self.pagecontent)[0]
        artist = artist.replace('\n', '').replace('\t', '')
        
        if ',' in artist:
            chunked = artist.split(', ')
            self.artist = ' '.join(reversed(chunked))
        else:
            self.artist = artist
    
    def current_track(self):
        if self.artist != '':
            return "%s - %s" % (self.artist, self.title)
        else:
            # no title - means "News" or things like this
            return self.title

Parser = Bayern3Parser

if __name__ == '__main__':
    base.test_parser(Parser, 'b3now.html')
