#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class LandesWelleParser(base.StationBase):
    """Parser for LandesWelle"""
    
    __station__ = 'Landeswelle'
    __version__ = '0.1.0'
    
    def __init__(self, url='http://www.landeswelle.de/lwt/components/flash/index_lwt.php'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        divided = self.pagecontent.split('#')
        self.title = divided[1].split('=')[1]
        self.artist = divided[2].split('=')[1]
    
    def current_track(self):
        return "%s - %s" % (self.capstext(self.artist), self.capstext(self.title))

Parser = LandesWelleParser

if __name__ == '__main__':
    base.test_parser(Parser, 'lwt.html')

