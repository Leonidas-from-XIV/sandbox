#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base, re

class GoTvParser(base.StationBase):
    """GoTv"""
    
    __station__ = 'GoTv'
    __version__ = '0.1.1'
    
    def __init__(self, url='http://www.gotv.at/titel_main.php'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        
        track = self.cut_content("\('VideoJetzt'\).innerHTML != '", "'\)")[0]
        # remove eventually contained excape backslashes
        track = track.replace('\\', '')
        
        # remove eventual html tags
        rex = re.compile("<.*?>")
        track = rex.sub('', track)
        
        # set informations
        self.artist, self.title = track.split(' - ')
    
    def current_track(self):
        return "%s - %s" % (self.artist, self.title)

Parser = GoTvParser

if __name__ == '__main__':
    base.test_parser(Parser, 'gotv.html')
    