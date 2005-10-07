#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class AntenneParser(base.StationBase):
    """Parser for Antenne Bayern"""
    
    __station__ = 'AntenneBayern'
    __version__ = '0.6.1'
    __versiontuple__ = base.splitver(__version__)
    
    def __init__(self, url='http://webradio.antenne.de/antenne/webradio/new_channels/ant_infos.php'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        artists_rex = self.create_regexp('<b>', '</b>')
        artists = artists_rex.findall(self.pagecontent)
        self.artist = artists[1]
        
        title_rex = self.create_regexp('</b>, ', '</a>')
        titles = title_rex.findall(self.pagecontent)
        self.title = titles[0]
    
    def current_track(self):
        if self.title != '':
            return "%s - %s" % (self.capstext(self.artist), self.capstext(self.title))
        else:
            # no title - means "News" or things like this
            return self.artist

Parser = AntenneParser

if __name__ == '__main__':
    base.test_parser(Parser, 'news.html')
