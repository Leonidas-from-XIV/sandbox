#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base

class EnergyMucParser(base.StationBase):
    """Energy in Munich
    listen to it on 93.3 MHz"""
    
    __station__ = 'EnergyMunich'
    __version__ = '0.7.1'
    __versiontuple__ = base.splitver(__version__)
    
    def __init__(self, url='http://www.energy.de/static/ticker/write_titel.phtml'):
        # how about this url? http://www.energy.de/static/ticker/ticker.php?sender=muenchen
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        track_rex = self.create_regexp('-1<p>-1<p>', '\r<p>')
        track = track_rex.findall(self.pagecontent)[0]
        self.artist, self.title = track.split(' - ')
    
    def current_track(self):
        return "%s - %s" % (self.capstext(self.artist), self.title)

Parser = EnergyMucParser

if __name__ == '__main__':
    base.test_parser(Parser, 'write_titel.html')
    