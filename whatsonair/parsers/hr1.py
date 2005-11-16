#!/usr/bin/env python
# -*- encoding: latin-1 -*-

import base, hr3

class HR1Parser(hr3.HR3Parser):
    """HR1"""
    
    __station__ = 'HR1'
    __version__ = '0.1.2'
    
    def __init__(self, url='http://www3.admin.hr-online.de/playlist/playlist.php?tpl=hr1'):
        base.StationBase.__init__(self, url)

Parser = HR1Parser

if __name__ == '__main__':
    base.test_parser(Parser, 'hr1.html')
    