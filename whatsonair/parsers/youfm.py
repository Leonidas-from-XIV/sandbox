#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""This plugin depends on the HR3 plugin, as it uses its parser internally"""

import base, hr3

class YouFmParser(hr3.HR3Parser):
    """YouFM"""
    
    __station__ = 'YouFM'
    __version__ = '0.1.2'
    
    def __init__(self, url='http://www3.admin.hr-online.de/playlist/playlist.php?tpl=youfm'):
        hr3.HR3Parser.__init__(self, url)

Parser = YouFmParser

if __name__ == '__main__':
    base.test_parser(Parser, 'youfm.html')
    