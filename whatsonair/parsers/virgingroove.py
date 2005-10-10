#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""This plugin depends on the VirginRadio plugin, as it uses its parser internally"""

import base, virgin

class VirginGrooveParser(virgin.VirginParser):
    """Virgin Radio Groove: non-stop disco and Motown"""
    
    __station__ = 'VirginGroove'
    __version__ = '0.1.0'
    
    def __init__(self, url='http://mangle.smgradio.com/gr.js'):
        virgin.VirginParser.__init__(self, url)

Parser = VirginGrooveParser

if __name__ == '__main__':
    base.test_parser(Parser, 'gr.js')
