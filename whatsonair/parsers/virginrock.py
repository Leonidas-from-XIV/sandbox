#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""This plugin depends on the VirginRadio plugin, as it uses its parser internally"""

import base, virgin

class VirginRockParser(virgin.VirginParser):
    """Virgin Radio Classic Rock: the rock authority"""
    
    __station__ = 'VirginRock'
    __version__ = '0.1.0'
    
    def __init__(self, url='http://mangle.smgradio.com/vc.js'):
        virgin.VirginParser.__init__(self, url)

Parser = VirginRockParser

if __name__ == '__main__':
    base.test_parser(Parser, 'vc.js')
