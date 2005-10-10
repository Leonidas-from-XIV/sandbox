#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""This plugin depends on the VirginRadio plugin, as it uses its parser internally"""

import base, virgin

class VirginXtremeParser(virgin.VirginParser):
    """Virgin Radio Xtreme: new music - no limits"""
    
    __station__ = 'VirginXtreme'
    __version__ = '0.1.0'
    
    def __init__(self, url='http://mangle.smgradio.com/vx.js'):
        virgin.VirginParser.__init__(self, url)

Parser = VirginXtremeParser

if __name__ == '__main__':
    base.test_parser(Parser, 'vx.js')
