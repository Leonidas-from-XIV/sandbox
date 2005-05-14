#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A namespace wrapper for QT"""
import qt

for name in dir(qt):
    if name.startswith('q') or name.startswith('Q'):
        globals()[name[1:]] = getattr(qt, name)
    else:
        globals()[name] = getattr(qt, name)
    
