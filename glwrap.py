#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A namespace wrapper for opengl"""
import OpenGL

class OGLWrapper(object):
    """General OpenGL wrapper"""
    def __init__(self, module):
        module = 'GL'
        funcprefix = module.lower()
        constprefix = module.upper() + '_'
        mod = eval("OpenGL." + module)
        #mod = __import__('OpenGL.' + module)
        self.__rename(mod, funcprefix, constprefix)
        
    def __rename(self, module, funcprefix, constprefix):
        """Replaces prefixes"""
        for name in module.__dict__.keys():
            if name.startswith(funcprefix):
                setattr(self, name[len(funcprefix):], module.__dict__[name])
            elif name.startswith(constprefix):
                setattr(self, name[len(constprefix):], module.__dict__[name])
            else:
                setattr(self, name, module.__dict__[name])

gl = OGLWrapper('gl')
glu = OGLWrapper('glu')
glut = OGLWrapper('glut')
gle = OGLWrapper('gle')
wgl = OGLWrapper('wgl')