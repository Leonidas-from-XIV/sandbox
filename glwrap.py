#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A namespace wrapper for opengl"""
import OpenGL

class OGLWrapper(object):
    """General OpenGL wrapper"""
    def __init__(self, module):
        funcprefix = module.lower()
        constprefix = module.upper() + '_'
        # import the submodule
        mod = __import__('OpenGL.' + module, globals(), locals(), ['OpenGL'])
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

gl = OGLWrapper('GL')
glu = OGLWrapper('GLU')
glut = OGLWrapper('GLUT')
gle = OGLWrapper('GLE')
wgl = OGLWrapper('WGL')