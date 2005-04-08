#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A namespace wrapper for opengl"""

class OGLWrappers(object):
    """Wrapper
    dir(wrapped class) --> glwrap.(wrapped class).attrs.keys()"""
    def __getattr__(self, name):
        try:
            return self.attrs[name]
        except KeyError:
            raise AttributeError("%s has no attribute '%s'" % (self.wrapped, name))
        
    def replace(self, module, funcprefix, constprefix):
        """Returns a dictionary with renamed variables.
        You need to specify the module, the prefix of the functions
        and the prefix of the constants to be removed."""
        attrs = {}
        for name in module.__dict__.keys():
            if name.startswith(funcprefix):
               attrs[name[len(funcprefix):]] = module.__dict__[name]
            elif name.startswith(constprefix):
                attrs[name[len(constprefix):]] = module.__dict__[name]
            else:
                attrs[name] = module.__dict__[name]
        return attrs

class GLWrap(OGLWrappers):
    def __init__(self):
        import OpenGL.GL
        self.wrapped = OpenGL.GL.__name__
        self.attrs = self.replace(OpenGL.GL, 'gl', 'GL_')

class GLUWrap(OGLWrappers):
    def __init__(self):
        import OpenGL.GLU
        self.wrapped = OpenGL.GLU.__name__
        self.attrs = self.replace(OpenGL.GLU, 'glu', 'GLU_')

class GLUTWrap(OGLWrappers):
    def __init__(self):
        import OpenGL.GLUT
        self.wrapped = OpenGL.GLUT.__name__
        self.attrs = self.replace(OpenGL.GLUT, 'glut', 'GLUT_')

class GLEWrap(OGLWrappers):
    def __init__(self):
        import OpenGL.GLE
        self.wrapped = OpenGL.GLE.__name__
        self.attrs = self.replace(OpenGL.GLE, 'gle', 'GLE_')

class WGLWrap(OGLWrappers):
    def __init__(self):
        import OpenGL.WGL
        self.wrapped = OpenGL.WGL.__name__
        self.attrs = self.replace(OpenGL.WGL, 'wgl', 'WGL_')
        
gl = GLWrap()
glu = GLUWrap()
glut = GLUTWrap()
gle = GLEWrap()
wgl = WGLWrap()