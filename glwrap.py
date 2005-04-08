#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A namespace wrapper for opengl"""

class OGLWrappers(object):
    def __getattr__(self, name):
        try:
            return self.attrs[name]
        except KeyError:
            raise AttributeError("%s has no attribute '%s'" % (self.wrapped, name))
        
    def replace(self, module, funcprefix, constprefix):
        attrs = {}
        for name in module.__dict__.keys():
            if name.startswith('gle'):
               attrs[name[3:]] = module.__dict__[name]
            elif name.startswith('GLE_'):
                attrs[name[4:]] = module.__dict__[name]
            else:
                attrs[name] = module.__dict__[name]
        return attrs
    

class GLWrap(OGLWrappers):
    wrapped = "OpenGL.GL"
    def __init__(self):
        import OpenGL.GL
        self.attrs = {}
        
        for name in OpenGL.GL.__dict__.keys():
            if name.startswith('gl'):
                self.attrs[name[2:]] = OpenGL.GL.__dict__[name]
            elif name.startswith('GL_'):
                self.attrs[name[3:]] = OpenGL.GL.__dict__[name]
            else:
                self.attrs[name] = OpenGL.GL.__dict__[name]

class GLUWrap(OGLWrappers):
    wrapped = "OpenGL.GLU"
    def __init__(self):
        import OpenGL.GLU
        self.attrs = {}
        
        for name in OpenGL.GLU.__dict__.keys():
            if name.startswith('glu'):
                self.attrs[name[3:]] = OpenGL.GLU.__dict__[name]
            elif name.startswith('GLU_'):
                self.attrs[name[4:]] = OpenGL.GLU.__dict__[name]
            else:
                self.attrs[name] = OpenGL.GLU.__dict__[name]

class GLEWrap(OGLWrappers):
    def __init__(self):
        import OpenGL.GLE
        self.wrapped = OpenGL.GLE.__name__
        self.attrs = self.replace(OpenGL.GLE, 'gle', 'GLE_')
        
        
    
gl = GLWrap()
glu = GLUWrap()
gle = GLEWrap()