#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A namespace wrapper for opengl"""

class OGLWrappers(object):
    """Wrapper"""
    def __replace(self, module, funcprefix, constprefix):
        """Replaces prefixes"""
        for name in module.__dict__.keys():
            if name.startswith(funcprefix):
                setattr(self, name[len(funcprefix):], module.__dict__[name])
            elif name.startswith(constprefix):
                setattr(self, name[len(constprefix):], module.__dict__[name])
            else:
                setattr(self, name, module.__dict__[name])

class GLWrap(OGLWrappers):
    """Wrapper for GL"""
    def __init__(self):
        import OpenGL.GL
        self.attrs = self._OGLWrappers__replace(OpenGL.GL, 'gl', 'GL_')

class GLUWrap(OGLWrappers):
    """Wrapper for GLU"""
    def __init__(self):
        import OpenGL.GLU
        self.attrs = self._OGLWrappers__replace(OpenGL.GLU, 'glu', 'GLU_')

class GLUTWrap(OGLWrappers):
    def __init__(self):
        import OpenGL.GLUT
        self.attrs = self._OGLWrappers__replace(OpenGL.GLUT, 'glut', 'GLUT_')

class GLEWrap(OGLWrappers):
    def __init__(self):
        import OpenGL.GLE
        self.attrs = self._OGLWrappers__replace(OpenGL.GLE, 'gle', 'GLE_')

class WGLWrap(OGLWrappers):
    def __init__(self):
        import OpenGL.WGL
        self.attrs = self._OGLWrappers__replace(OpenGL.WGL, 'wgl', 'WGL_')
        
gl = GLWrap()
glu = GLUWrap()
glut = GLUTWrap()
gle = GLEWrap()
wgl = WGLWrap()