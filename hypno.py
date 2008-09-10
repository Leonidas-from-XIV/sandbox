#!/usr/bin/env python
# -*- coding: UTF-8 -*-
import math
import gtk
import gnomecanvas

class HypnoticWindow(object):
    def __init__(self):
        self.window = gtk.Window()
        self.window.connect('delete_event', gtk.main_quit)

        self.canvas = gnomecanvas.Canvas()
        self.canvas.set_size_request(400, 400)
        self.canvas.set_scroll_region(0, 0, 400, 400)
        self.window.add(self.canvas)

        back = self.canvas.root().add(gnomecanvas.CanvasRect,
                    outline_color="black", fill_color="white",
                    x1=0, y1=0, x2=400, y2=400)

        self.window.show_all()

    def create_spiral(self, x, y, line_width=3):
        path = [
            (gnomecanvas.MOVETO_OPEN, x, y)
        ]
        for dummy in xrange(500):
            r = math.hypot(x, y) + 0.4
            a = math.atan2(y, x) + 0.2
            x = r * math.cos(a)
            y = r * math.sin(a)
            path.append((gnomecanvas.LINETO, x, y))

        path_def = gnomecanvas.path_def_new(path)
        bpath = self.canvas.root().add(gnomecanvas.CanvasBpath,
                outline_color='black', width_pixels=line_width)
        bpath.set_bpath(path_def)

def main():
    win = HypnoticWindow()
    win.create_spiral(0, 0)
    gtk.main()

if __name__ == '__main__':
    main()
