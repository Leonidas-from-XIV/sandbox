#!/usr/bin/env python
# -*- coding: UTF-8 -*-
import math
import gtk
import gnomecanvas

class HypnoticWindow(object):
    def __init__(self, width, height):
        self.window = gtk.Window()
        self.window.connect('delete_event', gtk.main_quit)
        self.width, self.height = width, height

        self.canvas = gnomecanvas.Canvas()
        self.canvas.set_size_request(width, height)
        self.canvas.set_scroll_region(0, 0, width, height)
        self.window.add(self.canvas)

        back = self.canvas.root().add(gnomecanvas.CanvasRect,
                    outline_color="black", fill_color="white",
                    x1=0, y1=0, x2=width, y2=height)

        self.window.show_all()

    def create_spiral(self, x, y, line_width=3):
        path = [
            (gnomecanvas.MOVETO_OPEN, self.width / 2, self.height / 2)
        ]
        for dummy in xrange(500):
            r = math.hypot(x, y) + 0.4
            a = math.atan2(y, x) + 0.2
            x = r * math.cos(a)
            y = r * math.sin(a)
            path.append(
                (gnomecanvas.LINETO, x + self.width / 2, y + self.height / 2))

        path_def = gnomecanvas.path_def_new(path)
        bpath = self.canvas.root().add(gnomecanvas.CanvasBpath,
                outline_color='black', width_pixels=line_width)
        bpath.set_bpath(path_def)

def main():
    win = HypnoticWindow(400, 400)
    win.create_spiral(0, 0)
    gtk.main()

if __name__ == '__main__':
    main()
