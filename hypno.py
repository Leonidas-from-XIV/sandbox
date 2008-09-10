#!/usr/bin/env python
# -*- coding: UTF-8 -*-
import math
import gtk, gobject
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
        self.bpath = self.canvas.root().add(gnomecanvas.CanvasBpath,
                outline_color='black', width_pixels=3)

        self.window.show_all()

    def create_spiral(self, x, y):
        self.path = [
            (self.width / 2, self.height / 2)
        ]
        for dummy in xrange(500):
            r = math.hypot(x, y) + 0.4
            a = math.atan2(y, x) + 0.2
            x = r * math.cos(a)
            y = r * math.sin(a)
            self.path.append(
                (x + self.width / 2, y + self.height / 2))

        self.render_path(self.path)

    def render_path(self, path):
        commands = [(gnomecanvas.MOVETO_OPEN, path[0][0], path[0][1])]
        commands += [(gnomecanvas.LINETO, x, y) for x, y in path[1:]]
        path_def = gnomecanvas.path_def_new(commands)
        self.bpath.set_bpath(path_def)

    def rotate(self, x0, y0, d):
        coords = list()
        old = self.path
        for i in range(0, len(old), 2):
            try:
                x, y = old[i][0] - x0, old[i + 1][1] - y0
            except IndexError:
                continue
            r = math.hypot(y, x)
            a = math.atan2(y, x) + d
            coords.append((x0 + r * math.cos(a), y0 + r * math.sin(a)))
        self.render_path(coords)
        self.path = coords

def main():
    win = HypnoticWindow(400, 400)
    win.create_spiral(0, 0)
    #win.rotate(0, 0, 0.1)
    #gobject.timeout_add(100, win.rotate, 0, 0, 0.1)
    gtk.main()

if __name__ == '__main__':
    main()
