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
        self.orientation = 0

        self.canvas = gnomecanvas.Canvas()
        self.canvas.set_size_request(width, height)
        self.canvas.set_scroll_region(0, 0, width, height)
        self.window.add(self.canvas)

        back = self.canvas.root().add(gnomecanvas.CanvasRect,
                fill_color='white', x1=0, y1=0, x2=width, y2=height)
        self.bpath = self.canvas.root().add(gnomecanvas.CanvasBpath,
                outline_color='black', width_pixels=3)

        self.window.show_all()

    def create_spiral(self):
        """rotate takes an angle in radian."""
        self.path = [
            (self.width / 2, self.height / 2)
        ]
        a = 0.03
        for t in frange(0, 10 * math.pi, 0.1):
            r = a * t
            angle = t + self.orientation
            x = r * math.cos(angle) * 200
            y = r * math.sin(angle) * 200
            self.path.append((x + 200, y + 200))

        self.render_path(self.path)

    def render_path(self, path):
        commands = [(gnomecanvas.MOVETO_OPEN, path[0][0], path[0][1])]
        commands += [(gnomecanvas.LINETO, x, y) for x, y in path[1:]]
        path_def = gnomecanvas.path_def_new(commands)
        self.bpath.set_bpath(path_def)

    def rotate(self, angle):
        self.orientation += angle
        self.create_spiral()
        return True

def frange(start, stop, step):
    while start < stop:
        yield start
        start += step

def main():
    win = HypnoticWindow(400, 400)
    win.create_spiral()
    gobject.timeout_add(10, win.rotate, 0.1)
    gtk.main()

if __name__ == '__main__':
    main()
