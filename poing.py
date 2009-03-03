#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""Small example on how to animate stuff on the GooCanvas.
The canvas is surprisingly useful, at least compared with GNOME Canvas."""

import gtk
import goocanvas

class PoingWindow(object):
    def __init__(self):
        # create and set up window, canvas and canvas items
        self.window = gtk.Window()
        self.window.connect('delete-event', gtk.main_quit)
        self.canvas = goocanvas.Canvas()
        root = self.canvas.get_root_item()
        self.point = goocanvas.Ellipse(
            parent=root,
            radius_x=1,
            radius_y=1,
            center_x=0,
            center_y=100)
        self.window.add(self.canvas)
        self.window.show_all()

    def animate(self):
        # move 200 to the right, 1000ms duration
        # and come back after that
        self.point.animate(200, 0, 1, 0, False,
                1000, 40, goocanvas.ANIMATE_BOUNCE)

def main():
    pw = PoingWindow()
    pw.animate()
    gtk.main()

if __name__ == '__main__':
    main()
