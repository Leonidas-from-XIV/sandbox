#!/usr/bin/env python
# -*- encoding: latin-1 -*- 

import gtk

class MathWindow(object):
    def __init__(self):
        self.window = gtk.Window()
        self.window.set_default_size(350, 100)
        self.window.connect('delete_event', self.delete_event)
        self.window.set_title('MathEngine')
        self.window.show_all()
    
    def delete_event(self, widget, event, data=None):
        gtk.main_quit()
        return False

def main():
    mw = MathWindow()
    gtk.main()

if __name__ == '__main__':
    main()