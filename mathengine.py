#!/usr/bin/env python
# -*- encoding: latin-1 -*- 

import gtk

class MathWindow(object):
    def __init__(self):
        self.window = gtk.Window()
        self.window.set_default_size(350, 100)
        self.window.connect('delete_event', self.delete_event)
        self.window.set_title('MathEngine')
        
        self.vbox = gtk.VBox()
        self.window.add(self.vbox)
        self.menubar = gtk.MenuBar()
        self.vbox.pack_start(self.menubar, expand=False)
        
        self.menu = gtk.MenuItem('Mode')
        self.mode_menu = gtk.Menu()
        self.menu.set_submenu(self.mode_menu)
        self.exit = gtk.MenuItem('Exit')
        self.mode_menu.add(self.exit)
        self.menubar.append(self.menu)
        #menuitem.set_submenu(self.mode_menu)
        
        #self.window.add(self.menubar)
        self.window.show_all()
    
    def delete_event(self, widget, event, data=None):
        gtk.main_quit()
        return False

def main():
    mw = MathWindow()
    gtk.main()

if __name__ == '__main__':
    main()