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
        
        self.menuitem = gtk.SeparatorMenuItem()
        self.mode_menu.add(self.menuitem)
        self.menuitem = gtk.MenuItem('About')
        self.menuitem.connect('activate', self.OnAbout)
        self.mode_menu.add(self.menuitem)
        self.menuitem = gtk.MenuItem('About Mode')
        self.menuitem.connect('activate', self.OnAboutMode)
        self.mode_menu.add(self.menuitem)
        self.menuitem = gtk.MenuItem('Exit')
        self.menuitem.connect('activate', self.OnExit)
        self.mode_menu.add(self.menuitem)
        
        self.menubar.append(self.menu)
        
        self.window.show_all()
    
    def delete_event(self, widget, event, data=None):
        gtk.main_quit()
        return False
    
    def OnExit(self, widget):
        gtk.main_quit()
    
    def OnAbout(self, widget):
        pass
    
    def OnAboutMode(self, widget):
        pass

def main():
    mw = MathWindow()
    gtk.main()

if __name__ == '__main__':
    main()