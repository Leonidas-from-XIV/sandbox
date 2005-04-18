#!/usr/bin/env python
# -*- encoding: latin-1 -*- 

import gtk

__version__ = '0.0.1'
__author__ = 'Marek Kubica'

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
        
        # Content
        self.table = gtk.Table()
        
        self.factor1 = gtk.Entry()
        self.factor1.set_sensitive(False)
        self.table.attach(self.factor1, 0, 1, 0, 1)
        
        self.op1 = gtk.Label()
        self.table.attach(self.op1, 1, 2, 0, 1)
        
        self.factor2 = gtk.Entry()
        self.factor2.set_sensitive(False)
        self.table.attach(self.factor2, 2, 3, 0, 1)
        
        self.op2 = gtk.Label()
        self.table.attach(self.op2, 3, 4, 0, 1)
        
        self.factor3 = gtk.Entry()
        self.factor3.set_sensitive(False)
        self.table.attach(self.factor3, 4, 5, 0, 1)
        
        self.lastresult_label = gtk.Label('Last result:')
        self.table.attach(self.lastresult_label, 0, 1, 1, 2)
        
        self.lastresult = gtk.Label('(No last result)')
        self.table.attach(self.lastresult, 2, 3, 1, 2)
        
        self.check = gtk.Button('Check it!')
        self.check.connect('clicked', self.OnCheck)
        self.table.attach(self.check, 4, 5, 1, 2)
        
        
        self.vbox.pack_start(self.table)
        
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
    
    def OnCheck(self, widget):
        print widget

class LoadableEngine:
    """A super class for all engines.
    Class members:
    __version__ - The version of the engine (the default is the version of the program)
    __author__ - The author of the engine (probably you, if you read this)
    state - the status of the engine: possible are unimplemented (default), experimental,
    unstable, stable. It has no real use, but is maybe helpful for the users
    question_field - very important! it sets the TextInputField where the user can input
    his answers (yes, this can be changed!). The default value is 3 for the third field.
    ops - a tuple (2) with the operators: the operators are strings with leading and 
    trailing spaces
    question() - outputs the question in a tuple (3), take care of question_field
    [write on]"""
    
    # set the version information equal to the programs'
    #+since is was made by the same person
    __version__ = __version__
    __author__ = __author__
    state = 'unimplemented'
    
    # which field will be checked for the answer?
    question_field = 3
    
    # which operators?
    ops = (" ? ", " ? ")

class MultiplyEngine(LoadableEngine):
    """The engine for multiplying"""
    state = 'stable'
    question_field = 3
    ops = (' x ', ' = ')
    def question(self):
        a = random.randrange(9)
        b = random.randrange(9)
        c = a * b
        return (a, b, c)

def main():
    mw = MathWindow()
    gtk.main()

if __name__ == '__main__':
    main()