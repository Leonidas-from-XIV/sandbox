#!/usr/bin/env python
# -*- encoding: latin-1 -*-
import gtk

class MainWindow(object):
    def __init__(self):
        self.window = gtk.Window()
        self.window.set_title('Haircolor')
        self.window.connect('delete_event', gtk.main_quit)
        
        self.box = gtk.Table()
        
        sw = gtk.ScrolledWindow()
        sw.set_shadow_type(gtk.SHADOW_ETCHED_IN)
        sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        
        self.liststore = gtk.ListStore(str, str)
        
        self.treeview = gtk.TreeView(self.liststore)
        self.tvcolumn1 = gtk.TreeViewColumn('Name')
        self.tvcolumn2 = gtk.TreeViewColumn('Haarfarbe')
        
        self.liststore.append(['Open', 'green'])
        self.liststore.append(['Open', '#000000'])
        
        self.treeview.append_column(self.tvcolumn1)
        self.treeview.append_column(self.tvcolumn2)
        sw.add(self.treeview)
        
        self.cell1 = gtk.CellRendererText()
        self.cell2 = gtk.CellRendererText()
        
        self.tvcolumn1.pack_start(self.cell1, True)
        self.tvcolumn2.pack_start(self.cell2, True)
        
        self.tvcolumn1.set_attributes(self.cell1, text=0)
        self.tvcolumn2.set_attributes(self.cell2, background=1)
        
        self.colorbutton = gtk.Button('Color')
        self.colorbutton.connect('clicked', self.choosecolor)

        self.box.attach(sw, 0, 1, 0, 1)
        self.box.attach(self.colorbutton, 0, 2, 1, 2)
        
        self.window.add(self.box)
        self.window.show_all()
        
    
    def choosecolor(self, widget):
        self.colorseldlg = gtk.ColorSelectionDialog('Choose Hair Color')
        button = self.colorseldlg.run()

        if button == gtk.RESPONSE_OK:
            self.colorseldlg.hide()
        else:
            self.colorseldlg.hide()
    
    def load(self):
        self.model.append(['n', 'm'])
        
    def save(self):
        pass

def main():
    MainWindow()
    gtk.main()

if __name__ == '__main__':
    main()
