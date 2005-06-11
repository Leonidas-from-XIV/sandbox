#!/usr/bin/env python
# -*- encoding: latin-1 -*-
import pickle
import gtk, gtk.gdk

class MainWindow(object):
    def __init__(self):
        self.window = gtk.Window()
        self.window.set_title('Haircolor')
        self.window.connect('delete_event', self.quit)
        self.window.set_size_request(200, 200)
        
        self.box = gtk.Table()
        
        sw = gtk.ScrolledWindow()
        sw.set_shadow_type(gtk.SHADOW_ETCHED_IN)
        sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        
        self.liststore = gtk.ListStore(str, str)
        
        self.treeview = gtk.TreeView(self.liststore)
        self.tvcolumn1 = gtk.TreeViewColumn('Name')
        self.tvcolumn2 = gtk.TreeViewColumn('Haarfarbe')
        self.treeview.connect('button_press_event', self.treeview_event)
        
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
        
        self.addbutton = gtk.Button('Add')
        #self.colorbutton.connect('clicked', self.choosecolor)
        self.removebutton = gtk.Button('Remove')

        self.box.attach(sw, 0, 1, 0, 1)
        self.box.attach(self.colorbutton, 0, 2, 1, 2)
        
        self.window.add(self.box)
        
        self.load()
        self.window.show_all()
        
        
    
    def choosecolor(self, widget):
        self.colorseldlg = gtk.ColorSelectionDialog('Choose Hair Color')
        colorsel = self.colorseldlg.colorsel
        
        color = self.current_col()[1]
        color = gtk.gdk.color_parse(color)
        
        colorsel.set_current_color(color)
        colorsel.set_has_opacity_control(False)
        colorsel.set_has_palette(False)
        button = self.colorseldlg.run()

        if button == gtk.RESPONSE_OK:
            color = self.color_changed()
            current_col = self.current_col()
            current_col[1] = color
            self.colorseldlg.hide()
        else:
            self.colorseldlg.hide()
    
    def color_changed(self):
        color = self.colorseldlg.colorsel.get_current_color() 
        
        hexcolor = '#'+(hex(color.red)+'0')[2:4] \
            +(hex(color.green)+'0')[2:4] \
            +(hex(color.blue)+'0')[2:4]
        hexcolor = hexcolor.upper()
        return hexcolor
    
    def quit(self, widget, event):
        self.save()
        gtk.main_quit()
    
    def load(self):
        self.liststore.append(['Jenny', '#FBFF89'])
        self.liststore.append(['Svetla', '#251832'])
        
    def save(self):
        for i in self.liststore:
            print i[0], i[1]
    
    def current_col(self):
        selection = self.treeview.get_selection()
        selected = selection.get_selected_rows()[1][0][0]
        #print selected
        
        #print self.liststore[selected]
        return self.liststore[selected]
    
    def treeview_event(self, treeview, event):
        if event.button == 1:
            x = int(event.x)
            y = int(event.y)
            pthinfo = treeview.get_path_at_pos(x, y)
            if pthinfo != None:
                current_col = self.current_col()
                print current_col[1]
                #self.colorbutton.set_bg_color('red')
                self.change_button_color(current_col[1])
    
    def change_button_color(self, color):
        """Changes the color of the button"""
        style = self.colorbutton.get_style().copy()
        cm = self.colorbutton.get_colormap()
        bgc = cm.alloc_color(color)
        style.bg[gtk.STATE_NORMAL] = bgc
        self.colorbutton.set_style(style) 
                

def main():
    MainWindow()
    gtk.main()

if __name__ == '__main__':
    main()
