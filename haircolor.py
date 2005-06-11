#!/usr/bin/env python
# -*- encoding: latin-1 -*-
import gtk, gtk.gdk

class MainWindow(object):
    def __init__(self):
        """Set up the main window"""
        # Create a window
        self.window = gtk.Window()
        # set the window title
        self.window.set_title('Haircolor')
        # add a callback for exiting
        self.window.connect('delete_event', self.quit)
        # resize window
        self.window.set_size_request(400, 200)
        
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
        
        self.addbutton = gtk.Button(stock=gtk.STOCK_ADD)
        self.addbutton.connect('clicked', self.addentry)
        self.removebutton = gtk.Button(stock=gtk.STOCK_REMOVE)
        self.removebutton.connect('clicked', self.removeentry)

        self.box.attach(sw, 0, 3, 0, 1)
        self.box.attach(self.addbutton, 0, 1, 1, 2)
        self.box.attach(self.removebutton, 1, 2, 1, 2)
        self.box.attach(self.colorbutton, 2, 3, 1, 2)
        
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
        try:
            f = file(picklefile, 'r')
            vals = pickle.load(f)
            f.close()
        
            for key in vals.keys():
                l = [key, vals[key]]
                self.liststore.append(l)
        except IOError:
            self.liststore.append(['Jenny', '#FBFF89'])
            self.liststore.append(['Svetla', '#251832'])
        
    def save(self):
        vals = {}
        for i in self.liststore:
            name, color = i
            vals[name] = color
        
        # Save to a picklefile
        f = file(picklefile, 'w')
        pickle.dump(vals, f)
        f.close()
        
    
    def current_col(self):
        selection = self.treeview.get_selection()
        selected = selection.get_selected_rows()[1][0][0]

        return self.liststore[selected]
    
    def treeview_event(self, treeview, event):
        if event.button == 1:
            x = int(event.x)
            y = int(event.y)
            pthinfo = treeview.get_path_at_pos(x, y)
            if pthinfo != None:
                selected = pthinfo[0][0]
                current_col = self.liststore[selected]
                self.change_button_color(current_col[1])
    
    def change_button_color(self, color):
        """Changes the color of the button"""
        style = self.colorbutton.get_style().copy()
        cm = self.colorbutton.get_colormap()
        bgc = cm.alloc_color(color)
        style.bg[gtk.STATE_NORMAL] = bgc
        self.colorbutton.set_style(style) 
    
    def addentry(self, widget):
        result = self.input()
        if result != ['', '']:
            #self.liststore.append(['Niemand', '#FFFFFF'])
            self.liststore.append(result)
            
        for i in self.liststore:
            print i
    
    def input(self):
        dialog = gtk.Dialog("Input", self.window, 0,
            (gtk.STOCK_OK, gtk.RESPONSE_OK,
            gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL))
            
        hbox = gtk.HBox(False, 8)
        hbox.set_border_width(8)
        dialog.vbox.pack_start(hbox, False, False, 0)
    
        stock = gtk.image_new_from_stock(
            gtk.STOCK_DIALOG_QUESTION,
            gtk.ICON_SIZE_DIALOG)
        hbox.pack_start(stock, False, False, 0)
    
        table = gtk.Table(2, 2)
        table.set_row_spacings(4)
        table.set_col_spacings(4)
        hbox.pack_start(table, True, True, 0)
    
        label = gtk.Label("Name:")
        table.attach(label, 0, 1, 0, 1)
        local_entry1 = gtk.Entry()
        table.attach(local_entry1, 1, 2, 0, 1)
    
        label = gtk.Label("Haarfarbe:")
        table.attach(label, 0, 1, 1, 2)
        local_entry2 = gtk.Entry()
        table.attach(local_entry2, 1, 2, 1, 2)
    
        dialog.show_all()
        
        response = dialog.run()
        
        result = ['', '']
        if response == gtk.RESPONSE_OK:
            result = [local_entry1.get_text(),
                local_entry2.get_text().upper()]
        
        dialog.destroy()
        return result
    
    def removeentry(self, widget):
        selection = self.treeview.get_selection() 
        iter = selection.get_selected()[1]
        
        if iter:
            path = self.liststore.get_path(iter)
            self.liststore.remove(iter)

class Backend(object):
    def __init__(self):
        """This should"""
        pass
    
    def load(self):
        return {}
    
    def save(self, values):
        pass
    
class PickleBackend(Backend):
    picklefile = 'haircolor.pickle'
    
    def __init__(self):
        import pickle
        self.pickle = pickle
    
    def load(self):
        try:
            f = file(self.picklefile, 'r')
            vals = self.pickle.load(f)
            f.close()
            return vals
        except IOError:
            return {}
    
    def save(self, values):
        f = file(self.picklefile, 'w')
        self.pickle.dump(values, f)
        f.close()

class SQLiteBackend(Backend):
    dbfile = 'haircolor.sqlite'
    

def main():
    #MainWindow()
    #gtk.main()
    print PickleBackend().load()

if __name__ == '__main__':
    main()
