#!/usr/bin/env python
# -*- encoding: latin-1 -*-
import gtk, gtk.gdk

class MainWindow(object):
    def __init__(self):
        """Set up the main window"""
        # Create a window
        self.window = gtk.Window()
        # set the window title
        self.window.set_title('Achromatic')
        # add a callback for exiting
        self.window.connect('delete_event', self.quit)
        # resize window
        self.window.set_size_request(400, 200)
        
        # the layout managers
        self.vbox = gtk.VBox()
        self.hbox = gtk.HBox()
        
        # create a scrolledwindow, so the content can be scrolled
        sw = gtk.ScrolledWindow()
        sw.set_shadow_type(gtk.SHADOW_ETCHED_IN)
        sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        
        # a liststoe
        self.liststore = gtk.ListStore(str, str)
        # a treeview
        self.treeview = gtk.TreeView(self.liststore)
        # some columns
        self.tvcolumn1 = gtk.TreeViewColumn('Name')
        self.tvcolumn2 = gtk.TreeViewColumn('Farbe')
        # the clicked event
        self.treeview.connect('button_press_event', self.treeview_click)
        
        # add these columns
        self.treeview.append_column(self.tvcolumn1)
        self.treeview.append_column(self.tvcolumn2)
        # add the treeview to the ScrolledWindow
        sw.add(self.treeview)
        
        # the cell renderers for text entries
        self.cell1 = gtk.CellRendererText()
        self.cell2 = gtk.CellRendererText()
        
        # add them to the columns
        self.tvcolumn1.pack_start(self.cell1, True)
        self.tvcolumn2.pack_start(self.cell2, True)
        
        # set the attributes of the collumns
        # first one is the text
        self.tvcolumn1.set_attributes(self.cell1, text=0)
        # second one just background color
        self.tvcolumn2.set_attributes(self.cell2, background=1)
        
        # a button
        self.colorbutton = gtk.Button('_Farbe')
        self.colorbutton.connect('clicked', self.choosecolor)
        
        self.addbutton = gtk.Button(stock=gtk.STOCK_ADD)
        self.addbutton.connect('clicked', self.addentry)
        self.removebutton = gtk.Button(stock=gtk.STOCK_REMOVE)
        self.removebutton.connect('clicked', self.removeentry)

        self.vbox.pack_start(sw, True, True)
        self.vbox.pack_start(self.hbox, False, False)
        self.hbox.pack_start(self.addbutton)
        self.hbox.pack_start(self.removebutton)
        self.hbox.pack_start(self.colorbutton)
        
        self.window.add(self.vbox)
        
        self.choosebackend()
        self.load()
        
        self.window.show_all()
    
    def choosebackend(self):
        """Selects the backend and sets it"""
        try:
            self.backend = SQLiteBackend()
        except:
            self.backend = PickleBackend()
    
    def choosecolor(self, widget):
        self.colorseldlg = gtk.ColorSelectionDialog('Choose Color')
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
        vals = self.backend.load()
        
        if vals != {}:
            for key in vals.keys():
                l = [key, vals[key]]
                self.liststore.append(l)
        else:
            self.liststore.append(['Jenny', '#FBFF89'])
            self.liststore.append(['Svetla', '#251832'])
        
    def save(self):
        vals = {}
        for i in self.liststore:
            name, color = i
            vals[name] = color
        
        self.backend.save(vals)
        
    
    def current_col(self):
        selection = self.treeview.get_selection()
        selected = selection.get_selected_rows()[1][0][0]

        return self.liststore[selected]
    
    def treeview_click(self, treeview, event):
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
    
        label = gtk.Label("Farbe:")
        table.attach(label, 0, 1, 1, 2)
        local_entry2 = gtk.Entry()
        local_entry2.set_text('#')
        table.attach(local_entry2, 1, 2, 1, 2)
    
        dialog.show_all()
        
        # Run the dialog as long as we get wrong entries
        goodentry = False
        while not goodentry:
            response = dialog.run()
        
            result = ['', '']
            if response == gtk.RESPONSE_OK:
                name = local_entry1.get_text()
                color = local_entry2.get_text().upper()
                try:
                    gtk.gdk.color_parse(color)
                    result = [name, color]
                    goodentry = True
                except ValueError:
                    # not a Hexcolor
                    d = gtk.MessageDialog(dialog, gtk.DIALOG_MODAL, 
                        gtk.MESSAGE_ERROR, gtk.BUTTONS_OK, 
                        'Die angegebene Farbe wurde nicht gefunden. Sie sollte im #RRGGBB-Format sein')
                
                    # show the error
                    d.run()
                    # destroy the messagebox
                    d.destroy()
                    local_entry2.set_text('#')
                    
            elif response == gtk.RESPONSE_CANCEL:
                goodentry = True
        
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
    picklefile = 'achromatic.pickle'
    
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
    dbfile = 'achromatic.sqlite'
    
    def __init__(self):
        from pysqlite2 import dbapi2 as sqlite
        self.sqlite = sqlite
        
        self.con = sqlite.connect(self.dbfile)
        self.cur = self.con.cursor()
        self.cur.execute("SELECT * FROM sqlite_master WHERE type='table' AND name='colors'")
        found = self.cur.fetchall()
        if found == []:
            # create table
            self.cur.execute("CREATE TABLE colors (name TEXT, color TEXT)")
            self.con.commit()

    def load(self):
        self.cur.execute("SELECT * FROM colors")
        vals = self.cur.fetchall()
        if vals == []:
            return {}
        else:
            data = {}
            for name, color in vals:
                name, color = str(name), str(color)
                data[name] = color
            return data
    
    def save(self, values):
        # first delete all old records
        self.cur.execute("DELETE FROM colors")
        
        for key in values.keys():
            color = values[key]
            cmd = "INSERT INTO colors (name,color) values ('%(key)s','%(color)s')" % locals()
            #cmd = "INSERT INTO colors (name,color) VALUES (%s,%s)"
            #print cmd
            #self.cur.execute(cmd, locals())
            self.cur.execute(cmd)
        
        self.con.commit()

def main():
    MainWindow()
    gtk.main()
    

if __name__ == '__main__':
    main()
