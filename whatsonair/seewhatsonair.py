#!/usr/bin/env python
# -*- encoding: latin-1 -*- 
"""See Whats On Air
A GUI for the WhatsOnAir backend library written using GTK+.

This frontend is written by Leonidas, the initiator of the 
WhatsOnAir project.

It reflects the most current parsers available, although it is
pretty minimalistic at the moment

Will use gtk.ListStore ant gtk.SpinButton (for time intervals)"""

import gtk
import whatsonair

class StationWindow(object):
    """The older UI"""
    def __init__(self):
        self.window = gtk.Window()
        self.window.set_title("What's on Air?")
        self.window.connect("delete_event", self.delete_event) 
        
        self.box = gtk.Table()
        
        self.stations = gtk.combo_box_new_text()
        self.track = gtk.Label('Click on Update')
        self.update = gtk.Button('Update')
        self.q = gtk.Button('Quit')
        self.populate()
        
        self.q.connect("clicked", lambda widget: gtk.main_quit())
        self.update.connect("clicked", self.update_click)
        
        self.box.attach(self.stations, 0, 1, 0, 1)
        self.box.attach(self.track, 0, 1, 1, 2)
        self.box.attach(self.update, 1, 2, 0, 1)
        self.box.attach(self.q, 1, 2, 1, 2)
        self.box.set_row_spacings(5)
        self.box.set_col_spacings(5)
        self.window.add(self.box)
        
        self.window.show_all()
    
    def delete_event(self, widget, event, data=None):
        """Window closing"""
        gtk.main_quit()
        return False
    
    def populate(self):
        """Fills that dropdown list"""
        # go though all available parsers
        for station in whatsonair.allparsers:
            # add the station name to the dropdown list
            self.stations.append_text(station.__station__)
        self.stations.set_active(0)
    
    def update_click(self, widget):
        """Updates the track,
        first fetches informations of rhe selected station"""
        active = self.stations.get_active()
        model = self.stations.get_model()
        selectedstation = model[active][0]
        
        for station in whatsonair.allparsers:
            if station.__station__ == selectedstation:
                #self.update_track(station)
                self.update.set_sensitive(False)
                self.stations.set_sensitive(False)
                self.track.set_text('Updating...')
                # use GTK pseudo threads
                gtk.idle_add(self.update_track, station)
    
    def update_track(self, parser):
        """Universal caption updater"""
        try:
            station = parser()
            station.feed(station.pagecontent)
            self.track.set_label(station.currenttrack())
        except whatsonair.IncompatibleParser:
            self.track.set_label('Update failed')
        except IOError:
            self.track.set_label('Network error')
        else:
            self.track.set_label(station.currenttrack())
            
        self.update.set_sensitive(True)
        self.stations.set_sensitive(True)
    
class TempNewUI(object):
    def __init__(self):
        import gobject
        self.window = gtk.Window()
        self.window.set_title("What's on Air?")
        self.window.connect("delete_event", self.delete_event) 
        
        self.box = gtk.Table()
        
        self.stations = gtk.ListStore(bool, str, str, str)
        iter = self.stations.append()
        print iter

        #self.stations.set(iter, 0, True, 1, "Bla", 2, "Bal", 3, "blub")
        #print dir(self.stations)
        sw = gtk.ScrolledWindow() 
        treeview = gtk.TreeView(self.stations) 
        #print treeview
        sw.add(treeview)
        renderer = gtk.CellRendererToggle()
        
        column = gtk.TreeViewColumn('Question?', renderer, active=0)
        treeview.append_column(column) 
        
        column = gtk.TreeViewColumn('Station', renderer)
        treeview.append_column(column) 
        column = gtk.TreeViewColumn('Artist', renderer)
        treeview.append_column(column) 
        column = gtk.TreeViewColumn('Title', renderer)
        treeview.append_column(column)
        
        self.stations.set(iter, 0, 'Foo')
        self.stations.append([False, 'red', "green", "blue"])
        
        self.track = gtk.Label('Click on Update')
        self.update = gtk.Button('Update')
        self.q = gtk.Button('Quit')
        
        self.q.connect("clicked", lambda widget: gtk.main_quit())
        self.update.connect("clicked", self.update_click)
        
        self.box.attach(sw, 0, 1, 0, 1)
        self.box.attach(self.track, 0, 1, 1, 2)
        self.box.attach(self.update, 1, 2, 0, 1)
        self.box.attach(self.q, 1, 2, 1, 2)
        self.box.set_row_spacings(5)
        self.box.set_col_spacings(5)
        self.window.add(self.box)
        
        self.window.show_all()
    
    def delete_event(self, widget, event, data=None):
        """Window closing"""
        gtk.main_quit()
        return False
    
    def update_click(self, widget):
        """Updates the track,
        first fetches informations of rhe selected station"""
        active = self.stations.get_active()
        model = self.stations.get_model()
        selectedstation = model[active][0]
        
        for station in whatsonair.allparsers:
            if station.__station__ == selectedstation:
                #self.update_track(station)
                self.update.set_sensitive(False)
                self.stations.set_sensitive(False)
                self.track.set_text('Updating...')
                # use GTK pseudo threads
                gtk.idle_add(self.update_track, station)
    
    def update_track(self, parser):
        """Universal caption updater"""
        try:
            station = parser()
            station.feed(station.pagecontent)
            self.track.set_label(station.currenttrack())
        except whatsonair.IncompatibleParser:
            self.track.set_label('Update failed')
        except IOError:
            self.track.set_label('Network error')
        else:
            self.track.set_label(station.currenttrack())
            
        self.update.set_sensitive(True)
        self.stations.set_sensitive(True)

def main():
    """The main method - just opens the window"""
    sw = TempNewUI()
    gtk.main()

if __name__ == '__main__':
    main()