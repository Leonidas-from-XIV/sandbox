#!/usr/bin/env python
# -*- encoding: latin-1 -*- 
"""See Whats On Air
A GUI for the WhatsOnAir backend library written using GTK+.

This frontend is written by Leonidas, the initiator of the 
WhatsOnAir project.

The interface is pretty simple: you have a list of radio 
stations, an interval chooser and a manual update button."""

import gtk, gobject
import whatsonair
    
class UserInterface(object):
    def __init__(self):
        self.window = gtk.Window()
        self.window.set_title("What's on Air?")
        self.window.connect('delete_event', self.delete_event) 
        
        self.box = gtk.Table()
        
        sw = gtk.ScrolledWindow()
        sw.set_shadow_type(gtk.SHADOW_ETCHED_IN)
        sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        
        self.model = gtk.ListStore(bool, str, str, str)
        self.treeview = gtk.TreeView(self.model) 
        self.treeview.columns_autosize()
        sw.add(self.treeview)
        
        for i in range(4):
            if i == 0:
                renderer = gtk.CellRendererToggle()
                renderer.connect('toggled', self.quest_toggled, self.model)
                column = gtk.TreeViewColumn('Question', renderer, active=i)
            elif i == 1:
                renderer = gtk.CellRendererText()
                column = gtk.TreeViewColumn('Station', renderer, text=i)
            elif i == 2:
                renderer = gtk.CellRendererText()
                column = gtk.TreeViewColumn('Artist', renderer, text=i)
            elif i == 3:
                renderer = gtk.CellRendererText()
                column = gtk.TreeViewColumn('Title', renderer, text=i)
            self.treeview.append_column(column)
        
        for st in whatsonair.allparsers:
            iterator = self.model.append()
            self.model.set_value(iterator, 1, st.__station__)
        
        
        self.track = gtk.Label('Click on Update')
        self.update = gtk.Button('Manual Update')
        
        adj = gtk.Adjustment(value=10, lower=1, upper=3600, step_incr=1, page_incr=5, page_size=0) 
        self.interval = gtk.SpinButton(adj, 0, 0)
        self.interval.set_numeric(True)
        adj.connect('value_changed', self.interval_changed) 
        self.sid = gobject.timeout_add(10 * 1000, self.cyclic_update)
        
        self.update.connect('clicked', self.update_click)
        
        self.box.attach(sw, 0, 2, 0, 1)
        self.box.attach(self.interval, 0, 1, 1, 2, yoptions=gtk.SHRINK)
        self.box.attach(self.update, 1, 2, 1, 2, yoptions=gtk.SHRINK)
        self.box.set_row_spacings(5)
        self.box.set_col_spacings(5)
        self.window.add(self.box)
        
        self.window.show_all()
    
    def delete_event(self, widget, event, data=None):
        """Window closing"""
        gtk.main_quit()
        return False
    
    def quest_toggled(self, cell, path, model):
        """The handler for clicking on the checkmark"""
        # get toggled iterator
        iterator = model.get_iter((int(path),))
        
        quest = model.get_value(iterator, 0)
        station = model.get_value(iterator, 1)
        #print station
        #print "Currently ", quest, " changed to ",
    
        # swich the value to the negative
        quest = not quest
    
        # set new value
        model.set(iterator, 0, quest) 
        
        # update the values of this station
        gobject.idle_add(self.updatestations)
    
    def interval_changed(self, widget):
        """The interval in the widget was changed.
        So stop the current timeout-job and start a new with
        the new timeout interval"""
        #print widget
        new_value = int(self.interval.get_value())
        interval = new_value * 1000
        
        # stop the former microthread
        gobject.source_remove(self.sid)
        # start the new microthread
        self.sid = gobject.timeout_add(interval, self.cyclic_update)
    
    def cyclic_update(self):
        print 'Timeout #%s' % str(self.sid)
        gobject.idle_add(self.updatestations)
        
        # True - wait until the next timeout, False - break
        return True
        #return False
    
    def update_click(self, widget):
        """Updates the track,
        first fetches informations of the selected station"""
        # get the selection
        selection = self.treeview.get_selection()
        model, selected = selection.get_selected_rows()
        
        # now the iterator mapping to that selection
        iterator = model.get_iter((selected[0][0],))
        # and the station name
        selectedstation = model.get_value(iterator, 1)
        
        for station in whatsonair.allparsers:
            if station.__station__ == selectedstation:
                #self.update_track(station)
                self.update.set_sensitive(False)
                self.track.set_text('Updating...')
                # use GTK pseudo threads
                gobject.idle_add(self.update_track, station, iterator)
    
    def update_track(self, parser, iterator):
        """Universal caption updater"""
        try:
            station = parser()
            station.feed(station.pagecontent)
            tr = station.currenttrack().split(' - ', 1)
            self.model.set_value(iterator, 2, tr[0])
            self.model.set_value(iterator, 3, tr[1])
        except whatsonair.IncompatibleParser:
            self.update.set_label('Update failed')
        except IOError:
            self.update.set_label('Network error')
        except IndexError:
            pass
            
        self.update.set_sensitive(True)
        # do not start periodically
        return False
    
    def updatestations(self):
        """Updates the selected stations"""
        for row in self.model:
            # go though all rows
            statname = row[1]
            update = row[0]
            if update:
                parser = None
                for station in whatsonair.allparsers:
                    if station.__station__ == statname:
                        parser = station
                station = parser()
                try:
                    station.feed(station.pagecontent)
                    tr = station.currenttrack().split(' - ', 1)
                    row[2] = tr[0]
                    row[3] = tr[1]
                except whatsonair.IncompatibleParser:
                    row[2] = 'Incompatible'
                    row[3] = 'Parser'
                    row[0] = False
                except IOError:
                    row[2] = 'Network'
                    row[3] = 'Error'
                    row[0] = False
                

def main():
    """The main method - just opens the window"""
    sw = UserInterface()
    gtk.main()

if __name__ == '__main__':
    main()