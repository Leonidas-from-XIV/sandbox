#!/usr/bin/env python
# -*- encoding: latin-1 -*- 
"""See Whats On Air
A GUI for the WhatsOnAir backend library written using GTK+.

This frontend is written by Leonidas, the initiator of the 
WhatsOnAir project.

The interface is pretty simple: you have a list of radio 
stations, an interval chooser and a manual update button."""

import pickle
import gtk, gobject
import whatsonair

picklefile = 'seewhatsonair.pickle'

class UserInterface(object):
    """The graphical user interface.
    This class represents the main window"""
    
    def __init__(self):
        """Constructor of main window.
        Builds the UI elements"""
        # create a window
        self.window = gtk.Window()
        # set the title
        self.window.set_title("What's on Air?")
        # add an event, when clicking on the close-button
        self.window.connect('delete_event', self.delete_event)
        # set a default size for the window
        self.window.set_size_request(500, 400)
        
        # create a layout manager: Table
        self.box = gtk.Table()
        
        # create a scrolled window - so users can scroll the display
        #+when the text is too long. 
        sw = gtk.ScrolledWindow()
        # set the shadow type
        sw.set_shadow_type(gtk.SHADOW_ETCHED_IN)
        # and when to display the scrollbars: when needed
        sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        
        # create a liststore
        self.model = gtk.ListStore(bool, str, str, str)
        # and generate a treeview aut of that liststore
        self.treeview = gtk.TreeView(self.model)
        # enable autosizing for the columns, very nice
        self.treeview.columns_autosize()
        # add the treeview to the scrolled window
        sw.add(self.treeview)
        
        # add the columns to the treeview
        for i in range(4):
            if i == 0:
                # add a toggle cell
                renderer = gtk.CellRendererToggle()
                # connect it to a callback
                renderer.connect('toggled', self.quest_toggled, self.model)
                # create the column
                column = gtk.TreeViewColumn('Question', renderer, active=i)
            elif i == 1:
                # add a text cell
                renderer = gtk.CellRendererText()
                # create the column
                column = gtk.TreeViewColumn('Station', renderer, text=i)
            elif i == 2:
                # add a text cell
                renderer = gtk.CellRendererText()
                # create the column
                column = gtk.TreeViewColumn('Artist', renderer, text=i)
            elif i == 3:
                # add a text cell
                renderer = gtk.CellRendererText()
                # create the column
                column = gtk.TreeViewColumn('Title', renderer, text=i)
            
            # append the created column to the treeview
            self.treeview.append_column(column)
        
        # initialize the plugin controller - for accessing the plugins
        self.plugcon = whatsonair.PluginController()
        
        # load the configfile
        try:
            # open the file
            f = file(picklefile, 'r')
            # restore the settings
            states = pickle.load(f)
            # close the file
            f.close()
        except IOError:
            # file not found
            #+so make a dictionary where all values are set to False
            states = {}
            for plugin in self.plugcon:
                # plugin.__station__
                states[plugin.__station__] = False
        
        # add the stations to the tree view
        for plugin in self.plugcon:
            # append the station at the end of the treeview
            iterator = self.model.append()
            # set the name of the station
            self.model.set_value(iterator, 1, plugin.__station__)
            
            # set the question state of the station
            self.model.set_value(iterator, 0, states[plugin.__station__])
        
        # add update button and bind a clicked callback
        self.update = gtk.Button('Manual update')
        self.update.connect('clicked', self.update_click)
        
        # defualt interval for updating is ten seconds
        default_interval = 10
        # create an anjustment - with some default values
        adj = gtk.Adjustment(value=default_interval, lower=1, upper=3600, step_incr=1, page_incr=5, page_size=0)
        # create a spinbutton widget
        self.interval = gtk.SpinButton(adj, 0, 0)
        # set the widget to numeric input only
        self.interval.set_numeric(True)
        # add a callback: called when the interval was changed
        adj.connect('value_changed', self.interval_changed) 
        # create a pseudothread
        self.sid = gobject.timeout_add(default_interval * 1000, self.cyclic_update)
        
        # add the scrolledwindow to the layout manager
        self.box.attach(sw, 0, 2, 0, 1)
        # add the spinbutton
        self.box.attach(self.interval, 0, 1, 1, 2, yoptions=gtk.SHRINK)
        # add the button
        self.box.attach(self.update, 1, 2, 1, 2, yoptions=gtk.SHRINK)
        # set spacings for rows and columns
        self.box.set_row_spacings(5)
        self.box.set_col_spacings(5)
        # add the layout manager to the window
        self.window.add(self.box)
        # show all widgets on the window
        self.window.show_all()
    
    def delete_event(self, widget, event, data=None):
        """Window closing"""
        # save the checked stations
        states = {}
        for station in self.model:
            states[station[1]] = station[0]
        
        # open the configfile
        f = file(picklefile, 'w')
        # dump the variables into the configfile
        pickle.dump(states, f, pickle.HIGHEST_PROTOCOL)
        # close the configfile
        f.close()
        
        # exit the main loop
        gtk.main_quit()
        return False
    
    def quest_toggled(self, cell, path, model):
        """The handler for clicking on the checkmark"""
        # get toggled iterator
        iterator = model.get_iter((int(path),))
        
        quest = model.get_value(iterator, 0)
        station = model.get_value(iterator, 1)
    
        # swich the value to the negative
        quest = not quest
    
        # set new value
        model.set(iterator, 0, quest) 
        
        # update the values of this station
        gobject.idle_add(self.update_stations)
    
    def interval_changed(self, widget):
        """The interval in the widget was changed.
        So stop the current timeout-job and start a new with
        the new timeout interval"""
        new_value = int(self.interval.get_value())
        interval = new_value * 1000
        
        # stop the former microthread
        gobject.source_remove(self.sid)
        # start the new microthread
        self.sid = gobject.timeout_add(interval, self.cyclic_update)
    
    def cyclic_update(self):
        """Called cyclic to update the stations"""
        # display the current pseudo-thread ID
        #print 'Timeout #%d' % self.sid
        
        # update the stations
        gobject.idle_add(self.update_stations)
        
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
        
        for station in self.plugcon:
            if station.__station__ == selectedstation:
                #self.update_track(station)
                self.update.set_sensitive(False)
                # use GTK pseudo threads
                gobject.idle_add(self.update_track, station, iterator)
    
    def update_track(self, parser, iterator):
        """Universal caption updater"""
        try:
            station = parser()
            station.feed()
            station.parse()
            track = station.current_track()
            
            if track == None:
                self.update.set_label('No song currently')
                self.update.set_sensitive(True)
                return
            
            # the splitted track: may be just one part
            tr = track.split(' - ', 1)
            try:
                self.model.set_value(iterator, 2, tr[0])
                self.model.set_value(iterator, 3, tr[1])
                self.update.set_label('Manual update')
            except IndexError:
                # Ignore index-errors
                pass
            
        except IOError:
            self.update.set_label('Network error')
        
        self.update.set_sensitive(True)
        # do not start periodically
        return False
    
    def update_stations(self):
        """Updates the selected stations"""
        for row in self.model:
            # go though all rows
            statname = row[1]
            update = row[0]
            if update:
                parser = None
                for station in self.plugcon:
                    if station.__station__ == statname:
                        parser = station
                station = parser()
                station.feed()
                station.parse()
                try:
                    tr = station.current_track().split(' - ', 1)
                    row[2] = tr[0]
                    row[3] = tr[1]
                    self.update.set_label('Manual update')
                except AttributeError:
                    # ignore errors when station information cannot be collected
                    pass
                except IndexError:
                    # ignore errors when just one field is filled
                    pass

def main():
    """The main method - just opens the window"""
    # create the interface
    sw = UserInterface()
    # enter mainloop
    gtk.main()

if __name__ == '__main__':
    main()