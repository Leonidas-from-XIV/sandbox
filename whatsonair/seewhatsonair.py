#!/usr/bin/env python
# -*- encoding: latin-1 -*- 
"""See Whats On Air
A GUI for the library written using GTK+"""
import gtk
import whatsonair

class StationWindow(object):
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
        for station in whatsonair.allparsers:
            #print station.__station__
            self.stations.append_text(station.__station__)
        #self.stations.append_text('FM4')
        #self.stations.append_text('Antenne Bayern')
        #self.stations.append_text('Bayern 3')
        #self.stations.append_text('Gong')
        #self.stations.append_text('Energy')
        self.stations.set_active(0)
    
    def update_click(self, widget):
        active = self.stations.get_active()
        model = self.stations.get_model()
        station = model[active][0]
        if station == 'FM4':
            self.update_track(whatsonair.FM4Parser)
        elif station == 'Antenne Bayern':
            self.update_track(whatsonair.AntenneParser)
        elif station == 'Bayern 3':
            self.update_track(whatsonair.Bayern3Parser)
        elif station == 'Gong':
            self.update_track(whatsonair.GongParser)
        elif station == 'Energy':
            self.update_track(whatsonair.EnergyParser)
    
    def update_track(self, parser):
        """Universal Caption updater"""
        station = parser()
        station.feed(station.pagecontent)
        self.track.set_label(station.currenttrack())

def main():
    sw = StationWindow()
    gtk.main()

main()