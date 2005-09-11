#!/usr/bin/env python
# -*- encoding: latin-1 -*-
import pySonic
import gtk, gobject

class SampleWindow(object):
    keypad = [7, 8, 9,
                    4, 5, 6,
                    1, 2, 3]
    def __init__(self):
        self.window = gtk.Window()
        self.window.connect('delete_event', gtk.main_quit)
        self.window.connect('key_press_event', self.typed)
        self.window.set_title('Samplesonic')
        self.window.set_size_request(200, 200)
        self.layout = gtk.Table()
        
        self.sonic_init()
        
        # create the buttons
        self.samplerbuttons = {}
        inrow = 0
        row = 0
        for i in self.keypad:
            cmd = gtk.Button(str(i))
            cmd.connect('clicked', self.samplebutton)
            self.samplerbuttons[str(i)] = cmd
        
            self.layout.attach(cmd, inrow, inrow + 1, row, row + 1)
            
            inrow += 1
            if inrow > 2:
                row += 1
                inrow = 0
            
        self.window.add(self.layout)
        self.window.show_all()
    
    def sonic_init(self):
        self.world = pySonic.World()
        self.src = pySonic.Source()
    
    def samplebutton(self, widget):
        #print widget
        sound = widget.get_label()
        print sound
        
        self.src = pySonic.Source()
        self.src.Sound = pySonic.FileSample('800.ogg')
        #self.src.Sound = pySonic.MemorySample("""Zats""" * 800, 1, 8, 800)
        self.src.Play()
    
    def typed(self, widget, event):
        """The user typed a key"""
        key = event.string
        button = self.samplerbuttons[key]
        button.activate()
        

def main():
    sw = SampleWindow()
    gtk.main()

if __name__ == '__main__':
    main()