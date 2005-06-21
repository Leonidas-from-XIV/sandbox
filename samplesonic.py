#!/usr/bin/env python
# -*- encoding: latin-1 -*-
import pySonic
import gtk

class SampleWindow(object):
    keypad = [ 7, 8, 9,
                    4, 5, 6,
                    1, 2, 3]
    def __init__(self):
        self.window = gtk.Window()
        self.window.connect('delete_event', gtk.main_quit)
        self.window.set_title('Achromatic')
        self.window.set_size_request(300, 300)
        self.layout = gtk.Table()
        
        self.sonic_init()
        
        # create the buttons
        inrow = 0
        row = 0
        for i in self.keypad:
            cmd = gtk.Button('_'+ str(i))
            cmd.connect('clicked', self.samplebutton)
        
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
        sound = widget.get_label()[1:]
        
        self.src = pySonic.Source()
        self.src.Sound = pySonic.FileSample('hell.mp3')
        self.src.Play()
        

def main():
    sw = SampleWindow()
    gtk.main()

if __name__ == '__main__':
    main()