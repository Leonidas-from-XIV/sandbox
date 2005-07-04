#!/usr/bin/env python
# -*- encoding: latin-1 -*-
import time, urllib
import gtk

config = {
    # choose a host - available hosts are:
    # soho.esac.esa.int (the ESA server)
    # sohowww.estec.esa.nl (an alias for the ESA' one)
    # plop.nascom.nasa.gov (the NASA server)
    # sohowww.nascom.nasa.gov (an alias for the NASA' one)
    'host' : 'soho.esac.esa.int',
    # choose a version
    # eit_171 - blue
    # eit_195 - green
    # eit_284 - yellow
    # eit_304 - normal
    # mdi_igr - MDI Continuum
    # mdi_mag - MDI Magnetogram
    # c2 - LASCO C2
    # c3 - LASCO C3
    'variation' : 'eit_304',
    # size
    # 256
    # 512
    # 1024
    'size' : '256',
    'file' : time.strftime('%Y-%m-%d_%H-%M.gif',
                                    time.gmtime())
}
#http://soho.esac.esa.int/data/realtime/eit_304/1024/latest.gif

class SOHOWindow(object):
    def __init__(self):
        # Create a window
        self.window = gtk.Window()
        # set the window title
        self.window.set_title('SOHO Fetcher')
        # add a callback for exiting
        self.window.connect('delete_event', gtk.main_quit)
        # resize window
        self.window.set_size_request(450, 500)
        # an icon
        pb = gtk.gdk.pixbuf_new_from_file('soho.ico') 
        self.window.set_icon(pb)
        
        vbox = gtk.VBox()
        hbox = gtk.HBox()
        
        download = gtk.Button('Download')
        download.connect('clicked', self.download)
        hbox.pack_start(download, True, True)
        self.shot = gtk.Image()
        self.shot.set_from_file('soho-noimage.png')
        vbox.pack_start(self.shot, True)
        vbox.pack_start(hbox, False)
        
        
        self.statusbar = gtk.Statusbar()
        self.context_id = self.statusbar.get_context_id('SOHOStatus')
        
        vbox.pack_end(self.statusbar, False)
        self.window.add(vbox)
        self.set_statusbar('Up and running')
        self.window.show_all()
    
    def download(self, widget):
        path = buildpath()
        self.set_statusbar(u'Öffnen von %s...' % path)
        
        # get a progressive loader (so we can feed the bytes)
        pbloader = gtk.gdk.PixbufLoader()
        
        #uf = urllib.urlopen(path)
        uf = file('soho256.gif', 'rb')
        self.set_statusbar(u'Laden von %s...' % path)
        
        # how many data have we got?
        recv = 0
        while True:
            # read one kilobyte at once
            data = uf.read(2 ** 10)
            # was that all data? If len = 0: yes, so break out
            if len(data) == 0: 
                break
            recv += len(data)
            self.set_statusbar(u'Laden (%dkb) von %s...' % (recv / 1024, path))
            
            # feed the data to the pixbuf loader
            pbloader.write(data)
            # get a pixbuf from our loader
            pixbuf = pbloader.get_pixbuf()
            # show the pixbuf on the screen
            self.shot.set_from_pixbuf(pixbuf)
            # update the screen
            gtk.main_iteration()
            
        uf.close()
        
        pbloader.close()
        pixbuf = pbloader.get_pixbuf()
        self.shot.set_from_pixbuf(pixbuf)
        self.set_statusbar('Up and running')
    
    def set_statusbar(self, text):
        self.statusbar.push(self.context_id, text) 
        
def main():
    buildpath()
    sw = SOHOWindow()
    gtk.main()

def buildpath():
    return "http://%(host)s/data/realtime/%(variation)s/%(size)s/latest.gif" % config

if __name__ == '__main__':
    main()