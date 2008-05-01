#!/usr/bin/env python
# -*- encoding: latin-1 -*-
import time, urllib
import gtk

# choose a host - available hosts are:
# soho.esac.esa.int (the ESA server)
# sohowww.estec.esa.nl (an alias for the ESA' one)
# plop.nascom.nasa.gov (the NASA server)
# sohowww.nascom.nasa.gov (an alias for the NASA' one)
# choose a version
# eit_171 - blue
# eit_195 - green
# eit_284 - yellow
# eit_304 - normal
# mdi_igr - MDI Continuum
# mdi_mag - MDI Magnetogram
# c2 - LASCO C2
# c3 - LASCO C3
# size
# 256
# 512
# 1024


class SOHOWindow(object):
    def __init__(self):
        # Create a window
        self.window = gtk.Window()
        # set the window title
        self.window.set_title('SOHO Fetcher')
        # add a callback for exiting
        self.window.connect('delete_event', gtk.main_quit)
        # resize window
        self.window.set_size_request(650, 510)
        # an icon
        pb = gtk.gdk.pixbuf_new_from_file('soho.ico') 
        self.window.set_icon(pb)
        
        vbox = gtk.VBox()
        hbox = gtk.HBox()
        vbox.pack_start(hbox, True)
        
        sw = gtk.ScrolledWindow()
        sw.set_shadow_type(gtk.SHADOW_ETCHED_IN)
        sw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        
        self.shot = gtk.Image()
        sw.add_with_viewport(self.shot)
        self.shot.set_from_file('soho-noimage.png')
        
        hbox.pack_start(sw, True)
        
        # menu of the right
        rmenu = gtk.VBox()
        
        # the hosts
        rmenu.pack_start(gtk.Label('Host:'), False)
        self.hosts = gtk.combo_box_new_text()
        self.hosts.append_text('soho.esac.esa.int')
        self.hosts.append_text('sohowww.estec.esa.nl')
        self.hosts.append_text('plop.nascom.nasa.gov')
        self.hosts.append_text('sohowww.nascom.nasa.gov')
        self.hosts.set_active(0)
        rmenu.pack_start(self.hosts, False)
        
        # the variation
        rmenu.pack_start(gtk.Label('Variation:'), False)
        self.variation = gtk.combo_box_new_text()
        self.variation.append_text('eit_171')
        self.variation.append_text('eit_195')
        self.variation.append_text('eit_284')
        self.variation.append_text('eit_304')
        self.variation.append_text('mdi_igr')
        self.variation.append_text('mdi_mag')
        self.variation.append_text('c2')
        self.variation.append_text('c3')
        self.variation.set_active(3)
        rmenu.pack_start(self.variation, False)
        
        # the variation
        rmenu.pack_start(gtk.Label('Size:'), False)
        self.size = gtk.combo_box_new_text()
        self.size.append_text('256')
        self.size.append_text('512')
        self.size.append_text('1024')
        self.size.set_active(1)
        rmenu.pack_start(self.size, False)
        
        download = gtk.Button('Download')
        download.connect('clicked', self.download)
        
        rmenu.pack_start(download, False)
        hbox.pack_start(rmenu, False)
        
        self.statusbar = gtk.Statusbar()
        self.context_id = self.statusbar.get_context_id('SOHOStatus')
        
        vbox.pack_end(self.statusbar, False)
        self.window.add(vbox)
        self.set_statusbar('Up and running')
        self.window.show_all()
    
    def download(self, widget):
        # bufzize in bytes
        bufsize = 4 * 2 ** 10
        path = self.buildpath()
        self.set_statusbar(u'Öffnen von %s...' % path)
        
        # get a progressive loader (so we can feed the bytes)
        pbloader = gtk.gdk.PixbufLoader()
        
        uf = urllib.urlopen(path)
        #uf = file('soho256.gif', 'rb')
        self.set_statusbar(u'Laden von %s...' % path)
        
        # how many data have we got?
        recv = 0
        while True:
            # read one kilobyte at once
            data = uf.read(bufsize)
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
    
    def buildpath(self):
        settings = {
            'host' : self.hosts.get_model()[self.hosts.get_active()][0],
            'variation' : self.variation.get_model()[self.variation.get_active()][0],
            'size' : self.size.get_model()[self.size.get_active()][0]
                }
        return "http://%(host)s/data/realtime/%(variation)s/%(size)s/latest.gif" % settings
        
def main():
    sw = SOHOWindow()
    gtk.main()

if __name__ == '__main__':
    main()
