#!/usr/bin/env python
# -*- encoding: utf-8 -*-
"""A small sampler using PyGTK for the user interface.

In case you don't have PyGTK already get it from http://www.pygtk.org/
or - even better - let your package manager install it."""

import ConfigParser, logging, textwrap, sys, os
import gtk, gobject

class Configuration(object):
    def __init__(self, configfile):
        """Opens the configfile and reads the samples"""
        try:
            f = file(configfile, 'r')
            self.config = ConfigParser.SafeConfigParser()
            self.config.readfp(f)
            f.close()
        except IOError:
            self.create_empty(configfile)
            print 'Created a %s for you, go and edit it.' % configfile
            sys.exit(1)

        self.samples = {}
        for number in range(1, 10):
            number = str(number)
            try:
                self.samples[number] = self.config.get('Samples', number)
            except ConfigParser.NoOptionError:
                self.samples[number] = None

    def create_empty(self, filename):
        """Creates an empty configfile"""
        sample_conf = """\
        [Samples]
        1: file:///800.ogg
        2: file:///800.ogg
        3: file:///800.ogg
        4: file:///800.ogg
        5: file:///800.ogg
        6: file:///800.ogg
        7: file:///800.ogg
        8: file:///800.ogg
        9: file:///800.ogg"""
        sample_conf = textwrap.dedent(sample_conf)
        config_file = file(filename, 'w')
        config_file.write(sample_conf)
        config_file.close()

class SoundDriver(object):
    """A simple sound driver"""
    def __init__(self):
        raise NotImplementedError('Virtual Class')

    def play(self, url):
        """Parse file:///, freq:// and silence:// URLs"""
        # just sample implementation
        if url.startswith('file:///'):
            self.play_sample(url)
        else:
            raise NotImplementedError('These URLs are not supported yet')

    def play_sample(self, samplefile):
        """Plays a sample loaded from disk"""
        raise NotImplementedError('Virtual class')

    def play_freq(self, frequency, lenght):
        """Plays a frequency at a time"""
        raise NotImplementedError('The author does not know how to play these')

class SonicDriver(SoundDriver):
    """Driver for pySonic output
    get it from http://pysonic.sourceforge.net/"""

    def __init__(self):
        """Initializes pySonic"""
        import pySonic
        self.pySonic = pySonic

        self.world = pySonic.World()
        self.src = pySonic.Source()

    def play_sample(self, samplefile):
        """Plays a sample loaded from disk"""
        try:
            self.src.Sound = self.pySonic.FileSample(samplefile)
            self.src.Play()
        except self.pySonic.FMODError:
            # maybe the file was not found.. do nothing
            pass

class GstDriver(SoundDriver):
    def __init__(self):
        """Initialize the Gstreamer driver"""
        import gst
        self.gst = gst
        self.player = gst.element_factory_make('playbin', 'samplitude')
        #fakesink = gst.element_factory_make('fakesink', "my-fakesink")
        #self.player.set_property("video-sink", fakesink)

        bus = self.player.get_bus()
        bus.add_signal_watch()
        bus.connect('message', self.on_message)

    def play_sample(self, samplefile):
        """Play a sample loaded from the file system"""
        self.player.set_property('uri', samplefile)
        self.player.set_state(self.gst.STATE_PLAYING)

    def on_message(self, bus, message):
        """Handler for Gstreamer messages"""
        if message.type == self.gst.MESSAGE_ERROR:
            # we need some better handling for this
            print message

class NumpadWindow(object):
    """The window displaying the numpad buttons on the screen.
    Most of the work happens here."""

    # this is how the numpad looks like
    keypad = ['7', '8', '9',
              '4', '5', '6',
              '1', '2', '3']

    def __init__(self, sdriver, conf):
        """Initialize the window"""
        self.sd = sdriver
        self.conf = conf

        self.window = gtk.Window()
        self.window.connect('delete_event', gtk.main_quit)
        self.window.connect('key_press_event', self.key_typed)
        self.window.set_title('Samplitude')
        self.window.set_size_request(200, 200)

        self.layout = gtk.Table()

        # create the buttons
        self.samplerbuttons = {}
        inrow = 0
        row = 0
        for i in self.keypad:
            cmd = gtk.Button(i)
            cmd.connect('clicked', self.button_activated)
            self.samplerbuttons[i] = cmd

            self.layout.attach(cmd, inrow, inrow + 1, row, row + 1)

            inrow += 1
            if inrow > 2:
                row += 1
                inrow = 0

        self.window.add(self.layout)
        self.window.show_all()

    def button_activated(self, widget):
        """Is called when a button was activated:
        either by clicking or by typing a key"""
        number = widget.get_label()
        logging.debug(number)
        sample = self.conf.samples[number]
        logging.debug(sample)

        self.sd.play(sample)

    def key_typed(self, widget, event):
        """The user typed a key"""
        # get the string of that key
        key = event.string
        try:
            button = self.samplerbuttons[key]
            button.activate()
        except KeyError:
            # no numpad key pressed. Do nothing
            pass

def main():
    """The starter"""
    log_to_file = False
    log_filename = 'samplitude.log'
    loglevel = logging.DEBUG

    if log_to_file:
        logging.basicConfig(level=loglevel,
                            filename=log_filename,
                            filemode='w',
                            format='%(levelname)s: %(message)s')
        console = logging.StreamHandler()
        console.setLevel(loglevel)
        formatter = logging.Formatter('%(levelname)s: %(message)s')
        console.setFormatter(formatter)
        logging.getLogger('').addHandler(console)

        logging.debug('Logging to console & file')
    else:
        logging.basicConfig(level=loglevel,
                            format='%(levelname)s: %(message)s')

    logging.info('Program started')

    conf = Configuration('samplitude.ini')

    if sys.platform.startswith('win'):
        sd = SonicDriver()
    else:
        sd = GstDriver()

    numwin = NumpadWindow(sdriver=sd, conf=conf)
    # call the GTK mainloop
    gtk.main()

if __name__ == '__main__':
    # let's start rocking!
    main()
