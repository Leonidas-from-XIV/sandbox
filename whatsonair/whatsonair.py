#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A program for listing the songs currently on air.

Created by the What's On Air team, led by Marek Kubica.

This program is under GPL."""

import sys, os, os.path, optparse

__version__ = '0.8.8'

class IncompatibleParser(Exception):
    """An exception thrown when the parser is incompatible,
    i.e. causes an exception during parsing"""
    def __init__(self, parser, *args, **kwargs):
        """Constructor, also tells which parser is incompatible"""
        Exception.__init__(self, 'Incompatible %s parser, look for a newer version' % parser)

class PluginController(object):
    """This is the controller for all plugins.
    It imports the plugins from parser/ subdirectory and implements
    the Iterator-API so you can use it simply in for-loops.
    
    It was inspired by 2bock's plugin code, but nearly completely rewritten."""
    pluginlist = []

    def __init__(self):
        """The constructor, initializes the complete Controller, 
        does everything that is needed to be done."""
        # add the plugindir to the pythonpath
        sys.path.append(self.parser_dir())
        self.import_plugins()
        # remove the plugindir from the pythonpath
        sys.path.pop()

    def __iter__(self):
        """Iterates through the list of available plugins."""
        for module in self.pluginlist:
            try:
                if module.Parser != None:
                    yield module.Parser
            except AttributeError:
                # it is not really a valid plugin, it has no Parser attribute
                pass
    
    def parser_dir(self):
        """Gets the path of the directory where the parsers can be found."""
        try:
            appdir = os.path.dirname(__file__)
            if '.zip' in appdir:
                # frozen exe
                appdir = os.path.dirname(sys.argv[0])
        except NameError:
            # frozen exe, so no __file__
            appdir = os.path.dirname(sys.argv[0])
        pdir = os.path.join(appdir, 'parsers')
        return pdir
       
    def import_plugins(self):
        """The actual plugin importer. It is called automatically
        by the constructor, you should not call it by yourself."""
        path = self.parser_dir()
       
        for file in os.listdir(path):
            if file.endswith('.py') or file.endswith('.pyc'):
                plug = __import__(file.split('.')[0],globals(),locals(),[])
                if not plug in self.pluginlist:
                    self.pluginlist.append(plug)

def parser_chosen(option, opt, value, parser):
    """Called when a commandline options for a
    radio station was set. Disables the 'all' mode
    and sets the chosen station as 'to crawl'"""
    # disable crawling of all stations
    parser.values.all = False
    
    # get the name of the station
    station = opt[2:]
    
    try:
        # try to append the station to the list of stations to be crawled
        parser.values.stations.append(station)
    except AttributeError:
        # the list 'stations' was not found: create it
        parser.values.stations = []
        # and now append the station to that list
        parser.values.stations.append(station)

def main():
    """The main program. Initializes the Plugin-Controller,
    adds commandline options, binds them to callbacks,
    parses them and finally executes the actions that were
    requested by the user."""
    # create the plugin controller
    plugcontrol = PluginController()
    
    # create the option parser
    opts = optparse.OptionParser()
    # add some options
    opts.add_option("-v", "--version", dest="version", default=False,
        action="store_true", help="print program & parsers' versions and exit")
    opts.add_option("-d", "--descriptive", dest="descriptive", default=False,
        action="store_true", help="print station names")
    opts.add_option("-a", "--all", dest="all", default=True,
        action="store_true", help="question all stations (default)")
    
    allparsers = {}
    for parser in plugcontrol:
        call = parser.__station__.lower()
        allparsers[call] = parser
        opts.add_option("--" + call, action="callback", 
            callback=parser_chosen, help="question " + parser.__station__)
    
    options, args = opts.parse_args()
    
    if options.version:
        print "What's On Air \t%s" % __version__
        print 
        for parser in allparsers.values():
            print "%s Parser\t%s" % (parser.__station__, parser.__version__)
        
        sys.exit(0)
    
    if options.all:
        # go through all stations
        options.descriptive = True
        for parser in allparsers.values():
            print_current(parser, options.descriptive)
    else:
        # go just through selected stations
        for name, parser in allparsers.iteritems():
            # check whether to parse this particular station
            if name in options.stations:
                print_current(parser, options.descriptive)
        
def print_current(parser, descriptive):
    """Prints the current title playing on a station
    parser is a BaseParser derived class and descriptive
    tells whether it should be chatty. If chatty we also display
    the stations' name"""
    # initialize the parser (it does not matter what parser)
    current = parser()
    # feed the parser - it gets the needed informations from the internet
    current.feed()
    # let the parser parse the gotten inforamtions
    current.parse()
    
    if descriptive:
        # add the station title if requested
        print current.__station__,
    # print the station informations
    print current.current_track()

if __name__ == '__main__':
    main()
