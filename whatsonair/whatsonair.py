#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A program for listing the songs currently on air
This program is under GPL"""

import sys, os, os.path, optparse

__version__ = '0.8.6'

def splitver(version):
    """Splits the string representation of the version into a tuple form,
    so it's easier to parse"""
    return tuple(version.split('.'))

__versiontuple__ = splitver(__version__)

class IncompatibleParser(Exception):
    """An exception thrown when the parser is incompatible,
    i.e. causes an exception during parsing"""
    def __init__(self, parser, *args, **kwargs):
        """Constructor, also tells which parser is incompatible"""
        Exception.__init__(self, 'Incompatible %s parser, look for a newer version' % parser)

class PluginController(object):
    """Heavily modified 2bock code"""
    pluginlist = []

    def __init__(self):
        """The constructor"""
        # add the plugindir to the pythonpath
        sys.path.append(self.parserdir())
        self.import_plugins()
        # remove the plugindir from the pythonpath
        sys.path.pop()

    def __iter__(self):
        """An iterator"""
        for module in self.pluginlist:
            try:
                if module.Parser != None:
                    yield module.Parser
            except AttributeError:
                pass
    
    def parserdir(self):
        """Gets the path of the directory where the parsers can be found"""
        appdir = os.path.dirname(__file__)
        pdir = os.path.join(appdir, 'parsers')
        return pdir
       
    def import_plugins(self):
        path = self.parserdir()
       
        for file in os.listdir(path):
            if file.endswith('.py') or file.endswith('.pyc'):
                plug = __import__(file.split('.')[0],globals(),locals(),[])
                if not plug in self.pluginlist:
                    self.pluginlist.append(plug)
   
    def get_plugins(self):
        for module in self.pluginlist:
            if not module.Parser == None:
                instance = module.Parser()
                print instance.__station__
    


def parser_chosen(option, opt, value, parser):
    """Called when a commandline options for a
    radio station was set. Disables the 'all' mode
    and sets the chosen station as 'to crawl'"""
    # disable crawling of all stations
    parser.values.all = False
    
    # get the name of the station
    station = opt[2:]
    
    try:
        parser.values.stations.append(station)
    except AttributeError:
        parser.values.stations = []
        parser.values.stations.append(station)
    

def main():
    """The main program"""
    # Create the plugin controller
    plugcontrol = PluginController()
    
    opts = optparse.OptionParser()
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
        print "WhatsOnAir \t%s" % __version__
        print 
        for parser in allparsers.values():
            print "%s Parser\t%s" % (parser.__station__, parser.__version__)
        sys.exit(0)
    
    if options.all:
        # go through all stations
        options.descriptive = True
        for parser in allparsers.values():
            printcurrent(parser, options.descriptive)
    else:
        # go just through selected stations
        for name, parser in allparsers.iteritems():
            # check whether to parse this particular station
            if name in options.stations:
                printcurrent(parser, options.descriptive)
        
def printcurrent(parser, descriptive):
    """Prints the current title playing on a station
    parser is a BaseParser derived class and descriptive
    tells whether it should be chatty. If chatty we also display
    the stations' name"""
    current = parser()
    current.feed()
    current.parse()
    
    if descriptive:
        print current.__station__,
    current.current_track()

if __name__ == '__main__':
    main()
