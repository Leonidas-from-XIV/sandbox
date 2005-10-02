#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A program for listing the songs currently on air
This program is under GPL"""

import urllib, HTMLParser, htmlentitydefs, re, sys, os, optparse

__version__ = '0.8.6'

def splitver(version):
    """Splits the string representation of the version into a tuple form,
    so it's easier to parse"""
    return tuple(version.split('.'))

__versiontuple__ = splitver(__version__)
def openoffline(filename):
    """Reads a local file and returns the raw contents.
    Useful in 'offline' mode, to pass it manually too parsers feed()"""
    f = file(filename, 'r')
    content = f.read()
    f.close()
    return content

class IncompatibleParser(Exception):
    """An exception thrown when the parser is incompatible,
    i.e. causes an exception during parsing"""
    def __init__(self, parser, *args, **kwargs):
        """Constructor, also tells which parser is incompatible"""
        Exception.__init__(self, 'Incompatible %s parser, look for a newer version' % parser)

class PluginController(object):
    pluginlist = []

    def __init__(self):
        # add the plugindir to the pythonpath
        sys.path.append(self.parserdir())
        self.import_plugins()
        # remove the plugindir from the pythonpath
        sys.path.pop()
        
    
    def parserdir(self):
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
    
    def __iter__(self):
        """An iterator"""
        for module in self.pluginlist:
            if not module.Parser == None:
                yield module.Parser

def parser_chosen(option, opt, value, parser):
    station = opt[2:]
    try:
        parser.values.stations[station] = True
    except AttributeError:
        parser.values.stations = {}
        parser.values.stations[station] = True
    parser.values.all = False
    

def main():
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
        options.descriptive = True
        for parser in allparsers.values():
            #try:
            printcurrent(parser, options.descriptive)
            #except:
                # failed, so ignore silently
            #    pass
    else:
        for name, parser in allparsers.iteritems():
            if options.stations[name]:
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
