#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A program for listing the songs currently on air"""

import urllib, HTMLParser, htmlentitydefs, optparse, re, sys

stationurls = {'FM4' : 'http://fm4.orf.at/trackservicepopup/stream',
    'Antenne' : 'http://webradio.antenne.de/antenne/webradio/new_channels/ant_infos.php',
    'Bayern3' : 'http://reload.br-online.de/bayern3/playlists/zusatzdaten_hp.html',
    'Charivari' : 'http://www.charivari.de/der_beste_mix.php',
    'Energy' : 'http://213.200.64.229/freestream/download/energy/muenchen/start.html',
    'Gong' : 'http://web1.beamgate.com/Gong/getPlaylist.jsp'}

__version__ = '0.8.2'

def splitver(version):
    return tuple(version.split('.'))

__versiontuple__ = splitver(__version__)
def openoffline(filename):
    """Reads a local file and returns the raw contents.
    Useful in 'offline' mode, to pass it manually too parsers feed()"""
    f = file(filename, 'r')
    fc = f.read()
    f.close()
    return fc


class IncompatibleParser(Exception):
    """An exception thrown when the parser is incompatible,
    i.e. causes an exception during parsing"""
    def __init__(self, parser, *args, **kwargs):
        """Constructor, also tells which parser is incompatible"""
        Exception.__init__(self, 'Incompatible %s parser, look for a newer version' % parser)
    
class StationBase(object, HTMLParser.HTMLParser):
    """The base class for each radio station parser
    provides already some rough tools like the HTMLParser.
    The defined methods should be overloaded to provide a
    consistent interface for all derived station parsers"""
    __station__ = 'StationBase'
    __version__ = '1.0.0'
    __versiontuple__ = splitver(__version__)
    
    crawlerurl = None
    pagecontent = None
    
    def __init__(self, url, offline=False):
        """Initialize some values.
        This method should be always called, to initalize th HTMLParser."""
        HTMLParser.HTMLParser.__init__(self)
        self.crawlerurl = url
        if not offline:
            parsepage = urllib.urlopen(self.crawlerurl)
            self.pagecontent = parsepage.read()
            parsepage.close()
    
    def currenttrack(self):
        """Return the current track in the format
        ARTIST - TITLE as string, unicode is also ok"""
        raise NotImplementedError("Abstract class")
    
    def alltitles():
        """Return all parsed titles..
        format unknown, as nowhere used"""
        raise NotImplementedError("Abstract class")
    
    def capstext(self, text):
        """A helper mehtod to make the texts look consistent
        [...]"""
        chunks = text.split()
        
        if len(chunks) > 1:
            return reduce(lambda x, y: x.capitalize() + ' ' + y.capitalize(), chunks)
        else:
            return text.capitalize()

class FM4Parser(StationBase):
    """The Parser for the austrian sidestream radio station
    FM4, which is part of ORF
    Look at it's homepage
    http://fm4.orf.at"""
    __station__ = 'FM4'
    __version__ = '0.9.0'
    __versiontuple__ = splitver(__version__)
    
    aired = {}
    timeparsing = False
    artistparsing = False
    titleparsing = False
    temptime = ''
    
    def __init__(self, url=stationurls['FM4'], offline=False):
        StationBase.__init__(self, url, offline)
    
    def handle_starttag(self, tag, attrs):
        if tag == 'span':
            if attrs[0][1] == 'tracktitle':
                self.titleparsing = True
            elif attrs[0][1] == 'artist':
                self.artistparsing = True
            elif attrs[0][1] == 'starttime':
                self.timeparsing = True
    
    def handle_endtag(self, tag):
        if tag == 'span':
            self.artistparsing = False
            self.titleparsing = False
            self.timeparsing = False
    
    def handle_data(self, data):
        if self.artistparsing:
            try:
                self.aired[self.temptime]['Artist'] += data
            except KeyError:
                self.aired[self.temptime]['Artist'] = data
        elif self.titleparsing:
            self.aired[self.temptime]['Title'] = data
        elif self.timeparsing:
            self.temptime = data[:-1]
            self.aired[self.temptime] = {}
    
    def handle_entityref(self, name):
        if self.titleparsing or self.artistparsing:
            self.handle_data(htmlentitydefs.entitydefs[name])
    
    def feed(self, *args, **kwargs):
        """Wrapper for the real feed() method,
        on errors raises an IncompatibleParser Exception"""
        try:
            StationBase.feed(self, *args, **kwargs)
        except:
            raise IncompatibleParser('FM4')
    
    def currenttrack(self):
        timekeys = sorted(self.aired)
        playing = self.aired[timekeys[-1]]
        current = playing['Artist'] + ' - ' + playing['Title']
        return current

class EnergyParser(StationBase):
    __station__ = 'Energy'
    __version__ = '0.6.0'
    __versiontuple__ = splitver(__version__)
    
    trackparsing = False
    artist = ''
    title = ''
    
    def __init__(self, url=stationurls['Energy'], offline=False):
        StationBase.__init__(self, url, offline)
    
    def handle_starttag(self, tag, attrs):
        if tag == 'font':
            self.trackparsing = True
    
    def handle_endtag(self, tag):
        if tag == 'font':
            self.trackparsing = False
    
    def handle_data(self, data):
        splitdata = data.splitlines()
        for entry in splitdata:
            if entry != '':
                track = entry.split(' - ')
            
                self.artist = self.capstext(track[0])
                self.title = track[1]
    
    def currenttrack(self):
        return self.artist + ' - ' + self.title

class AntenneParser(StationBase):
    __station__ = 'Antenne'
    __version__ = '0.6.0'
    __versiontuple__ = splitver(__version__)
    
    trackparsing = False
    artist = ''
    title = ''
    
    def __init__(self, url=stationurls['Antenne'], offline=False):
        StationBase.__init__(self, url, offline)
    
    def handle_starttag(self, tag, attrs):
        if tag == 'td':
            try:
                if attrs[2][1] == 'boxlila':
                    self.trackparsing = True
            except IndexError:
                pass
    
    def handle_endtag(self, tag):
        if tag == 'td':
            self.trackparsing = False
    
    def handle_data(self, data):
        if self.trackparsing:
            if self.artist == '':
                self.artist = self.capstext(data)
            else:
                datasplit = data.split(None, 1)
                if len(datasplit) != 0:
                    self.title = self.capstext(data.split(None, 1)[1])
    
    def feed(self, *args, **kwargs):
        """Wrapper for the real feed() method,
        on errors raises an IncopmatibleParser Exception"""
        try:
            StationBase.feed(self, *args, **kwargs)
        except:
            raise IncompatibleParser('Antenne')
    
    def currenttrack(self):
        if not self.artist == '':
            return self.artist + ' - ' + self.title
        else:
            return "No title info currently"

class Bayern3Parser(StationBase):
    __station__ = 'Bayern3'
    __version__ = '0.6.5'
    __versiontuple__ = splitver(__version__)
    
    titleparsing = False
    artistparsing = False
    artist = ''
    title = ''
    
    def __init__(self, url=stationurls['Bayern3'], offline=False):
        StationBase.__init__(self, url, offline)
    
    def handle_starttag(self, tag, attrs):
        if tag == 'div':
            if attrs == []:
                self.titleparsing = True
            else:
                try:
                    if attrs[0][1] == 'webradio_interpret':
                        self.artistparsing = True
                except IndexError:
                    pass
    
    def handle_endtag(self, tag):
        if tag == 'div':
            self.artistparsing = False
            self.titleparsing = False
    
    def handle_data(self, data):
        if self.titleparsing:
            self.title = data.replace('"', '')
        elif self.artistparsing:
            self.artist += data
    
    def handle_entityref(self, name):
        if self.titleparsing or self.artistparsing:
            self.handle_data(htmlentitydefs.entitydefs[name])
    
    def currenttrack(self):
        return self.artist + ' - ' + self.title

class GongParser(StationBase):
    __station__ = 'Gong'
    __version__ = '0.9.1'
    __versiontuple__ = splitver(__version__)
    
    trackparsing = False
    timenow = ''
    aired = {}
    
    def __init__(self, url=stationurls['Gong'], offline=False):
        StationBase.__init__(self, url, offline)
    
    def handle_starttag(self, tag, attrs):
        if tag == 'td':
            self.trackparsing = True
    
    def handle_endtag(self, tag):
        if tag == 'td':
            self.trackparsing  = False
    
    def handle_data(self, data):
        if self.trackparsing:
            timepattern = re.compile(r'\d\d:\d\d')
            istime = timepattern.match(data)
            if istime:
                self.timenow = istime.group()
            else:
                artist, title = self.splittrack(data)
                artist, title = self.capstext(artist), self.capstext(title)
                try:
                    self.aired[self.timenow]['Artist'] = artist
                    self.aired[self.timenow]['Title'] = title
                except KeyError:
                    self.aired[self.timenow] = {}
                    self.aired[self.timenow]['Artist'] = artist
                    self.aired[self.timenow]['Title'] = title
    
    def splittrack(self, trackname):
        artistmatch = re.compile(r'[A-Z]{2,}')
        artist = artistmatch.findall(trackname)
        artist = reduce(lambda x, y: x + ' ' + y, artist)
        
        title = trackname.replace(artist, '')
        
        title = title.replace('von', '')
        title = title.replace(':', '')
        title = title.strip()
        
        return (artist, title)
    
    def handle_entityref(self, name):
        if trackparsing:
            self.handle_data(htmlentitydefs.entitydefs[name])
    
    def feed(self, *args, **kwargs):
        """Wrapper for the real feed() method,
        on errors raises an IncompatibleParser Exception"""
        try:
            StationBase.feed(self, *args, **kwargs)
        except:
            raise IncompatibleParser('Gong')
    
    def currenttrack(self):
        timekeys = sorted(self.aired)
        playing = self.aired[timekeys[-1]]
        current = playing['Artist'] + ' - ' + playing['Title']
        return current

allparsers = [FM4Parser, EnergyParser, AntenneParser, Bayern3Parser, GongParser]
    
def main():
    parser = optparse.OptionParser()
    parser.add_option("-v", "--version", dest="version", default=False,
        action="store_true", help="print program & parsers' versions and exit")
    parser.add_option("-d", "--descriptive", dest="descriptive", default=False,
        action="store_true", help="also print station names")
    parser.add_option("-a", "--all", dest="all", default=True,
        action="store_true", help="question all stations")
    parser.add_option("--fm4", dest="fm4", default=False,
        action="store_true", help="question FM4")
    parser.add_option("--antenne", dest="antenne", default=False,
        action="store_true", help="question Antenne Bayern")
    parser.add_option("--energy", dest="energy", default=False,
        action="store_true", help="question Energy Munich")
    parser.add_option("--bayern3", dest="bayern3", default=False,
        action="store_true", help="question Bayern 3")
    parser.add_option("--gong", dest="gong", default=False,
        action="store_true", help="question Gong")
        
    (options, args) = parser.parse_args()
    
    if options.version:
        print "WhatsOnAir \t\t%s" % __version__
        print 
        for parser in allparsers:
            print "%s Parser \t%s" % (parser.__name__, parser.__version__)
        sys.exit(0)
    
    if options.fm4 or options.antenne or options.energy or options.bayern3 or options.gong:
        options.all = False
    
    if options.all:
        for parser in allparsers:
            try:
                printcurrent(parser, options.descriptive)
            except:
                # failed
                pass
    else:
        # which stations to question?
        if options.fm4:
            printcurrent(FM4Parser, options.descriptive)
        if options.antenne:
            printcurrent(AntenneParser, options.descriptive)
        if options.energy:
            printcurrent(EnergyParser, options.descriptive)
        if options.bayern3:
            printcurrent(Bayern3Parser, options.descriptive)
        if options.gong:
            printcurrent(GongParser, options.descriptive)
        
def printcurrent(parser, descriptive):
    current = parser()
    current.feed(current.pagecontent)
    
    if descriptive:
        print current.__station__,
    print current.currenttrack()

if __name__ == '__main__':
    main()
