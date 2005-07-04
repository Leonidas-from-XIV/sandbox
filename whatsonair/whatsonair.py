#!/usr/bin/env python
# -*- encoding: latin-1 -*-
"""A program for listing the songs currently on air
This program is under GPL"""

import urllib, HTMLParser, htmlentitydefs, re, sys

stationurls = {'FM4' : 'http://fm4.orf.at/trackservicepopup/stream',
    'Antenne' : 'http://webradio.antenne.de/antenne/webradio/new_channels/ant_infos.php',
    'Bayern3' : 'http://reload.br-online.de/bayern3/playlists/zusatzdaten_hp.html',
    'Charivari' : 'http://www.charivari.de/der_beste_mix.php',
    'Energy' : 'http://www.energy.de/static/ticker/write_titel.phtml',
    'Gong' : 'http://web1.beamgate.com/Gong/getPlaylist.jsp',
    'RTL' : 'http://www.hitradio-rtl-sachsen.de/streamplayer/onair.php',
    'NRJ' : 'http://www.nrj.de/www/index_top.php',
    'PSR' : 'http://www.radiopsr.de/www/webradio/e98cb037f376fa53b314c166766ef55e.php',
    'YOUFM' : 'http://www3.admin.hr-online.de/playlist/playlist.php?tpl=youfm',
    'HR3' : 'http://www3.admin.hr-online.de/playlist/playlist.php?tpl=hr3neu',
    'NJoy' : 'http://www1.n-joy.de/njoy_pages_idx/0,3043,SPM2140,00.html',
    'EinsLive' : 'http://www.einslive.de/diemusik/dieplaylists/die_letzten_12_titel/index.phtml',
    'SunshineLive': 'http://www.sunshine-live.de/core/playlist.php3',
    'EnergyBerlin': 'http://www.energy.de/static/ticker/write_titel.phtml'}

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
    
class StationBase(object, HTMLParser.HTMLParser):
    """The base class for each radio station parser
    provides already some rough tools like the HTMLParser.
    The defined methods should be overloaded to provide a
    consistent interface for all derived station parsers
    
    The constructor loads the page content into self.pagecontent,
    and also cares about timeout handling. So in child classes
    you need to call StationBase.__init__ for doing the most work.
    
    All child classes also have a feed() method, where the page content
    is parsed and all values initialized. After that, curenttrack() can
    be called to get the currently playing track."""
    __station__ = 'StationBase'
    __version__ = '1.0.0'
    __versiontuple__ = splitver(__version__)
    
    crawlerurl = None
    pagecontent = None
    
    def __init__(self, url, offline=False):
        """Initialize some values.
        This method should be always called, to initalize the HTMLParser."""
        HTMLParser.HTMLParser.__init__(self)
        self.crawlerurl = url
        if not offline:
            timeout = urllib.socket.getdefaulttimeout()
            urllib.socket.setdefaulttimeout(10.0)
            parsepage = urllib.urlopen(self.crawlerurl)
            self.pagecontent = parsepage.read()
            parsepage.close()
            urllib.socket.setdefaulttimeout(timeout)
    
    def currenttrack(self):
        """Return the current track in the format
        ARTIST - TITLE as string, unicode is also ok"""
        raise NotImplementedError("Abstract class")
    
    def alltitles():
        """Return all parsed titles..
        format unknown, as nowhere used"""
        raise NotImplementedError("Abstract class")
    
    def capstext(self, text):
        """A helper method to make the texts look consistent
        [...]"""
        chunks = text.split()
        
        if len(chunks) > 1:
            return reduce(lambda x, y: x.capitalize() + ' ' + y.capitalize(), chunks)
        else:
            return text.capitalize()

class FM4Parser(StationBase):
    """The Parser for the austrian sidestream radio station
    FM4, which is part of ORF.
    Look at its homepage http://fm4.orf.at"""
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
    """The first Energy parser.
    Handles Energy Munich former known as Energy 93.3
    """
    __station__ = 'Energy'
    __version__ = '0.7.0'
    __versiontuple__ = splitver(__version__)
    
    artist = ''
    title = ''
    
    def __init__(self, url=stationurls['Energy'], offline=False):
        StationBase.__init__(self, url, offline)
    
    def feed(self, content):
        try:
            r = re.compile(r'(?<=&interpr_muc=)[\w|\s]*(?= &&sg_muc=)')
            self.artist = self.capstext(r.findall(content)[0])
            r = re.compile(r'(?<=&&sg_muc=)[\w|\s]*(?=\+\+\+&)')
            self.title = r.findall(content)[0].strip()
        except:
            raise IncompatibleParser(self.__station__)
    
    def currenttrack(self):
        return self.artist + ' - ' + self.title

class AntenneParser(StationBase):
    """Parser for Antenne Bayern"""
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
            raise IncompatibleParser(self.__station__)
    
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
            raise IncompatibleParser(self.__station__)
    
    def currenttrack(self):
        timekeys = sorted(self.aired)
        playing = self.aired[timekeys[-1]]
        current = playing['Artist'] + ' - ' + playing['Title']
        return current
    
class PSRParser(StationBase):
    """This station provides multiple songs
    this parser has to be changed a bit.
    
    Code taken from Iopodx"""
    __station__ = 'PSR'
    __version__ = '0.3.0'
    __versiontuple__ = splitver(__version__)
    
    trackparsing = False
    artist = ''
    title = ''
    
    def __init__(self, url=stationurls['PSR'], offline=False):
        StationBase.__init__(self, url, offline)
    
    def feed(self, text):
        """Wrapper for the real feed() method,
        on errors raises an IncompatibleParser Exception"""
        try:
            result = text.split('</b></td>\n 	<td>')
            result = result[1].split('</td>')
            track = result[0].split(' - ')
            if len(track) > 1:
                self.artist, self.title = track[0], track[1]
                # else: no song now
                
        except:
            raise IncompatibleParser(self.__station__)
    
    def currenttrack(self):
        if not self.artist == '':
            return self.artist + ' - ' + self.title
        else:
            return "No title info currently"
        
class NRJParser(StationBase):
    """Coded by Iopodx"""
    __station__ = 'NRJ'
    __version__ = '0.3.0'
    __versiontuple__ = splitver(__version__)
    trackparsing = False
    artist = ''
    title = ''
    
    def __init__(self, url=stationurls['NRJ'], offline=False):
        StationBase.__init__(self, url, offline)
    
    def feed(self, text):
        """Wrapper for the real feed() method,
        on errors raises an IncompatibleParser Exception"""
        try:
            result = text.split('JETZT BEI ENERGY: \r\n\t')
            result = result[1].split('\n <a href=')
            track = result[0].split(' \xb7 ')
            if len(track) > 1:
                self.artist, self.title = self.capstext(track[0]), self.capstext(track[1])
                # else: no song now
        except:
            raise IncompatibleParser(self.__station__)
    
    def currenttrack(self):
        if not self.artist == '':
            return self.artist + ' - ' + self.title
        else:
            return "No title info currently"

class RTLParser(StationBase):
    """Coded by Iopodx"""
    __station__ = 'RTL'
    __version__ = '0.1.0'
    __versiontuple__ = splitver(__version__)
    trackparsing = False
    artist = ''
    title = ''
    
    def __init__(self, url=stationurls['RTL'], offline=False):
        StationBase.__init__(self, url, offline)
    
    def feed(self, text):
        """Wrapper for the real feed() method,
        on errors raises an IncompatibleParser Exception"""
        try:
            result = text.split('<td class="Stil1"><b>')
            result = result[1].split('</td>')
            track = result[0].split('</b><br>')
            if len(track) > 1:
                self.artist, self.title = self.capstext(track[0]), self.capstext(track[1])
                # else: no song now
        except:
            raise IncompatibleParser(self.__station__)
    
    def currenttrack(self):
        if not self.artist == '':
            return self.artist + ' - ' + self.title
        else:
            return "No title info currently"

class YOUFMParser(StationBase):
    """Quickly coded by Johi"""
    __station__ = 'YOUFM'
    __version__ = '0.1.1'
    __versiontuple__ = splitver(__version__)
    
    aired = {}
    
    def __init__(self, url=stationurls['YOUFM'], offline=False):
        StationBase.__init__(self, url, offline)
        
    def feed(self, text):
        """Wrapper for the real feed() method,
        on errors raises an IncompatibleParser Exception"""
        try:
            feed = text.split('<table width="100%" cellpadding="2">\r\n\t<tr>\r\n\t\t<td><span class="text">Datum</span></td>\r\n\t\t<td><span class="text">Zeit</span></td>\r\n\t\t<td><span class="text">Interpret</span></td>\r\n\t\t<td><span class="text">Titel</span></td>\r\n\t</tr>\r\n\t\t<tr>')[1].split('</table>')[0]
            data = feed.split("</tr>\r\n\t\t<tr>")[:-1]
            for item in data:
                values = item.replace('\t','').replace('\r','').replace('<td bgcolor="#ffffff">', '').replace('<td bgcolor="#ffffff" align="center">', '').replace('</td>', '').split("\n")[1:-1]
                timex = int(values[0].replace('.','') + values[1].replace(':',''))
                self.aired[timex] = {"Artist":values[2].decode("latin-1"), "Title":values[3].decode("latin-1"), "Day": values[0], "Time": values[1]} 
        except:
            raise IncompatibleParser(self.__station__)
    
    def currenttrack(self):
        if len(self.aired) > 0:
            timekeys = self.aired.keys()[:]
            timekeys.sort()
            playing = self.aired[timekeys[-1]]
            current = playing['Artist'] + ' - ' + playing['Title']
            return current
        else:
            return 'No title info currently'

class HR3Parser(YOUFMParser):
    """Quickly coded by Johi.
    This parser is fun, as it uses internally the YOUFMParser."""
    __station__ = 'HR3'
    __version__ = '0.0.1'
    __versiontuple__ = splitver(__version__)
    
    def __init__(self, url=stationurls['HR3'], offline=False):
        YOUFMParser.__init__(self, url, offline)

class NJoyParser(StationBase):
    """NJoy Parser by Iopodx"""
    __station__='NJoy'
    __version__ = '0.1.0'
    __versiontuple__ = splitver(__version__)
    trackparsing = False
    artist = ''
    title = ''
    
    def __init__(self, url=stationurls['NJoy'], offline=False):
        StationBase.__init__(self, url, offline)
    
    def feed(self, text):
        """Wrapper for the real feed() method,
        on errors raises an IncompatibleParser Exception"""
        try:
            result = text.split('" title="N-JOY Playlist">')
            result = result[1].split('\n</a></span>')
            track = result[0].split(' - ')
            if len(track) > 1:
                self.artist, self.title = track[0], self.capstext(track[1])
                # else: no song now
        except:
            raise IncompatibleParser(self.__station__)
    
    def currenttrack(self):
        if not self.artist == '':
            return self.artist + ' - ' + self.title
        else:
            return "No title info currently"

class EinsLiveParser(StationBase):
    """EinsLiveParser by Iopodx
    various additions by Leonidas"""
    __station__='EinsLive'
    __version__ = '0.1.1'
    __versiontuple__ = splitver(__version__)
    trackparsing = False
    artist = ''
    title = ''
    
    def __init__(self, url=stationurls['EinsLive'], offline=False):
        """Constructs the Parser"""
        # get the normal timeout
        timeout = urllib.socket.getdefaulttimeout()
        # set ten seconds timeout
        urllib.socket.setdefaulttimeout(10.0)
        
        # get the data
        try:
            StationBase.__init__(self, url, offline)
        except:
            # failed.. somehow (maybe by a timeout) so use a mirror
            StationBase.__init__(self, "http://bofod.bo.funpic.de/einslive.php", offline)
        
        # return to default timeout lenght
        urllib.socket.setdefaulttimeout(timeout)
    
    def feed(self, text):
        """Wrapper for the real feed() method,
        on errors raises an IncompatibleParser Exception"""
        try:
            result = text.split('</TD><TD valign="top" class="cont">')
            result = result[1].split('</TD></TR><TR><TD valign="top"')
            track = result[0].split('</TD><TD valign="top" class="contbold">')
            if len(track) > 1:
                track[0] = track[0].replace('&nbsp;', '')
                track[1] = track[1].replace('&nbsp;', '')
                self.artist, self.title = track[0], self.capstext(track[1])
                # else: no song now
        except:
            raise IncompatibleParser(self.__station__)
    
    def currenttrack(self):
        if not self.artist == '':
            return self.artist + ' - ' + self.title
        else:
            return "No title info currently"

class SunshineLiveParser(StationBase):
    """SunshineLiveParser by Iopodx
    various additions by Leonidas"""
    __station__='SunshineLive'
    __version__ = '0.1.1'
    __versiontuple__ = splitver(__version__)
    trackparsing = False
    artist = ''
    title = ''
    
    def __init__(self, url=stationurls['SunshineLive'], offline=False):
        """Constructs the Parser"""
        StationBase.__init__(self, url, offline)

    def feed(self, text):
        """Wrapper for the real feed() method,
        on errors raises an IncompatibleParser Exception"""
        try:
            result1 = text.split('</TD><TD NOWRAP>')
            result = text.split('</TD><TD NOWRAP>')
            result = result[2].split('</TD><TD><A HREF="')
            track = [result[0], result1[1]]
            if len(track) > 1:
                self.artist, self.title = track[0], track[1]
                # else: no song now
        except:
            raise IncompatibleParser(self.__station__)
    
    def currenttrack(self):
        if not self.artist == '':
            return self.artist + ' - ' + self.title
        else:
            return "No title info currently"

class EnergyBerlinParser(EnergyParser):
    """EnergyBerlinParser by Iopodx
    """
    __station__='EnergyBerlin'
    __version__ = '0.1.1'
    __versiontuple__ = splitver(__version__)
    artist = ''
    title = ''
    
    def __init__(self, url=stationurls['EnergyBerlin'], offline=False):
        """Constructs the Parser"""
        StationBase.__init__(self, url, offline)

    def feed(self, content):
        """Wrapper for the real feed() method,
        on errors raises an IncompatibleParser Exception"""
        try:
            r = re.compile(r'(?<=&<p>)[\w|\s|/]*(?=<p>)')
            trackname = r.findall(content)[0].strip()
            trackname = trackname.split('/')
            self.artist = self.capstext(trackname[0].strip())
            self.title = trackname[1].strip()
        except:
            raise IncompatibleParser(self.__station__)
    
    def currenttrack(self):
        if not self.artist == '':
            return self.artist + ' - ' + self.title
        else:
            return "No title info currently"

allparsers = [FM4Parser, EnergyParser, AntenneParser, Bayern3Parser, 
    GongParser, PSRParser, NRJParser, RTLParser, YOUFMParser, HR3Parser,
    NJoyParser, EinsLiveParser, SunshineLiveParser, EnergyBerlinParser]
    
def main():
    """The commandline frontend"""
    # import optik here so it is not needed when using as a library
    import optparse
    
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
    parser.add_option("--psr", dest="psr", default=False,
        action="store_true", help="question PSR (Sachsen)")
    parser.add_option("--nrj", dest="nrj", default=False,
        action="store_true", help="question NRJ")
    parser.add_option("--rtl", dest="rtl", default=False,
        action="store_true", help="question RTL")
    parser.add_option("--youfm", dest="you", default=False,
        action="store_true", help="question YOUFM")
    parser.add_option("--hr3", dest="hr3", default=False,
        action="store_true", help="question HR3")
    parser.add_option("--njoy", dest="njoy", default=False,
        action="store_true", help="question NJoy")
    parser.add_option("--einslive", dest="einslive", default=False,
        action="store_true", help="question EinsLive")
    parser.add_option("--sunshinelive", dest="sunshinelive", default=False,
        action="store_true", help="question SunshineLive")
    parser.add_option("--energyberlin", dest="energyberlin", default=False,
        action="store_true", help="question EnergyBerlin")
        
    (options, args) = parser.parse_args()
    
    if options.version:
        print "WhatsOnAir \t%s" % __version__
        print 
        for parser in allparsers:
            print "%s Parser\t%s" % (parser.__station__, parser.__version__)
        sys.exit(0)
    
    if (options.fm4 or options.antenne or options.energy or options.bayern3 or
        options.gong or options.psr or options.nrj or options.rtl or options.njoy or
        options.einslive or options.sunshinelive or options.energyberlin):
        options.all = False
    
    if options.all:
        options.descriptive = True
        for parser in allparsers:
            try:
                printcurrent(parser, options.descriptive)
            except:
                # failed, so ignore silently
                pass
    else:
        # which stations to question?
        # should be refactored at some time
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
        if options.psr:
            printcurrent(PSRParser, options.descriptive)
        if options.nrj:
            printcurrent(NRJParser, options.descriptive) 
        if options.rtl:
            printcurrent(NRJParser, options.descriptive) 
        if options.you:
            printcurrent(YOUFMParser, options.descriptive) 
        if options.hr3:
            printcurrent(HR3Parser, options.descriptive) 
        if options.njoy:
            printcurrent(NJoyParser, options.descriptive) 
        if options.einslive:
            printcurrent(EinsLiveParser, options.descriptive)
        if options.sunshinelive:
            printcurrent(SunshineLiveParser, options.descriptive)
        if options.energyberlin:
            printcurrent(EnergyBerlinParser, options.descriptive) 
        
def printcurrent(parser, descriptive):
    """Prints the current title playing on a station
    parser is a BaseParser derived class and descriptive
    tells whether it should be chatty. If chatty we also display
    the stations' name"""
    current = parser()
    current.feed(current.pagecontent)
    
    if descriptive:
        print current.__station__,
    print current.currenttrack()

if __name__ == '__main__':
    main()
