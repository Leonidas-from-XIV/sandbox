import urllib, re
import base

class AntenneParser(base.StationBase):
    """Parser for Antenne Bayern"""
    
    __station__ = 'AntenneBayern'
    __version__ = '0.6.1'
    __versiontuple__ = base.splitver(__version__)
    
    def __init__(self, url='http://webradio.antenne.de/antenne/webradio/new_channels/ant_infos.php'):
        base.StationBase.__init__(self, url)
    
    def parse(self):
        """Call feed first"""
        artists_rex = re.compile(r'(?<=<b>).*(?=</b>)')
        artists = artists_rex.findall(self.pagecontent)
        self.artist = artists[1]
        
        title_rex = re.compile(r'(?<=</b>, ).*(?=</a>)')
        titles = title_rex.findall(self.pagecontent)
        self.title = titles[0]
    
    def current_track(self):
        print "%s - %s" % (self.capstext(self.artist), self.capstext(self.title))

Parser = AntenneParser

if __name__ == '__main__':
    p = Parser()
    #p.feed()
    p.pagecontent = file('ant_infos.php.htm', 'r').read()
    #print p.pagecontent
    p.parse()
    p.current_track()
