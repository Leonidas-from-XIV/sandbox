#!/usr/bin/env ruby

# import support for all kinds of feeds
require 'rss/0.9'
require 'rss/1.0'
require 'rss/2.0'
# import support for encodings
require 'iconv'
# import support for CGI
require 'cgi'

class FeedConverter
    def initialize
        @xml_source = ''
        @feeddata = []
    end
    
    def from_file(filename)
        f = File::open(filename)
        @xml_source = f.read
        f.close
    end
    
    def parse
        feed = RSS::Parser.parse(@xml_source)
    
        feed.items.each do |item|
            itemdata = {}
            itemdata['title'] = item.title
            itemdata['link'] = item.link
            @feeddata << itemdata
        end
    end
    
    # Output HTML from the internal data structure
    def to_html
        # header
        puts %q(<html><head><title>Sidebar</title></head><body>)
    
        @feeddata.each do |item|
            print '<p><a href="'
            print item['link']
            print '">'
            print convert_entity(item['title'])
            #print item['title']
            puts '</a></p>'
        end
    
        # footer
        puts %q(</body></html>)
    end
end

def convert_entity(text)
    text = Iconv.new('iso-8859-15', 'utf-8').iconv(text) 
    
    feed = text.inspect[1...-1]

    feed.gsub!(/\\([0-7]+)|(.)/m) do
        if $2
            $&
        else
            '&#%s;' % $1.to_i(8).to_s(10)
        end
    end
    
    feed.gsub!('\"', '&quot;')

    return feed
end

def main
    fc = FeedConverter.new
    fc.from_file('sd.xml')
    fc.parse
    fc.to_html
    
end

main if __FILE__ == $0