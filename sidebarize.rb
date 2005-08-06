#!/usr/bin/env ruby

require 'cgi'
require 'rss/0.9'
require 'rss/1.0'
require 'rss/2.0'
require 'iconv'

def from_file(filename)
    f = File::open(filename)
    content = f.read
    f.close
    return content
end

def to_html(feeddata)
    # header
    puts %q(<html><head><title>Sidebar</title></head><body>)
    
    feeddata.each do |item|
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
    xml_source = from_file('sd.xml')
    feed = RSS::Parser.parse(xml_source)
    
    feeddata = []
    feed.items.each do |item|
        itemdata = {}
        itemdata['title'] = item.title
        itemdata['link'] = item.link
        feeddata << itemdata
    end
    to_html(feeddata)
end

main if __FILE__ == $0