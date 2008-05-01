#!/usr/bin/env ruby
# Sidebarize is a quite simple converter, like a lot others.
# But in one way Sidebarize is special: it converts
# feeds into HTML-pages suitable for sidebars in Gecko-browsers.

# import support for all kinds of feeds
require 'rss/0.9'
require 'rss/1.0'
require 'rss/2.0'
# import support for encodings
require 'iconv'
# import support for CGI
require 'cgi'
# import support for getting files from URLs
require 'net/http'
require 'uri'

Version = '0.1.1'

Header = %q(<!DOCTYPE html PUBLIC 
	"-//W3C//DTD XHTML 1.0 Strict//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"> 
<html xmlns="http://www.w3.org/1999/xhtml">
    <head>
        <title>%s Sidebar</title>
        <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1" />
        <link rel="stylesheet" type="text/css" href="sidebarize.css" />
    </head>

    <body>
        <p>%s</p>)

Footer = %q(        <p>
            <img src="fourpiece32.png" width="32" height="32" class="logo" alt="Fourpieces Logo" title="Fourpieces Logo" />
            generated by Sidebarize %s
        </p>
    </body>
</html>)

Entry = %q(        <div>
            <img src="arrow.png" width="6" height="11" alt="&gt;" />
            <a href="%s">%s</a>
        </div>)

class FeedConverter
    def initialize
        @xml_source = ''
        @feed_data = []
        @feed_version = ''
        @feed_description = ''
    end
    
    def from_file(filename)
        f = File::open(filename)
        @xml_source = f.read
        f.close
    end
    
    def from_url(url)
        @xml_source = Net::HTTP.get(URI.parse(url))
    end
    
    def parse
        feed = RSS::Parser.parse(@xml_source)
        @feed_description = feed.channel.description
        @feed_name = feed.channel.title
        
    
        feed.items.each do |item|
            item_data = {}
            item_data['title'] = item.title
            item_data['link'] = item.link
            @feed_data << item_data
        end
    end
    
    # Output HTML from the internal data structure
    def to_html
        # header
        puts Header % [convert_entity(@feed_name), convert_entity(@feed_description)]
        
        # the entries
        @feed_data.each do |item|
            puts Entry % [convert_link(item['link']), convert_entity(item['title'])]
        end
    
        # footer
        print Footer % Version
    end
end

# Converts entities
# uses code by murphy extended with iconv conversion
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

# converts & to entity to make the w3c validator happy
# see http://www.htmlhelp.com/tools/validator/problems.html#amp
def convert_link(url)
    url.gsub('&', '&amp;')
end

# Called when errors happen
def show_error(text)
    puts Header % ['No', text]
    puts Footer % Version
end

# Starter
def main
    fc = FeedConverter.new
    cgi = CGI.new
    if cgi.has_key? 'url'
        # yeah, the user pointed us to an URL
        
        begin
            # try to load from it
            fc.from_url(cgi['url'])
        rescue => e
            # something failed
            puts 'Content-Type: text/html'
            puts
            show_error('The file could not be loaded.')
            #puts "<!-- #{e} -->"
        end
        #fc.from_file('sd.xml')
        
        begin
            # try to parse it and to generate HTML
            fc.parse
            
            puts 'Content-Type: text/html'
            puts
            fc.to_html
        rescue RSS::NotWellFormedError => e
            # parsing failed so show an error message
            puts 'Content-Type: text/html'
            puts
            show_error('The specified feed is not valid.')
        end
    else
        # no, we've got no URL, generate error message
        puts 'Content-Type: text/html'
        puts
        show_error('You have to set the url=http://domain.tld/path/feed.xml to your feed.')
    end
end

main if __FILE__ == $0
