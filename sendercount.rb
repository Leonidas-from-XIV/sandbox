#!/usr/bin/env ruby
require 'yaml'
require 'mailparser'
#DateTime, Time, Date
#Date::parse parses RFC2822 dates correctly

def main()
    # create the worker
    w = Worker.new
    # let him load his settings from the yamlfile
    w.loadsettings
    
    w.init_mails
    w.init_senders
    w.init_author_mails
    w.author_mails
    w.unknown_authors
end

class Worker
    attr_reader :mails
    
    def initialize
        # the path to maildir (in MH format)
        @maildirpath = ''
        # the list of known senders
        @known = {}
        # the list of ignored mails
        @ignore = []
        # the full list of _all_mails
        @mails = []
        # senders in Name => SenderInstance format.
        @senders = {}
    end
    
    def init_mails
        # change the mail directory
        Dir.chdir(@maildirpath)
        # get all files from there (except hidden ones)
        mailfiles = Dir.glob('*')
    
        # go through all found mails
        mailfiles.each do |mail|
            # open them
            f = File.new(mail, 'r')
            
            # add the parsed, lightened mail to our list of mails
            @mails << Mail.new(MailParser.parse_message(f))
        end
    end
    
    def init_senders
        # init the senders (create from every known entry a sender)
        @known.keys.each do |key|
            s = Sender.new
            @known[key].each { |i| s.addaddress(i) }
            @senders[key] = s
        end
    end
    
    def init_author_mails
        authors = {}
        @mails.each do |m|
            # go through all malis
            
            # check if the sender is in the ignore list
            if @ignore.include? m.sender
                # yes, it is in the ignore list, so skip to the next mail
                next
            else
                # no, it is not in the ignore list.. good.
                
                # have we sorted this mail to some user? (not yet)
                sorted = false
                
                # okay, check all known senders..
                @senders.each do |sender|
                    # if some of these senders hat that address add this mail to the sender
                    if sender[1].addresses.include? m.sender
                        sender[1].addmail(m)
                        # we have found a known user, add the mail to that user
                        sorted = true
                    end
                end
                
                # the mail was still not sorted to some sender, 
                #+so it must be from some unknown sender 
                #+(add this sender to the senders list with the mail address as name)
                unless sorted
                    # create new sender
                    s = Sender.new
                    # add this sender this email address
                    s.addaddress(m.sender)
                    # and the current mail
                    s.addmail(m)
                    @senders[m.sender] = s
                    # and now really: we have sorted it
                    sorted = true
                end
            end
        end
    end
    
    def author_mails
        messages = 0
        simplelist = {}
        
        @senders.each do |name, value|
            simplelist[name] = value.mails.length unless value.mails.empty?
        end
        
        # sort the list (from largest to lowest)
        sortedlist = simplelist.sort {|a,b| b[1] <=> a[1] } 
        # calculate number of mails
        sortedlist.each {|i| messages += i[1]}
        
        messages = messages.to_f
        
        sortedlist.each do |name, number|
            # calculate the percent value
            per = (number / messages) * 100
            # shorten it to two numbers after the floating point
            percent = sprintf("%2.2f", per)
            # display
            puts "#{name}: #{number} (#{percent}%)"
        end
    end
    
    def unknown_authors
        unknown = []
        @senders.keys.each() do |sender|
            unknown << sender if sender =~ /@/
        end
        
        unless unknown.empty?
            puts "The following #{unknown.length} adresses are unknown: #{unknown.join(', ')}"
            puts "Consider adding them to the known- or blacklist."
        end
    end
    
    def loadsettings
        begin
            save = YAML.load_file('settings.yaml')
            unless save
                # the file is empty.. well, not valid YAML
                raise(ArgumentError)
            end
            
            # do something with the loaded values
            @maildirpath = save['maildirpath']
            @known = save['known']
            @ignore = save['ignore']
            
        rescue Errno::ENOENT, ArgumentError => e
            # file not found or not valid
            # so create a valid one
            File.open('settings.yaml', 'w' ) do |out|
                save = {'maildirpath' => '/path/to/mh/',
                    'known' => {'Firstname Lastname' => ['user@server.tld']},
                    'ignore' => ['blacklist@bad.tld']}
                YAML.dump(save, out)
            end
            raise(ArgumentError, 'You should check settings.yaml before re-running this program')
        end
    end
end

class Sender
    attr_accessor :mails, :addresses
    def initialize
        @addresses = []
        @mails = []
    end
    
    def addmail(mail)
        @mails << mail
    end
    
    def addaddress(address)
        @addresses << address
    end
end

class Mail
    attr_reader :sender, :date
    def initialize(parsedmail)
        @sender = parsedmail[:from][0].downcase
        @date = parsedmail[:date]
    end
end

if __FILE__ == $0
    main()
end