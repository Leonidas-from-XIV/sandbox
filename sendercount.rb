#!/usr/bin/env ruby
require 'yaml'
require 'mailparser'
#DateTime, Time, Date
#Date::parse parses RFC2822 dates correctly

def main()
    #s = Sender.new
    #s.adresses << 'abc@def.de'
    #s2 = Sender.new
    #s2.adresses << 'new@domain.edu'
    #p s + s2
    
    # create the worker
    w = Worker.new
    # let him load his settings from the yamlfile
    w.loadsettings
    
    w.analyze
    w.initsenders
    w.author_mails_init
    w.author_mails
    
    # show all unknown adresses
    #w.showunknown
    # show statistic results
    #w.showresults
end

class Worker
    attr_reader :mails
    
    def initialize
        # prepare the counter - set to empty
        @maildirpath = ''
        @known = {}
        @ignore = []
        @mails = []
        @senders = {}
    end
    
    def analyze
        # change the mail directory
        Dir.chdir(@maildirpath)
        # get all files from there (except hidden ones)
        mailfiles = Dir.glob('*')
    
        # go through all found mails
        for mail in mailfiles#[0..2]
            # open them
            f = File.new(mail, 'r')
            m = MailParser.parse_message f
            mail = Mail.new(m)
            @mails << mail
        
            # close the file
            f.close
        end
    end
    
    def initsenders
        for key in @known.keys
            s = Sender.new
            @known[key]
            @known[key].each { |i| s.addadress(i) }
            @senders[key] = s
        end
    end
    
    def author_mails_init
        authors = {}
        for m in @mails
            # go through all malis
            
            # check if the sender is in the ignore list
            if not @ignore.index(m.sender) == nil
                # yes, it is in the ignore list, so skip to the next mail
                next
            else
                # no, it is not in the ignore list.. good.
                
                # have we sorted this mail to some user? (not yet)
                sorted = false
                
                # okay, check all known senders..
                for sender in @senders
                    # if some of these senders hat that adress add this mail to the sender
                    if sender[1].adresses.index(m.sender) != nil
                        sender[1].addmail(m)
                        # we have found a known user, add the mail to that user
                        sorted = true
                    end
                end
                
                # the mail was still not sorted to some sender, 
                #+so it must be from some unknown sender 
                #+(add this sender to the senders list with the mail adress as name)
                if not sorted
                    # create new sender
                    s = Sender.new
                    # add this sender this email adress
                    s.addadress(m.sender)
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
        @senders.each do |sender|
            if sender[1].mails != []
                name = sender[0]
                value = sender[1]
                puts "#{name}: #{value.mails.length}"
            end
        end
    end
    
    def loadsettings
        begin
            save = YAML.load_file('settings.yaml')
            if not save
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
    attr_reader :mails, :adresses
    attr_writer :mails, :adresses
    def initialize
        @adresses = []
        @mails = []
    end
    
    def addmail(mail)
        @mails << mail
    end
    
    def addadress(adress)
        @adresses << adress
    end
    
    def adresses?
        @adresses
    end
    
    def mails?
        return @mails.length
    end
    
    def +(another)
        added = Sender.new
        another.adresses.each {|i| added.adresses << i }
        @adresses.each {|i| added.adresses << i }
        another.mails.each {|i| added.mails << i }
        @mails.each {|i| added.mails << i }
        return added
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