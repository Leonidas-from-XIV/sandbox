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
    
    w.analyze
    w.initsenders
    w.author_mails_init
    w.author_mails
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
            
            # let the mailperser parse them
            #+this is slow so it could be made in some other way
            #+maybe by just parsing necessary parts
            m = MailParser.parse_message f
            
            # close the file
            f.close
            
            # create a new, lighter mail instance (saves a lot of RAM)
            mail = Mail.new(m)
            # add this instance to the list of mails
            @mails << mail
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
        messages = 0
        simplelist = {}
        
        @senders.each do |name, value|
            simplelist[name] = value.mails.length if value.mails != []
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