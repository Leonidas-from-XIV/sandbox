#!/usr/bin/env ruby
require 'yaml'
require 'date'
require 'mailparser'


#The main method. This one controls the execution of all commands.
def main()
    # create the worker
    w = Worker.new
    # let him load his settings from the yamlfile
    w.load_settings
    
    # initialize the mails in the maildir
    w.init_mails
    w.init_senders
    w.init_author_mails
    w.unknown_authors
    w.author_mails
    puts
    w.mails_per_day
end

# The Worker class is used to create a representation of the real world.
# In this world, there are the @mails, the list of @known people,
# some @ignore'd ones, and the @senders who have sent the mails.
# ---
# Some kind of example:
#
#  w = Worker.new
#  # load the settings
#  w.load_settings()
#
# This should be quite easy to understand.
class Worker
    
    # Creates a new class. Initializes some variables.
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
    
    # Load all mails from the maildir and save them to 
    # @mails as Mail instances. This should be called at first
    # after loading the settings. The settings are loaded by load_settings().
    def init_mails
        # change the mail directory
        Dir.chdir(@maildirpath)
        # get all files from there (except hidden ones)
        mailfiles = Dir.glob('*')
    
        # go through all found mails
        mailfiles.each do |mail|
            # open them
            f = File.new(mail, 'r')
            
            sender = nil
            date = nil
            f.each_line do |line| 
                if line.match(/^From: /)
                    line.gsub!(/From: /, '')
                    sender = MailParser.get_mail_address(line)[0]
                elsif line.match(/^Date: /)
                    line.gsub!(/Date: /, '')
                    date = Date.parse(line)
                end
            end
            
            # add the parsed, lightened mail to our list of mails
            @mails << Mail.new(sender, date)
        end
    end
    
    # Initialize the @sender hash: create for each known sender
    # (from the settings-file) a key in the hash and create there
    # a Sender instance with all e-mail addresses filled in.
    def init_senders
        # init the senders (create from every known entry a sender)
        @known.keys.each do |key|
            s = Sender.new
            @known[key].each { |i| s.addaddress(i) }
            @senders[key] = s
        end
    end
    
    # Sort the mails from @mails to @senders where every 
    # mail gets sorted properly to the author. This should be called
    # after init_mails().
    def init_author_mails
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
    
    # Output the list of authors who have written most mails in descending
    # order. Also prints the percentage of all processed mails.
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
        
        puts "Mail-number results of #{messages} processed messages:"
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
    
    # Outputs the list of unknown addresses
    def unknown_authors
        unknown = []
        @senders.keys.each() do |sender|
            unknown << sender if sender =~ /@/
        end
        
        unless unknown.empty?
            puts
            puts "The following #{unknown.length} adresses are unknown: #{unknown.join(', ')}"
            puts "Consider adding them to the known- or ignorelist in settings.yaml."
            puts
        end
    end
    
    # Loads the settings from a settings file. The file is called
    # settings.yaml, and is, (surprise) a YAML file.
    def load_settings
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
    
    # Outputs a statistic about mails-per-day per author.
    def mails_per_day
        all_mpd = {}
        @senders.each do |name, value|
            # next sender it the current sender has no mails
            next if value.mails.empty?
            
            alldates = []
            value.mails.each {|i| alldates << i.date }
            alldates.sort!
            
            # get the date of the first (=oldest) mail
            since = alldates[0]
            # get the date of today
            now = Date.today()
            # how many days is the first mail old?
            duration = (now - since)
            # divide the number of mails by the duration
            mpd = (value.mails.length / duration).to_f
            # add the mail-per-day value to the dict
            all_mpd[name] = mpd
        end
        
        # sort from biggest to smallest
        all_mpd = all_mpd.sort {|a,b| b[1] <=> a[1] }
        
        puts "Mails-per-day results of #{all_mpd.length} processed authors:"
        
        all_mpd.each do |person, mpd|
            puts "#{person}: #{sprintf('%0.3f', mpd)} mails/day"
        end
    end
    
    def init_mailers
        @senders.each do |sender|
        end
    end
    
    def used_mailers
    end
end

# This represends an author
class Sender
    attr_accessor :mails, :addresses
    # Set some variables
    def initialize
        @addresses = []
        @mails = []
    end
    
    # Add the Mail object.
    def addmail(mail)
        @mails << mail
    end
    
    # Adds an email-address.
    def addaddress(address)
        @addresses << address
    end
end

class Mail
    attr_reader :sender, :date
    def initialize(sender, date)
        @sender = sender.downcase
        #time = parsedmail[:date]
        @date = date
    end
end

if __FILE__ == $0
    main()
end