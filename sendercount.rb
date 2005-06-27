#!/usr/bin/env ruby
require 'yaml'
#DateTime, Time, Date
#Date::parse parses RFC2822 dates correctly

def main()
    # create the worker
    w = Worker.new
    # let him load his settings from the yamlfile
    w.loadsettings
    # analyze the mail
    w.analyze
    # show all unknown adresses
    w.showunknown
    # show statistic results
    w.showresults
end

class Worker
    def initialize
        # prepare the counter - set to empty
        @counter = {}
        @maildirpath = ''
        @known = {}
        @ignore = []
    end
    
    def analyze
        # change the mail directory
        Dir.chdir(@maildirpath)
        # get all files from there (except hidden ones)
        mailfiles = Dir.glob('*')
    
        # go through all found mails
        for mail in mailfiles
            # open them
            f = File.new(mail, 'r')
        
            # seach the sender (From: field)
            sender = ''
            f.each_line {|i| sender = i if i.match(/From: /)}
            
            # seach the date (Date: field)
            date = Date.today
            f.each_line {|i| date = Date.parse(i) if i.match(/Date: /)}
            #p date
        
            # remove From: parts
            sender.gsub!(/From: /, '')
        
            # get just the email address, not the name
            sender = sender.split
            sendermail = ''
            sender.each {|i| sendermail = i if i.match(/@/) }
        
            # remove <> tags around email-adresses
            sendermail.gsub!('<', '')
            sendermail.gsub!('>', '')
            # remove =20-styled MIME influences
            sendermail.gsub!(/=\d\d/, '')
            # rewrite adresses into lower case
            sendermail.downcase!
        
            # now inspect:
            if @known[sendermail]
                #yeah, we know this user (is in the known list), so add him by name
                sendername = @known[sendermail]
            else
                # no we do not know him
                if not @ignore.include? sendermail
                    # ^^ check if he is in blacklist
                    sendername = sendermail
                    # ^^ no he wasn't, so add him to the statistic
                end
            end
        
            begin
                # add another mail to the counter
                @counter[sendername] += 1
            rescue => e
                # there is no such author yet, so create
                @counter[sendername] = 1
            end
        
            # close the file
            f.close
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
                    'known' => {'user@server.tld' => 'Firstname Lastname'},
                    'ignore' => ['blacklist@bad.tld']}
                YAML.dump(save, out)
            end
            raise(ArgumentError, 'You should check settings.yaml before re-running this program')
        end
    end
    
    def showresults
        # initialize the message count
        messages = 0
        # add the values to the count
        @counter.values.each {|i| messages += i }
    
        puts "Results of #{messages} processed messages:"
    
        # sort the messages
        sorted = @counter.sort {|a,b| a[1] <=> b[1] }.reverse
    
        # convert to float
        messages = messages.to_f
    
        # display statistics
        sorted.each do |name, number|
            # calculate the percent value
            per = (number / messages) * 100
            # shorten it to two numbers after the floating point
            percent = sprintf("%2.2f", per)
            # display
            puts "#{name}: #{number} (#{percent}%)"
        end
    end
    
    def showunknown()
        # get the users
        users = @counter.keys()
        # delete all users without @ and keep just the email adresses
        users.delete_if { |i| i.match(/@/) == nil }
    
        # do we have any?
        if users.length != 0
            # yes, so display them
            puts "Unknown users: #{users.length}"
            users.each {|i| puts i }
            puts 'You can add these users to knownlist or to ignorelist in settings.yaml.'
            puts
        end
    end
end

class Author
    def initialize(adress)
        @adress = adress
        @dates = []
    end
    
    def addmail(date)
        @dates << date
    end
    
    def mails?
        return @dates.length
    end
end

class Mail
    def initialize(sender=nil, date=nil)
        @sender = sender
        @date = date
    end
end

if __FILE__ == $0
    main()
end