#!/usr/bin/env ruby

# Let's check how many lines are really code, not blanks or comments
def main
    codelines = 0
    alllines = 0
    ARGF.each do |line|
        alllines += 1
        line.strip!
        codelines += 1 unless line =~ /^#/ or line.empty?
    end
    
    puts "#{codelines} lines of #{alllines} lines are sourcecode, that's #{((codelines / alllines.to_f) * 100).to_i}%"
end

if __FILE__ == $0
    main()
end