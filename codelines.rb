#!/usr/bin/env ruby

# Let's check how many lines are really code, not blanks or comments
def main
    lines_code = 0
    lines_all = 0
    in_docstring = false
    
    ARGF.each do |line|
        lines_all += 1
        line.strip!
	
        #codelines += 1 unless line =~ /^#/ or line.empty?
	
	# ruby rdoc
	if line =~ /^=begin/
	    in_docstring = true
	elsif line =~ /^=end/
	    in_docstring = false
	    lines_code -= 1
	end
	
	# python docstrings
	if line =~ /^""".*"""$/
	    # a one-line docstring: there are no states
	    #+so just remove one line from the lines 
	    #+being marked code
	    lines_code -= 1
	elsif line =~ /^"""/
	    # a multiline docstring (the beginning or the end)
	    if in_docstring
	        in_docstring = false
		lines_code -= 1
	    else
		in_docstring = true
	    end
	elsif line =~ /"""$/
	    # end of an multiline string
	    in_docstring = false
	    lines_code -= 1
	end
	
	    
        if line =~ /^#/ or line.empty?
            # line starts with a # - a comment - ignore it
	else
	    #puts '[Matched]' + line unless in_docstring
	    lines_code += 1 unless in_docstring
	    #puts line unless in_docstring
        end
        
    end
    
    puts "#{lines_code} lines of #{lines_all} lines are sourcecode, that's #{((lines_code / lines_all.to_f) * 100).to_i}%"
end

main if __FILE__ == $0
