#!/usr/bin/env ruby

# the main caller
def main
    puts 'Expression solver'
    puts 'Type Ctrl+C to quit'
    # start s loop
    loop do
        # get user input
        begin
            print '>>> '
            exp = gets
        rescue Interrupt => e
            puts 'Quitting'
            exit(0)
        end
        
        # get rid of the linefeed
        exp.chomp!
        
        # quit if the expression is a q
        break if exp == 'q'
        
        puts solve_exp_rpn(exp)
    end
end

# Just an empty exception.. 
class StackError < Exception
end

# A First in last out stack
class FiLo
    attr_reader :stack
    def initialize
        @stack = []
    end
    
    def push(element)
        @stack << element
    end
    
    def pop
        unless @stack.empty?
            @stack.pop
        else
            raise(StackError, 'Cannot pop from empty stack')
        end
    end
    
    def peek
        unless @stack.empty?
            @stack[-1]
        else
            raise(StackError, 'Cannot peek from empty stack')
        end
    end
    
    def clear
        @stack = []
    end
end

# RPN Reverse Polish Notation
# http://www.activevb.de/tutorials/tut_polnat/polnat.html
def solve_exp_rpn(exp)
    # create a new stack
    stack = FiLo.new
    tokens = exp.split
    
    tokens.each do |token|
        if token =~ /^\d+\.?\d?$/
            # it is a number (fixnum or float)
            stack.push(token.to_f)
        elsif token =~ /[A-Za-z]/
            # do nothing.. ignore it simply
        else
            # is an operator
            tmp_number = stack.pop()
            
            case token
                when '+'
                    stack.push(stack.pop() + tmp_number)
                when '-'
                    stack.push(stack.pop() - tmp_number)
                when '*'
                    stack.push(stack.pop() * tmp_number)
                when '/'
                    stack.push(stack.pop() / tmp_number)
            end
        end
    end
    
    stack.pop()
end

# APL
def solve_exp_apl(exp)
    puts exp
end

main() if __FILE__ == $0
