#!/usr/bin/env ruby
# Author: Jehadur Rahman
# Organization: CyArm
# GitHub: https://github.com/JehadurRE
# Website: jehadurre.me
# Date: 2025-07-10 09:43 (Thu)

# by Andronik Ordian

def fib_partial_sum(m, n)
  sum = 0
  current = 0
  next_val = 1
  from = m % 60
  to = n % 60
  
  to += 60 if to < from
  
  (0..to).each do |i|
    sum += current if i >= from
    
    new_current = next_val
    next_val = (next_val + current) % 10
    current = new_current
  end
  
  sum % 10
end

if __FILE__ == $0
  m, n = gets.split().map(&:to_i)
  puts "#{fib_partial_sum(m, n)}"
end
