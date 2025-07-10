#!/usr/bin/env ruby
# Author: Jehadur Rahman
# Organization: CyArm
# GitHub: https://github.com/JehadurRE
# Website: jehadurre.me
# Date: 2025-07-10 09:43 (Thu)

# by Andronik Ordian

def fib_sum_last_digit(n)
  n = n % 60  # Pisano period for mod 10
  
  return n if n <= 1
  
  previous = 0
  current = 1
  sum = 1
  
  (0...n - 1).each do |i|
    tmp_previous = previous
    previous = current
    current = (tmp_previous + current) % 10
    sum = (sum + current) % 10
  end
  
  sum
end

if __FILE__ == $0
  n = gets.to_i
  puts "#{fib_sum_last_digit(n)}"
end
