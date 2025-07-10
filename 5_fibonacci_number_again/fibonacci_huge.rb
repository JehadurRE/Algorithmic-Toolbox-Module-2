#!/usr/bin/env ruby
# Author: Jehadur Rahman
# Organization: CyArm
# GitHub: https://github.com/JehadurRE
# Website: jehadurre.me
# Date: 2025-07-10 09:43 (Thu)

# by Andronik Ordian

def find_pisano_period(m)
  prev = 0
  current = 1
  
  (0...m * m).each do |i|
    temp = current
    current = (prev + temp) % m
    prev = temp
    
    return i + 1 if prev == 0 && current == 1
  end
  0
end

def fib_huge(n, m)
  p = find_pisano_period(m)
  n = n % p
  
  return n if n <= 1
  
  previous = 0
  current = 1
  
  (0...n - 1).each do |i|
    tmp_previous = previous
    previous = current
    current = (tmp_previous + current) % m
  end
  
  current
end

if __FILE__ == $0
  a, b = gets.split().map(&:to_i)
  puts "#{fib_huge(a, b)}"
end