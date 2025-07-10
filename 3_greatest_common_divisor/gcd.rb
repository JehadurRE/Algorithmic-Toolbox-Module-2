#!/usr/bin/env ruby
# Author: Jehadur Rahman
# Organization: CyArm
# GitHub: https://github.com/JehadurRE
# Website: jehadurre.me
# Date: 2025-07-10 09:43 (Thu)

# by Andronik Ordian

def gcd(a, b)
  return a if b == 0
  a = a % b
  gcd(b, a)
end

if __FILE__ == $0
  a, b = gets.split().map(&:to_i)
  puts "#{gcd(a, b)}"
end