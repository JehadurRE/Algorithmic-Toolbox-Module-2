#!/usr/bin/env ruby
# Author: Jehadur Rahman
# Organization: CyArm
# GitHub: https://github.com/JehadurRE
# Website: jehadurre.me
# Date: 2025-07-10 09:43 (Thu)

# by Andronik Ordian

def calc_fib_naive(n)
  return n if n <= 1
  calc_fib_naive(n - 1) + calc_fib_naive(n - 2)
end

def calc_fib(n)
  return n if n <= 1
  
  arr = Array.new(n + 1)
  arr[0] = 0
  arr[1] = 1
  
  (2..n).each do |i|
    arr[i] = arr[i - 1] + arr[i - 2]
  end
  
  arr[n]
end

def test_solution
  raise "Test failed" unless calc_fib(3) == 2
  raise "Test failed" unless calc_fib(10) == 55
  (0...20).each do |n|
    raise "Test failed for n=#{n}" unless calc_fib(n) == calc_fib_naive(n)
  end
end

if __FILE__ == $0
  n = gets.to_i
  # puts "#{calc_fib_naive(n)}"  # Slow version
  # test_solution  # Test the implementation
  puts "#{calc_fib(n)}"
end
