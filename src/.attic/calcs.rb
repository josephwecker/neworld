#!/usr/bin/env ruby

(1..20).each do |i|
  len = i * 2
  puts '----------------------'
  puts "orientation - cardinal"
  puts "unit :        #{len}"
  puts "unit/2:       #{len / 2.0}"
  puts ''
  len = Math.sqrt(len**2 + len**2)
  puts "orientation - diagonal"
  puts "sqrt(2u^2):   #{len}"
  puts "sqrt(2u^2)/2: #{len / 2.0}"
  puts ''
end
