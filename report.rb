#!/usr/bin/env ruby

require 'simplecov'

SimpleCov.start
SimpleCov.command_name 'Touchdown Tests'

system("make")
