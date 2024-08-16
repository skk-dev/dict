#!/usr/bin/env ruby
# -*- coding: utf-8 -*-
## Copyright (C) 2005 MITA Yuusuke <clefs@mail.goo.ne.jp>
##
## Author: MITA Yuusuke <clefs@mail.goo.ne.jp>
## Maintainer: SKK Development Team <skk@ring.gr.jp>
## Keywords: japanese, dictionary
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2, or (at your option)
## any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program, see the file COPYING.  If not, write to the
## Free Software Foundation Inc., 51 Franklin St, Fifth Floor, Boston,
## MA 02110-1301, USA.
##
### Instruction:
##
## 

require 'jcode' if RUBY_VERSION.to_f < 1.9
#require 'kconv'
require 'skkdictools'
require 'optparse'
opt = OptionParser.new

keep_annotation = false
output_all = true
unannotate_unique = false
unannotate_cap = 99999999
doublebar = "remove"
rulesets = Array.new
default_rulesets = [
  [ "exclude", '※|\?$' ],
  # [ "exclude", "\[卑\]" ],
  [ "keep", '旧字|異体字|本字|大字|†|→' ],
  # [ "keep", "NB:|=|≒|≠|和製|<rare>" ],
  # [ "cut", "‖" ] - 'doublebar' handles it inplace
]
encoding = "euc-jis-2004"


opt.on('-c pattern', 'cut annotations after <pattern>') { |pattern| rulesets << [ "cut", pattern]}
opt.on('-e pattern', 'eliminate candidates if <pattern> matches') { |pattern| rulesets << [ "exclude", pattern]}
opt.on('-x pattern', 'output pairs if <pattern> matches (use with -t)') { |pattern| rulesets << [ "extract", pattern]}
opt.on('-u pattern', 'unannotate candidates if <pattern> matches (use with -k)') { |pattern| rulesets << [ "unannotate", pattern]}
opt.on('-U pattern', 'keep annotations matching <pattern>') { |pattern| rulesets << [ "keep", pattern]}

opt.on('-s', 'unannotate if the candidate is "unique"') { unannotate_unique = true }
opt.on('-j VAL', "never unannotate if an entry has more than <VAL> candidates") { |v| unannotate_cap = v.to_i }
opt.on('-k', 'keep annotations by default') { keep_annotation = true }
opt.on('-t', "extraction mode: output requested pairs only") { output_all = false }
opt.on('-d', "apply default rulesets") { rulesets += default_rulesets }
opt.on('-8', "read and write in utf8") { encoding = "utf-8" }

opt.on('-b', "sticky '‖' -- annotation after '‖' will always be kept") { doublebar = "sticky" }
#opt.on('-B', "always remove annotations after '‖'") { doublebar = "remove" }
opt.on('-B', "treat '‖' as a part of annotation") { doublebar = "dumb" }


begin
  opt.parse!(ARGV)
  #rulesets = default_rulesets if rulesets.empty?
rescue OptionParser::InvalidOption
  print "'#{$0} -h' for help.\n"
  exit 1
end
Encoding.default_external = encoding
STDOUT.set_encoding(encoding, "utf-8")


while gets
  $_.encode!("utf-8")
  next if $_ =~ /^;/ || $_ =~ /^$/
  midasi, tokens = $_.parse_skk_entry
  total = tokens.count {|item| !item.nil? }
  #results = Array.new

  tokens.each do |token|
    word, annotation, comment = token.skk_split_tokens( doublebar == "dumb" ? nil : '‖')

    do_unannotate = !keep_annotation
    do_output = output_all
    do_unannotate = true if unannotate_unique && total == 1
    do_unannotate = false if unannotate_cap <= total

    rulesets.each do |rule|
      if !annotation.nil?
	match = (annotation =~ Regexp.compile(rule[1]))
	if match
	  case rule[0]
	  when "cut"
	    annotation = annotation[0, match]
	  when "extract"
	    do_output = true
	  when "exclude"
	    do_output = false
	  when "unannotate"
	    do_unannotate = true
	  when "keep"
	    do_unannotate = false
	  end
	end
      end
    end
    next if !do_output
    #results << [word, do_unannotate ? nil : annotation, doublebar == "sticky" ? comment : nil]
    print_pair(midasi, word, do_unannotate ? nil : annotation, doublebar == "sticky" ? comment : nil)
  end
end
