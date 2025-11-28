#!/usr/bin/env ruby

def build_state(log_line)
  # strip out the ANSI stuff
  log_line = log_line.gsub(/\e\[(\d+)m/, '')
  sha = log_line.scan(/\*[|\\\/ ]* ([0-9a-f]{4,})/)[0]
  if sha && sha[0]
    notes = `git notes show #{sha[0]} 2>/dev/null`.strip
    if notes =~ /CI build (.*)/
      return "\e[31m[#{$1}]\e[0m "
    end
  end
end

begin
  ARGF.each do |log_line|
    puts log_line.sub("<<<NOTE>>> ", build_state(log_line)||'')
  end
rescue Errno::EPIPE
end
