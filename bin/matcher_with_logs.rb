#!/usr/bin/env ruby

def benchmark(title, output)
  start = Time.now
  yield
ensure
  end_time = Time.now
  output.puts("#{title} - #{((end_time-start)*1000).to_i}ms")
end

begin
File.open('/Users/jon/matcher.log', 'w+') do |log|
  log.puts RUBY_VERSION
  log.puts(ARGV.inspect)
cache_file=nil, search_limit=nil, search_term=nil, paths=nil, matcher=nil, matching_paths=nil, greedy_matcher=nil, search_letters=nil

benchmark('startup', log) do
  # vim calls "matcher.rb --limit x --manifest y some search terms"
  search_limit = ARGV[1].to_i
  cache_file = ARGV[3]
  search_term = ARGV[4..-1].join(" ")
  # search_term = "app/user"#ARGV[4..4].join(" ")
end

if search_term.length==0
  puts File.read(cache_file)
  exit(0)
end


benchmark('regexp', log) do
  # take each individual character and regex-escape it.  Spaces will match both literal space and a directory slash.
  search_letters = search_term.split('').map{|l| Regexp.escape(l.gsub(/ /,'[ /]'))}
  greedy_matcher = Regexp.new(search_letters.join('.*'))
  matcher =        Regexp.new(search_letters.join('.*?'))
end

benchmark('matching', log) do
  # Greedy matching ('.*') is much faster than non-greedy ('.*?')
  # Pre-filter the paths to find ones that match the overall regex using the greedy match:
  # matching_paths = File.read(cache_file).split("\n").select{|p,m| p=~greedy_matcher}
  matching_paths = File.read(cache_file).scan(Regexp.new("^.*#{search_letters.join('.*')}.*$"))
  # matching_paths = STDIN.read.scan(Regexp.new("^.*#{search_letters.join('.*')}.*$"))
end

def count_coinciding_initial_letters(path, letters)
  matching_count = 0
  components = path.split("/")
  while component=components.shift
    while component and next_letter=letters.shift
      next if next_letter=='/' or next_letter==' '
      matching_count += 1 if component[0]==next_letter
      component = component[1..-1]
    end
  end
  matching_count
end

benchmark('sorting', log) do
  # Determine a 'score' for each path that shows how suitable each is, then print the best scoring paths.
  # A lower score is better.
  puts matching_paths.sort_by{|path|
    # using the non-greedy match, sort the matching paths by their minimum match range.
    # eg, if we search for 'ac' in abcd and abdc, the former should score higher because the match only spans 3 characters rather than 4.
    start_match,end_match = *matcher.match(path).offset(0)
    score = end_match - start_match
    # Paths where the match intersects the filename are better (eg when searching for 'foo', 'dir/foo.c' should score higher than 'dir/foo/bar.c')
    end_of_match_is_in_file_component = File.dirname(path).length < end_match
    score -= 5 if end_of_match_is_in_file_component
    # improve the score where the initial letters of path components match the search terms.
    # ie, when searching for foo/bar.c, "f/b" is a better search term than "f/a"
    # score -= 2*count_coinciding_initial_letters(path, search_term.split(''))
    
    # path.sub!(/$/, " #{count_coinciding_initial_letters(path, search_term.split(''))}")
    
    score
  }.map{|p,m| p}[0..search_limit]
end

log.write("\n\n")
end
rescue Exception => e
  puts e.inspect
end
