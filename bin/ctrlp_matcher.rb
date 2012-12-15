class Matcher
  attr_accessor :corpus, :search_term
  def initialize(search_term)
    @search_term = search_term.downcase
  end
  def search_letters
    # take each individual character and regex-escape it.  Spaces will match both literal space and a directory slash.    
    @search_letters ||= search_term.split('').map{|l| Regexp.escape(l.gsub(/ /,'[ /]'))}
  end
  def greedy_matcher
    @greedy_matcher ||= Regexp.new(search_letters.join('.*'), Regexp::IGNORECASE)
  end
  def generous_matcher
    @generous_matcher ||= Regexp.new(search_letters.join('.*?'), Regexp::IGNORECASE)
  end
  def lookahead_matcher
    @lookahead_matcher ||= Regexp.new("#{search_letters[0]}(?=.*?#{search_letters[1..-1].join('.*?')})", Regexp::IGNORECASE)
  end
  def matching_paths
    # Greedy matching ('.*') is much faster than non-greedy ('.*?')                                                       
    # Pre-filter the paths to find ones that match the overall regex using the greedy match:                              
    @matching_paths ||= corpus.select{|p,m| p=~greedy_matcher}
  end
  def matching_paths_by_score
    # Determine a 'score' for each path that shows how suitable each is, then print the best scoring paths.               
    # A lower score is better.                                                                                            
    matching_paths.map{|path|                                                                                    
      # using the non-greedy match, sort the matching paths by their minimum match range.                                 
      # eg, if we search for 'ac' in abcd and abdc, the former should score higher because the                            
      # match only spans 3 characters rather than 4.
      best_match = path.enum_for(:scan, lookahead_matcher).map{
        match = $~
        class<<match
          attr_accessor :score
        end
        match.score = begin
          start_of_match = match.offset(0)[0]
          end_of_match = match.offset(0)[1]
          score = ($&+$')[generous_matcher].length
          # Paths where the match intersects the filename are better (eg when searching for 'foo',                            
          # 'dir/foo.c' should score higher than 'dir/foo/bar.c')                                                             
          start_of_match_is_in_file_component = File.dirname(path).length < start_of_match
          end_of_match_is_in_file_component = File.dirname(path).length < end_of_match                                           
          score -= 5 if start_of_match_is_in_file_component                                    
          score -= 5 if end_of_match_is_in_file_component                                    
          score
        end
        match
      }.min_by(&:score)
                                                                                                                      
      [path, best_match.score]                                                                                                        
    }.sort_by{|p,s| s}.map{|p,score| p}
  end
end


# we'll get invoked with the arguments '<LIMIT> <MANIFEST_PATH> <SEARCH TERMS>'
search_limit = ARGV[0].to_i
cache_file = ARGV[1]
search_term = ARGV[2..-1].join(" ")

if search_term.length==0
  puts File.read(cache_file)
  exit(0)
end

m = Matcher.new(search_term)
m.corpus = File.read(cache_file).split("\n")
puts m.matching_paths_by_score[0..search_limit]
