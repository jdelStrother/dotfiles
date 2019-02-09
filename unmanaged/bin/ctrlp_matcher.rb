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
end

class FileMatcher < Matcher
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

class TagMatcher < Matcher
  def matching_paths_by_score
    matching_paths.sort_by{|c| c.length}
  end
end

class FileCorpus
  def initialize(path)
    @path = path
  end
  def phrases
    File.read(@path).split("\n")
  end
  def results_for_phrases(phrases)
    phrases
  end
end

class TagCorpus
  def initialize(path)
    @path = path
  end
  def phrases
    tags_and_context.keys
  end
  def results_for_phrases(phrases)
    phrases.map{|p| tags_and_context[p]}
  end
private
  def tags_and_context
    # We're searching a ctags file, which looks something like "create\tapp/models/boo.rb\tdef create(blah)"
    @tags_and_context ||= begin
                            tags = {}
                            File.read(@path).split("\n").each{|l| tags[l.split("\t")[0]] = l}
                            tags
                          end
  end
end

def ctrlp_matches(search_limit, cache_file, mode, search_term)
  log("Searching for #{search_limit}, #{cache_file}, #{mode}, #{search_term}")
  if search_term.length==0
    return File.read(cache_file).split("\n")
  end
  if mode=='first-non-tab'
    m = TagMatcher.new(search_term)
    corpus = TagCorpus.new(cache_file)
  else
    m = FileMatcher.new(search_term)
    corpus = FileCorpus.new(cache_file)
  end
  m.corpus = corpus.phrases
  matching_terms = m.matching_paths_by_score[0..search_limit]
  corpus.results_for_phrases(matching_terms)
rescue Exception=>e
  log e.inspect
ensure
  log("Done")
end  


def vim_ctrlp_matches(search_limit, cache_file, mode, search_term)
  result = ctrlp_matches(search_limit, cache_file, mode, search_term)
  Vim::command("let result = #{result.inspect}")
end

def log(msg)
  return unless $VERBOSE
  path = File.join(File.dirname(__FILE__), 'ctrlp.log')
  File.open(path, 'a'){|f| f.write "#{Time.now} #{msg}\n"}
end

if __FILE__==$0
  # we'll get invoked with the arguments '<LIMIT> <MANIFEST_PATH> <MODE> <SEARCH TERMS>'
  search_limit = ARGV[0].to_i
  cache_file = ARGV[1]
  mode = ARGV[2]
  search_term = ARGV[3..-1].join(" ")
  
  puts ctrlp_matches(search_limit, cache_file, mode, search_term).join("\n")
end
  
