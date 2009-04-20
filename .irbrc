require 'rubygems'
require 'utility_belt'
# 
# require 'irb/completion'
# ARGV.concat [ "--readline", "--prompt-mode simple" ]
# 
# HISTFILE = "~/.irb.hist" unless defined? HISTFILE
# MAXHISTSIZE = 10000 unless defined? MAXHISTSIZE
# begin
#   if defined? Readline::HISTORY
#     histfile = File::expand_path( HISTFILE )
#     if File::exists?( histfile )
#       lines = IO::readlines( histfile ).collect {|line| line.chomp}
#       puts "Read %d saved history commands from %s." %
#         [ lines.nitems, histfile ] if $DEBUG || $VERBOSE
#       Readline::HISTORY.push( *lines )
#     else
#       puts "History file '%s' was empty or non-existant." %
#         histfile if $DEBUG || $VERBOSE
#     end
# 
#     Kernel::at_exit {
#       lines = Readline::HISTORY.to_a.reverse.uniq.reverse
#       lines = lines[ -MAXHISTSIZE, MAXHISTSIZE ] if lines.nitems > MAXHISTSIZE
#       $stderr.puts "Saving %d history lines to %s." %
# 
#         [ lines.length, histfile ] if $VERBOSE || $DEBUG
#       File::open( histfile, File::WRONLY|File::CREAT|File::TRUNC ) {|ofh|
#         lines.each {|line| ofh.puts line }
#       }
#     }
#   end
# end
# 
# def ri(*names)
#   system(%{ri -Tf ansi #{names.map {|name| name.to_s}.join(" ")}})
# end
# 
# IRB.conf[:PROMPT_MODE] = :SIMPLE
# IRB.conf[:PROMPT][:SIMPLE][:RETURN] = "[37m\=> %s[0m\n" if IRB.conf[:PROMPT]  # need to check for [:PROMPT], or mongrel complains loudly
# 
# IRB.conf[:IRB_RC] = proc do |conf|
#   leader = " " * conf.irb_name.length
#   conf.prompt_i = "#{conf.irb_name} --> "
#   conf.prompt_s = leader + ' \-" '
#   conf.prompt_c = leader + ' \-+ '
#   conf.return_format = leader + " ==> %s\n\n"
#   puts "Welcome to interactive ruby!"
# end
# 
# 
class Object
  def dclone
    clone
  end
end
class Array
  def dclone
    klone = self.clone
    klone.clear
    self.each{|v| klone << v.dclone}
    klone
  end
end

class Object
  def what?(*args, &block)
    MethodFinder.new(self, *args, &block)
  end

  def _clone_
    self.clone
  rescue
    self
  end
end

class MethodFinder
  @@black_list = %w[ what? _clone_ exec exit exit! fork sleep syscall system ]

  def initialize(obj, *args, &block)
    @obj = obj
    @args = args
    @block = block
  end

  def self.write(*args)
  end

  # Find all methods on obj which, when called with *args, &block return result
  def self.find(obj, result, *args, &block)
    stdout, stderr = $stdout, $stderr
    $stdout = $stderr = self
    methods = obj.methods.select do |name|
      arity = obj.method(name).arity
      local_args = args.dclone
      !@@black_list.include?(name)                                       and
      (arity == args.size) || (arity < 0 && (arity+1).abs <= args.size)  and
      begin obj._clone_.send(name, *local_args, &block) == result; rescue Object; end
    end
    $stdout, $stderr = stdout, stderr
    methods
  end

  # As find method but also Pretty-prints the results
  def self.show(obj, result, *args, &block)
    args_string = args.empty? ? '' : "(" + args.map {|a| a.inspect}.join(", ") + ")"
    block_string = block.nil? ? '' : " {...}"
    find(obj, result, *args, &block).each do |name|
      puts "#{obj.inspect}.#{name}#{args_string}#{block_string} == #{result.inspect}"
    end
  end

  def ==(result)
    # MethodFinder.find(@obj, result, *@args, &@block)
    MethodFinder.show(@obj, result, *@args, &@block)
  end

end
