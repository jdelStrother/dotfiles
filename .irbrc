require 'rubygems'
require 'hirb'
require 'utility_belt'

original_rc = IRB.conf[:IRB_RC]
IRB.conf[:IRB_RC] = lambda do
  original_rc.call
  Hirb::View.enable
end

begin # ANSI codes
  ANSI_BLACK    = "\033[0;30m"
  ANSI_GRAY     = "\033[1;30m"
  ANSI_LGRAY    = "\033[0;37m"
  ANSI_WHITE    = "\033[1;37m"
  ANSI_RED      = "\033[0;31m"
  ANSI_LRED     = "\033[1;31m"
  ANSI_GREEN    = "\033[0;32m"
  ANSI_LGREEN   = "\033[1;32m"
  ANSI_BROWN    = "\033[0;33m"
  ANSI_YELLOW   = "\033[1;33m"
  ANSI_BLUE     = "\033[0;34m"
  ANSI_LBLUE    = "\033[1;34m"
  ANSI_PURPLE   = "\033[0;35m"
  ANSI_LPURPLE  = "\033[1;35m"
  ANSI_CYAN     = "\033[0;36m"
  ANSI_LCYAN    = "\033[1;36m"

  ANSI_BACKBLACK  = "\033[40m"
  ANSI_BACKRED    = "\033[41m"
  ANSI_BACKGREEN  = "\033[42m"
  ANSI_BACKYELLOW = "\033[43m"
  ANSI_BACKBLUE   = "\033[44m"
  ANSI_BACKPURPLE = "\033[45m"
  ANSI_BACKCYAN   = "\033[46m"
  ANSI_BACKGRAY   = "\033[47m"

  ANSI_RESET      = "\033[0m"
  ANSI_BOLD       = "\033[1m"
  ANSI_UNDERSCORE = "\033[4m"
  ANSI_BLINK      = "\033[5m"
  ANSI_REVERSE    = "\033[7m"
  ANSI_CONCEALED  = "\033[8m"

  XTERM_SET_TITLE   = "\033]2;"
  XTERM_END         = "\007"
  ITERM_SET_TAB     = "\033]1;"
  ITERM_END         = "\007"
  SCREEN_SET_STATUS = "\033]0;"
  SCREEN_END        = "\007"
end

begin # Custom Prompt

  if IRB and IRB.conf[:PROMPT]
    IRB.conf[:PROMPT][:SD] = {
      :PROMPT_I => ">> ", # normal prompt
      :PROMPT_S => "%l> ",  # string continuation
      :PROMPT_C => " > ",   # code continuation, first line
      :PROMPT_N => " > ",   # code continuation, remaining lines
      :RETURN   => "#{ANSI_BOLD}# => %s  #{ANSI_RESET}\n",  # return value
      :AUTO_INDENT => true
    }
    IRB.conf[:PROMPT_MODE] = :SD
  end
end


class Object
  def pm(*options) # Print methods
      methods = self.methods
      methods -= Object.methods unless options.include? :more
      filter = options.select {|opt| opt.kind_of? Regexp}.first
      methods = methods.select {|name| name =~ filter} if filter

      data = methods.sort.collect do |name|
        method = self.method(name)
        if method.arity == 0
          args = "()"
        elsif method.arity > 0
          n = method.arity
          args = "(#{(1..n).collect {|i| "arg#{i}"}.join(", ")})"
        elsif method.arity < 0
          n = -method.arity
          args = "(#{(1..n).collect {|i| "arg#{i}"}.join(", ")}, ...)"
        end
        klass = $1 if method.inspect =~ /Method: (.*?)#/
        [name, args, klass]
      end
      max_name = data.collect {|item| item[0].size}.max
      max_args = data.collect {|item| item[1].size}.max
      data.each do |item| 
        print " #{ANSI_BOLD}#{item[0].rjust(max_name)}#{ANSI_RESET}"
        print "#{ANSI_GRAY}#{item[1].ljust(max_args)}#{ANSI_RESET}"
        print "   #{ANSI_LGRAY}#{item[2]}#{ANSI_RESET}\n"
      end
      data.size
    end
  end





def ri(*names)
  puts `ri #{names.map {|name| name.to_s}.join(" ")}`
end

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
