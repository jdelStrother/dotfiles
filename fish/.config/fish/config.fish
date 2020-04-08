if status --is-login
  # chruby brings a lot of stuff into PATH and rearranges ordering
  # which messes with, eg, nix-shell.  Let's try only sourcing it in login shells
  source /usr/local/share/chruby/chruby.fish
  chruby 2.6.3
end

set PATH ~/bin ~/go/bin $PATH

if status --is-login
  scmpuff init -s --shell=fish | source
end

# Set up direnv hook. We'd normally use:
# eval (direnv hook fish)
# But it then doesn't trigger on opening a new shell
# https://github.com/direnv/direnv/issues/583
function __direnv_export_eval --on-event fish_prompt;
	"/usr/local/bin/direnv" export fish | source;
end

