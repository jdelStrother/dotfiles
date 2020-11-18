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

# not sure of the best way to get this to autoload, so source it manually
source (dirname (status --current-filename))/completions/git-lg.fish

