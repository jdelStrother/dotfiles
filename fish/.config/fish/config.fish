set PATH ~/bin ~/go/bin $PATH

if status --is-login && which scmpuff > /dev/null
  scmpuff init -s --shell=fish | source
end

# Set up direnv hook. We'd normally use:
# eval (direnv hook fish)
# But it then doesn't trigger on opening a new shell
# https://github.com/direnv/direnv/issues/583
if which direnv > /dev/null
  function __direnv_export_eval --on-event fish_prompt;
    direnv export fish | source;
  end
end

# Allow gpg signing via the terminal if we connect over ssh
# (Otherwise pinentry-mac will pop up a GUI window)
if test -n "$SSH_CONNECTION"
  set GPG_TTY (tty)
  set PINENTRY_USER_DATA "USE_CURSES=1"
end

set dir (dirname (status --current-filename))

# not sure of the best way to get this to autoload, so source it manually
source $dir/completions/git-lg.fish

source $dir/iterm2_shell_integration.(basename $SHELL)

# I use 24 bit color in iterm, but that's not going to work on most ssh hosts...
alias ssh="TERM=xterm-256color command ssh"
