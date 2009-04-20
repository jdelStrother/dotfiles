export GEMS=/usr/local/lib/ruby/gems/1.8/gems
export RI="--no-pager --format ansi --width 100"
export LC_CTYPE=en_US.UTF-8 # For the benefit of subversion
export CLICOLOR=1
# use yellow for directories
export LSCOLORS=Dxfxcxdxbxegedabagacad

typeset -U path
path=(~/bin /opt/local/bin $path)
unset manpath
