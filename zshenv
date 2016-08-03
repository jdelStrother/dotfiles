export RI="--no-pager --format ansi --width 100"
export LC_CTYPE=en_US.UTF-8 # This is the 21st century, assume UTF8
export CLICOLOR=1
# use yellow for directories
export LSCOLORS=Dxfxcxdxbxegedabagacad

if [[ -z $TMUX ]]; then # tmux's reattach-to-user-namespace command causes this file to get sourced twice, which makes rvm complain about path ordering
  typeset -U path
  path=(~/bin /usr/local/bin $path)
  unset manpath
  export KEYSTORE=/Users/jon/Documents/Certificates/javaKeystore.ImportKey

  export NODE_PATH=/usr/local/lib/node_modules:$NODE_PATH

fi

# Save our path to avoid paths_helper mangling it
# http://openradar.appspot.com/14630658
# See zshrc
FINALPATH=$PATH
