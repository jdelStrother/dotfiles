export RI="--no-pager --format ansi --width 100"
export LC_CTYPE=en_US.UTF-8 # For the benefit of subversion
export CLICOLOR=1
# use yellow for directories
export LSCOLORS=Dxfxcxdxbxegedabagacad

typeset -U path
path=(~/bin /usr/local/bin $path)
unset manpath
export KEYSTORE=/Users/jon/Documents/Certificates/javaKeystore.ImportKey

export NODE_PATH=/usr/local/lib/node_modules:$NODE_PATH
PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
