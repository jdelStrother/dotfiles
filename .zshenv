export RI="--no-pager --format ansi --width 100"
export LC_CTYPE=en_US.UTF-8 # For the benefit of subversion
export CLICOLOR=1
# use yellow for directories
export LSCOLORS=Dxfxcxdxbxegedabagacad

typeset -U path
path=(~/bin $path)
unset manpath
export KEYSTORE=/Users/jon/Documents/Certificates/keystore.ImportKey
