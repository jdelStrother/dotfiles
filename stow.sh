#! /bin/bash

set -x
if [ "$#" -eq 0 ]; then
  args="docker emacs fish git tmux tools vim"
else
  args="$@"
fi

stow -t ~ -v --ignore .DS_Store $args
