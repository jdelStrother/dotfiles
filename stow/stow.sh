#! /bin/bash

if [ "$#" -eq 0 ]; then
  args="docker emacs fish git tmux tools vim"
else
  args="$@"
fi

function is_bin_in_path {
  builtin type -P "$1" &> /dev/null
}

if ! is_bin_in_path stow ; then
  echo "install stow first. Or run:"
  echo 'nix-shell -p stow --run "./stow.sh ..."'
  exit 1
else
  set -x
  stow -t ~ -v --ignore='(.elc|.DS_Store)' $args
fi
