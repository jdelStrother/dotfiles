#!/bin/bash
defaults write ~/Library/Preferences/com.googlecode.iterm2 GlobalKeyMap "$(cat iTerm/GlobalKeyMap)"

if [[ ! -d ~/.emacs.d ]]; then
  git clone -b develop --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
  echo 'After running `./stow.sh emacs`, you should run `~/.emacs.d/bin/doom install`'
fi


# Emacs.app is installed with something along the lines of:
# git clone https://git.savannah.gnu.org/git/emacs.git
# cd emacs
# ./configure --with-ns && make && make install && mv nextstep/Emacs.app /Applications/
# ~/emacs/doom-emacs/bin/doom sync
