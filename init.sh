#!/bin/bash
defaults write ~/Library/Preferences/com.googlecode.iterm2 GlobalKeyMap "$(cat iTerm/GlobalKeyMap)"

mkdir -p ~/emacs
if [[ ! -d ~/emacs/spacemacs ]]; then
  git clone -b develop https://github.com/syl20bnr/spacemacs ~/emacs/spacemacs
fi
if [[ ! -d ~/emacs/chemacs ]]; then
  git clone https://github.com/plexus/chemacs.git ~/emacs/chemacs
  ~/emacs/chemacs/install.sh
fi
if [[ ! -d ~/emacs/doom-emacs ]]; then
  git clone -b develop https://github.com/hlissner/doom-emacs ~/emacs/doom-emacs
  echo 'After running `./stow.sh emacs`, you should run `~/emacs/doom-emacs/bin/doom install`'
fi

# Emacs.app is installed with something along the lines of:
# git clone https://git.savannah.gnu.org/git/emacs.git
# cd emacs
# ./configure --with-ns && make && make install && mv nextstep/Emacs.app /Applications/
# ~/emacs/doom-emacs/bin/doom sync
