#!/bin/bash
#locate the dir where this script is stored
export scmbDir="$( cd -P "$( dirname "$0" )" && pwd )"

# Symlink to ~/.scm_breeze if installing from another path
if [ "$scmbDir" != "$HOME/.scm_breeze" ]; then
  ln -fs "$scmbDir" "$HOME/.scm_breeze"
fi

# This loads SCM Breeze into the shell session.
exec_string="[ -s \"$HOME/.scm_breeze/scm_breeze.sh\" ] && source \"$HOME/.scm_breeze/scm_breeze.sh\""

# Add line to bashrc and zshrc if not already present.
for rc in bashrc zshrc; do
  if [ -s "$HOME/.$rc" ] && ! grep -q "$exec_string" "$HOME/.$rc"; then
    printf "\n$exec_string\n" >> "$HOME/.$rc"
    printf "== Added SCM Breeze to '~/.$rc'\n"
  fi
done

# Load SCM Breeze update scripts
source "$scmbDir/lib/scm_breeze.sh"
# Create '~/.*.scmbrc' files from example files
_create_or_patch_scmbrc


echo "== Run 'source ~/.bashrc' or 'source ~/.zshrc' to load SCM Breeze into your current shell."
