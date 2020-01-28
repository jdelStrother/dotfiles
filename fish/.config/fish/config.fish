source /usr/local/share/chruby/chruby.fish

#echo chruby from config.fish
chruby 2.6.3

set PATH ~/bin ~/go/bin $PATH
set -g fish_user_paths "/usr/local/opt/mysql@5.7/bin" $fish_user_paths

# tabtab source for packages
# uninstall by removing these lines
[ -f ~/.config/tabtab/__tabtab.fish ]; and . ~/.config/tabtab/__tabtab.fish; or true
