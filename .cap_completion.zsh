# 99% of this is ripped straight from http://weblog.rubyonrails.com/2006/3/9/fast-rake-task-completion-for-zsh
# Any broken stuff is probably mine.
# To use, save as .cap_completion.zsh in your home dir, and add "source .cap_completion.zsh" to your .zshrc file

_cap_does_task_list_need_generating () {
  if [ ! -f .cap_tasks ]; then return 0;
  else
    accurate=$(stat -f%m .cap_tasks)
    changed=$(stat -f%m config/deploy.rb)
    return $(expr $accurate '>=' $changed)
  fi
}

_cap () {
  if [ -f config/deploy.rb ]; then
    if _cap_does_task_list_need_generating; then
      cap -vT | cut -d " " -f 2 | sed -e '/^ *$/D' -e '1,2D' >! .cap_tasks
    fi
    compadd `cat .cap_tasks`
  fi
}

compdef _cap cap
