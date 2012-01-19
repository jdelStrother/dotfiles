######################################################################
#		      mako's zshrc file, v0.1
#  (plus a few modifications - git branch names in the prompt, for instance)
# 
######################################################################

# next lets set some enviromental/shell pref stuff up
# setopt NOHUP
#setopt NOTIFY
setopt NO_FLOW_CONTROL
setopt APPEND_HISTORY
# setopt AUTO_LIST		# these two should be turned off
# setopt AUTO_REMOVE_SLASH
# setopt AUTO_RESUME		# tries to resume command of same name
unsetopt BG_NICE		# do NOT nice bg commands
setopt CORRECT			# command CORRECTION
setopt EXTENDED_HISTORY		# puts timestamps in the history
# setopt HASH_CMDS		# turns on hashing
setopt HIST_ALLOW_CLOBBER
setopt HIST_REDUCE_BLANKS
setopt INC_APPEND_HISTORY SHARE_HISTORY
setopt ALL_EXPORT

setopt MENUCOMPLETE
# Set/unset  shell options
setopt   notify globdots correct pushdtohome cdablevars autolist listrowsfirst
setopt   correctall autocd longlistjobs
setopt   autoresume histignoredups pushdsilent noclobber
setopt   autopushd pushdminus extendedglob rcquotes mailwarning
unsetopt bgnice autoparamslash

# Autoload zsh modules when they are referenced
zmodload -a zsh/zpty zpty
zmodload -a zsh/zprof zprof
zmodload -ap zsh/mapfile mapfile

TZ="Europe/London"
HISTFILE=$HOME/.zhistory
HISTSIZE=10000
SAVEHIST=10000
HOSTNAME="`hostname`"
PAGER='less'
EDITOR="vim"
VISUAL=mate
GEM_EDITOR=mate

autoload colors zsh/terminfo
export TERM=xterm-color
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi
for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
    eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
    eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
    (( count = $count + 1 ))
done

PR_NO_COLOR="%{$terminfo[sgr0]%}"

LC_ALL='en_US.UTF-8'
LANG='en_US.UTF-8'
LC_CTYPE=C

unsetopt ALL_EXPORT

precmd() {
	local branch_ref branchless_ref

	psvar=()
	branch_ref=$(git symbolic-ref -q HEAD 2>/dev/null)
	cleanliness=""
	# Cleanliness is hard to figure out, and pretty slow, especially from a cold cache.  Only do it for repos that have run "git config --bool jds.showstatus 1"
	if [[ $(git config --bool --get jds.showstatus) = 'true' ]]; then
		(git diff-index --name-only --exit-code HEAD 2>&1) > /dev/null || cleanliness="*"
	else
		cleanliness="+"
	fi

	# We might be on a checked out branch:
	if [[ ! -z "$branch_ref" ]]; then
		psvar[1]="$cleanliness${branch_ref#refs/heads/}"
	else
		branchless_ref=$(git name-rev --name-only HEAD 2>/dev/null)
		if [[ ! -z "$branchless_ref" ]]; then
			# Or we might happen to be on the same commit as (eg) master, but not have it checked out
			psvar[2]="$cleanliness${branchless_ref#(refs/heads/|remotes/)}"
		fi
	fi
}

export PS1="[$PR_GREEN%n$PR_WHITE@$PR_GREEN%U%m%u$PR_NO_COLOR:$PR_RED%2c$PR_NO_COLOR]%(?.$.!) "	# username, underlined host, 2 levels of current directory, '$' if last command succeeded, '!' otherwise.

# right-hand prompt:
BRANCH_PROMPT="{$PR_LIGHT_GREEN%1v$PR_NO_COLOR}"
HEADLESS_PROMPT="{$PR_LIGHT_RED%2v$PR_NO_COLOR}"
# if psvar has 2 elements, we're not on a branch, but still have a suitable git name to use.
# if psvar has 1 element, we're on a checked out branch (and obviously have a suitable git name to use)
# otherwise, use a blank right-prompt.
export RPS1="%(2v.$HEADLESS_PROMPT.%(1v.$BRANCH_PROMPT.))"


autoload -U compinit
compinit
bindkey '^r' history-incremental-search-backward
bindkey '^q' history-incremental-search-forward
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history
bindkey "^[[H" beginning-of-line
bindkey "^[[1~" beginning-of-line
bindkey "^[[F"  end-of-line
bindkey "^[[4~" end-of-line
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand

zstyle ':completion::complete:*' use-cache on
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST

zstyle ':completion:*' list-colors ${(s.:.)LSCOLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the character to insert%s'
zstyle ':completion:*' menu select=1 _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/2 )) numeric )'
zstyle ':completion:*' select-prompt '%SScrolling active: current selection at %p%s'
zstyle ':completion:*:processes' command 'ps -aU$USER'
# Completion Styles
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
# list of completers to use
zstyle ':completion:*::::' completer _expand _complete _ignored _approximate
zstyle -e ':completion:*:approximate:*' \
  max-errors 'reply=( $(( ($#PREFIX:t+$#SUFFIX:h)/3 )) )'
    
# insert all expansions for expand completer
zstyle ':completion:*:expand:*' tag-order all-expansions
#
#NEW completion:
# 1. All /etc/hosts hostnames are in autocomplete
# 2. If you have a comment in /etc/hosts like #%foobar.domain,
#    then foobar.domain will show up in autocomplete!
zstyle ':completion:*' hosts $(awk '/^[^#]/ {print $2 $3" "$4" "$5}' /etc/hosts | grep -v ip6- && grep "^#%" /etc/hosts | awk -F% '{print $2}') 
# formatting and messages
zstyle ':completion:*' verbose yes
zstyle ':completion:*:descriptions' format '%B%d%b'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format 'No matches for: %d'
zstyle ':completion:*:corrections' format '%B%d (errors: %e)%b'
zstyle ':completion:*' group-name ''

# match uppercase from lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# offer indexes before parameters in subscripts
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# command for process lists, the local web server details and host completion
#zstyle ':completion:*:processes' command 'ps -o pid,s,nice,stime,args'
#zstyle ':completion:*:urls' local 'www' '/var/www/htdocs' 'public_html'
zstyle '*' hosts $hosts

# Filename suffixes to ignore during completion (except after rm command)
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns '*?.o' '*?.c~' \
    '*?.old' '*?.pro'
# the same for old style completion
#fignore=(.o .c~ .old .pro)

# ignore completion functions (until the _ignored completer)
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:scp:*' tag-order \
   files users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:scp:*' group-order \
   files all-files users hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order \
   users 'hosts:-host hosts:-domain:domain hosts:-ipaddr"IP\ Address *'
zstyle ':completion:*:ssh:*' group-order \
   hosts-domain hosts-host users hosts-ipaddr
zstyle '*' single-ignored show

zstyle ':completion::complete:git(|-help):*' commands all
zstyle ':completion::complete:git:*' verbose false
zstyle ':completion::*:git:*' list_aliases true

# # --------------------------------------------------------------------
# # aliases
# # --------------------------------------------------------------------
alias dmesg="sudo dmesg"
alias ls='/bin/ls -FGl'
alias ll='ls -al'
alias cp='/bin/cp -i'
alias mv='nocorrect /bin/mv -i'
alias git='nocorrect noglob git'
alias pstree='pstree -g 2'
alias bundle='nocorrect bundle'

# emacs key bindings:
bindkey -e

# fix forward-delete key : 
bindkey "[3~" delete-char

source ~/.cap_completion.zsh

# Do not exit on end-of-file (^D).
setopt IGNORE_EOF

export GREP_OPTIONS='--color=auto' 

# Unset MANPATH (set by path_helper), it's not needed on newer man
unset MANPATH

unalias run-help
autoload -U run-help
HELPDIR=~/lib/zsh-help


function trackingBranch() {
	local MYBRANCH REMOTE BRANCH
	MYBRANCH="$(git symbolic-ref HEAD)"
	MYBRANCH=${MYBRANCH##refs/heads/}

	REMOTE=$(git config --get "branch.$MYBRANCH.remote")
	BRANCH=$(git config --get "branch.$MYBRANCH.merge")
	BRANCH=${BRANCH##refs/heads/}
	if [[ -z $REMOTE ]]; then
		REMOTE=origin
	fi
	if [[ -z $BRANCH ]]; then
		BRANCH=master
	fi
	echo $REMOTE/$BRANCH
}

function ov(){
	eval 'echo "$*"; git log --pretty=format:"%Cred%H%Creset %n%Cblue%an%Creset %s%n" --graph HEAD $(trackingBranch) ^$(git merge-base HEAD $(trackingBranch))~3 $@ | git name-rev --stdin | less -RFX'
}


#gsub on last command  -  s .c .h
function s() {
	eval ${${${(f)"$(history -n)"}[-1]}//$1/$2}
}

if [[ -f ~/bin/j.sh ]]; then
  source ~/bin/j.sh
fi

alias gcruby='~/rubygc/bin/ruby'
alias gcrake='~/rubygc/bin/rake'
alias gcgem='~/rubygc/bin/gem'
alias gcirb='~/rubygc/bin/irb'
alias gcrails='~/rubygc/bin/rails'
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
# Make sure the rubyinline env var is set up:
[[ -s "$HOME/.rvm/hooks/after_use" ]] && source "$HOME/.rvm/hooks/after_use"

if [[ $TERM_PROGRAM == "Apple_Terminal" ]] && [[ -z "$INSIDE_EMACS" ]] {
  function chpwd {
    local SEARCH=' '
    local REPLACE='%20'
    local PWD_URL="file://$HOSTNAME${PWD//$SEARCH/$REPLACE}"
    printf '\e]7;%s\a' "$PWD_URL"
  }

  chpwd
}

