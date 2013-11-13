# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# Add system bash completion. Sometimes this doesn't happen over ssh.
# And on some systems (notably certain versions of RedHat) there is one-shot
# script.
if test -r /etc/bash_completion 
then
	. /etc/bash_completion 
elif test -d /etc/bash_completion.d 
then
	for f in /etc/bash_completion.d/*
	do 
		. $f 2&>/dev/null
	done
fi


# SSH Key management and setup
eval `ssh-agent`

# Let Gnome keyring do it's thing
test -z "$(which gnome-keyring-daemon 2>/dev/null)" || eval $(gnome-keyring-daemon -s)

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
#export HISTCONTROL=ignoreboth


# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


PS1='\u@\h:\w\$ '
unset color_prompt force_color_prompt

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    # eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

# Per machine config
[ -r ~/.bash_local ] && . ~/.bash_local

# Custom ENV init
export PATH=$PATH:~/bin:/usr/local/node/bin
export CDPATH=.:$CDPATH
export PAGER=less
export EDITOR=vim
export PROMPT_COMMAND= # Handy against some forms of juvenile mischief.
export TERM=xterm-256color

# Tell the python interpreter what to load on startup
[ -r ~/.pythonstartup ] && export PYTHONSTARTUP=~/.pythonstartup

# Custom autocomplete

# FIXME: Will crash if git isn't installed. Do I care enough?
_gitsync() { 
   cur=${COMP_WORDS[$COMP_CWORD]}
   COMPREPLY=($(compgen -W "$(git branch | sed s/\*// | sed s/^\ *//)" -- $cur))
   return $?
}
complete -F _gitsync gitsync

# VERY rough autocomplete for drush.
_drush() {
  if [ $COMP_CWORD -gt 1 ]
  then
    COMPREPLY=
    return 0
  fi

  cur=${COMP_WORDS[$COMP_CWORD]}
  RESULT=$(find /usr/local/lib/drush/commands . -name *.drush.inc -exec cat {} + | grep '^ *$items\[' | sed "s/^ *\\$\items\['//" | sed "s/'\].*$//")

  COMPREPLY=($(compgen -W  "$RESULT" -- "$cur"))
  return $?
}
complete -F _drush drush
 
_chef-remote() {
  cur=${COMP_WORDS[$COMP_CWORD]}
  RESULT=$(cat ~/.ssh/known_hosts | grep -v ^\| | cut -d, -f1 | cut -d\  -f1)
  COMPREPLY=($(compgen -W "$RESULT" -- "$cur"))
}
complete -F _chef-remote chef-remote

export VIMCLOJURE_SERVER_JAR="/opt/ng-server/server-2.3.0.jar"
