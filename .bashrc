# Not much is original in here. As I get older I seem to be getting
# more and more comfortable using the default settings. I should think
# more deeply about what this means...

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

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

# Damned bell...
xset -b

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

alias emacsnw='emacsclient -nw'
alias emacs='emacsclient -c'

export LEIN_SNAPSHOTS_IN_RELEASE=y

# Per machine config
[ -r ~/.bash_local ] && . ~/.bash_local

# Custom ENV init
export PATH=$PATH:~/bin:/usr/local/node/bin:/opt/LightTable
export CDPATH=.:$CDPATH
export PAGER=less
export EDITOR=vim
export PROMPT_COMMAND= # Handy against some forms of juvenile mischief.

# TODO: This should be covered in .Xresources. Check.
export TERM=xterm-256color

# Tell the python interpreter what to load on startup
[ -r ~/.pythonstartup ] && export PYTHONSTARTUP=~/.pythonstartup

# Fix boot version due to a bug in 2.4.x

export BOOT_VERSION=2.2.0

# Speed up boot start time
export BOOT_JVM_OPTIONS="-client 
-XX:+TieredCompilation 
-XX:TieredStopAtLevel=1 
-Xmx2g 
-XX:+UseConcMarkSweepGC 
-XX:+CMSClassUnloadingEnabled 
-Xverify:none"

