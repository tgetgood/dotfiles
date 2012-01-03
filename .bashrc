# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth
export PROMPT_COMMAND=

#Default editor
export EDITOR=vim

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

#if [ -f ~/.bash_aliases ]; then
#    . ~/.bash_aliases
#fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
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

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

#custom additions
alias ack="ack-grep -a"

alias tagthis="ctags --langmap=php:.module.inc.engine -R site/{includes,modules,themes/engines,sites}/*"
alias tagphp="ctags --langmap=php:.module.inc.engine.theme.php --php-kinds=cdfi --languages=php --recurse"
export CDPATH=.:/var/shared/sites/:/home/thomas/projects
export PAGER=less
#PATH additions
export PATH=$PATH:/home/thomas/bin:/usr/local/node/bin
# Tell the python interpreter what to load on startup
[ -r ~/.pythonstartup ] && export PYTHONSTARTUP=~/.pythonstartup

function vimso() {
FILE=${1:-~/.bashrc}
vim $FILE
source $FILE
}

function mcd() {
mkdir $1
cd $1
}

function scopethis() {
REPOS=`pwd`
cscope -b -q -u -i<(find $REPOS -path "\.git/*" -prune , -path '.svn/*' -prune ,  \( -name "*\.module" -o -name "*\.inc" -o -name "*\.php" -o -name "*\.install" -o -name "*\.engine" -o -name "*\.test" -o -name "*\.theme" \))

export CSCOPE_DB="$REPOS/cscope.out"
}

alias tailerror="sudo tail -f /var/log/apache2/error.log"
alias cccc="drush cc all && drush cc all && drush cc all && drush cc all"
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# Custom autocomplete

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
 
[ -r ~/.bash_local ] && . ~/.bash_local
