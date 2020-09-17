# Not much is original in here. As I get older I seem to be getting
# more and more comfortable using the default settings. I should think
# more deeply about what this means...

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

export LEIN_SNAPSHOTS_IN_RELEASE=y

# Per machine config
[ -r ~/.bash_local ] && . ~/.bash_local

# Custom ENV init
export PATH=~/bin:~/.cargo/bin:$PATH
export PAGER=less
export EDITOR=vim
export BROWSER=firefox
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"
export PROMPT_COMMAND= # Handy against some forms of juvenile mischief.
export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"

# Tell the python interpreter what to load on startup
[ -r ~/.pythonstartup ] && export PYTHONSTARTUP=~/.pythonstartup

