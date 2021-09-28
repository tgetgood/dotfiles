case $- in
  *i*)
    [ -r /etc/bash.bashrc ] && . /etc/bash.bashrc
    . ~/.bashrc
    ;;
esac

if [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    exec startx
fi
