case $- in
  *i*)
    [ -r /etc/bash.bashrc ] && . /etc/bash.bashrc
    . /etc/bash_completion
    . ~/.bashrc
    ;;
esac

