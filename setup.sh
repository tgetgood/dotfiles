SCRIPT_PATH=$(readlink -f $(dirname "${VIRTUAL_ENV}"))
ln -s $SCRIPT_PATH/vimrc ~/.vimrc
ln -s $SCRIPT_PATH/vim ~/.vim
ln -s $SCRIPT_PATH/bashrc ~/.bashrc
ln -s $SCRIPT_PATH/gitconfig ~/.gitconfig
ln -s $SCRIPT_PATH/my.cnf ~/.my.cnf
