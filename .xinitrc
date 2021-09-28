#!/usr/bin/env bash

if [ -f $HOME/.Xresources ]
then
	/usr/bin/env xrdb $HOME/.Xresources
fi

#source $HOME/.bashrc

export XKB_DEFAULT_OPTIONS="caps:escape"
setxkbmap -option caps:escape

awesome
