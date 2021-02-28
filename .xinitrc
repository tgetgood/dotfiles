#!/usr/bin/env bash

if [ -f $HOME/.Xresources ]
then
	/usr/bin/env xrdb $HOME/.Xresources
fi

source $HOME/.bashrc

setxkbmap -option caps:escape

dbus-launch awesome
