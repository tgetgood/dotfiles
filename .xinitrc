#!/usr/bin/env bash

if [ -f $HOME/.Xresources ]
then
	/usr/bin/env xrdb $HOME/.Xresources
fi

source $HOME/.bashrc

dbus-launch awesome
