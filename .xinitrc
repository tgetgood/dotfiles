#!/bin/bash

if [ -f $HOME/.Xmodmap ]
then
	/usr/bin/env xmodmap $HOME/.Xmodmap
fi

if [ -f $HOME/.Xresources ]
then
		/usr/bin/env xrdb $HOME/.Xresources
fi

#disable touchpad while typing
syndaemon -R -K -i 1 -d &

start-pulseaudio-x11

source ~/.bashrc

ssh-agent awesome
