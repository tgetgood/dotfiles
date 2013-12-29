#!/bin/bash

if [ -f $HOME/.Xmodmap ]; then
	/usr/bin/xmodmap $HOME/.Xmodmap
fi

#disable touchpad while typing
syndaemon -R -K -i 1 -d &

start-pulseaudio-x11


ssh-agent awesome
