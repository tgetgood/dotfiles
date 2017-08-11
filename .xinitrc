#!/bin/bash

if [ -f $HOME/.Xmodmap ]
then
	/usr/bin/env xmodmap $HOME/.Xmodmap
fi

if [ -f $HOME/.Xresources ]
then
		/usr/bin/env xrdb $HOME/.Xresources
fi

start-pulseaudio-x11

source $HOME/.bashrc

awesome 
