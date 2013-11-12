#!/bin/bash

if [ -f $HOME/.Xmodmap ]; then
	/usr/bin/xmodmap $HOME/.Xmodmap
fi

#disable touchpad while typing
syndaemon -R -K -i 1 -d &

pulseaudio --start

# Start Gnome keyring
eval $(/usr/bin/gnome-keyring-daemon --start --daemonize)
export GNOME_KEYRING_CONTROL GNOME_KEYRING_PID GPG_AGENT_INFO SSH_AUTH_SOCK

exec awesome
