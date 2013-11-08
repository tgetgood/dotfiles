if [ -f $HOME/.Xmodmap ]; then
	/usr/bin/xmodmap $HOME/.Xmodmap
fi

syndaemon -K -i 1 -d &
exec awesome
