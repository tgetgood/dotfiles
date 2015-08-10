#!/bin/bash

# Simple script to copy dotfiles to home directory.
# If the files already exist, back them up and delete them.
# Does not back up symlinks.
# overwrites .file~ if it exists.

# last edited 14/12/2011

HOME=~

SCRIPT_PATH=$( readlink -e $(dirname $0))

# shopt -s dotglob

replace_link() {
  # If file exists and is not a symlink, back it up, otherwise replace it.
	FILE=$1
	FROM=$2
	TO=$3

  if [ -h "$TO/$FILE" ]
  then
    rm "$TO/$FILE"
  elif [ -e "$TO/$FILE" ]
  then
    mv "$TO/$FILE" "$TO/$FILE~"
  fi

  ln -s "$FROM/$FILE" "$TO/$FILE"
}


# N.B.: any file name consisting soley of dots will be ignored
for file in `ls -a $SCRIPT_PATH | grep "^\." | grep -v '^\.*$'`
do
  f=`basename $file`

  # What to do if the special cases take over?
  if [ "$f" == ".git" ]
  then
    continue
  fi

	replace_link "$f" "$SCRIPT_PATH" "$HOME"

done

# Populate ~/bin with custom scripts.

test -d "$HOME/bin" || mkdir "$HOME/bin"

for file in $SCRIPT_PATH/bin/*
do
	replace_link `basename $file` "$SCRIPT_PATH/bin" "$HOME/bin"
done

# Create vim tempfile dir

test -d ~/.vim-tmp || mkdir ~/.vim-tmp

