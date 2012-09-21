# Simple script to copy dotfiles to home directory.
# If the files already exist, back them up and delete them.
# Does not back up symlinks.
# overwrites .file~ if it exists.

# last edited 14/12/2011

HOME=~thomas

SCRIPT_PATH=$( readlink -e $(dirname $0))

shopt -s dotglob

for file in $SCRIPT_PATH/*
do
  f=`basename $file`

  # What to do if the special cases take over?
  if [ "$f" == `basename $0` -o "$f" == ".git" -o "$f" == "bin" ]
  then
    continue
  fi

  # If file exists and is not a symlink, back it up, otherwise replace it.
  if [ -h "$HOME/$f" ]
  then
    rm "$HOME/$f"
  elif [ -e "$HOME/$f" ]
  then
    mv "$HOME/$f" "$HOME/$f~"
  fi

  ln -s "$SCRIPT_PATH/$f" "$HOME/$f"

done

# Symlink all scripts (clobber ~/bin is a bad idea).

test -d "$HOME/bin" || mkdir "$HOME/bin"

for file in $SCRIPT_PATH/bin/*
do
	f=`basename $file`
	ln -s "$SCRIPT_PATH/bin/$f" "$HOME/bin/$f"
done

