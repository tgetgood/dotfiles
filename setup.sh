# Simple script to copy dotfiles to home directory.
# If the files already exist, back them up and delete them.
# Does not back up symlinks.
# overwirtes .file~ if it exists.

# last edited by on 14/12/2011

HOME=~thomas

SCRIPT_PATH=$PWD
# This is not very good in general, just so far. 
shopt -s dotglob

for f in *
do
  # There should be only two special cases...
  if [ "$f" == `basename $0` -o "$f" == ".git" ]
  then
    continue
  fi

  # if file exists and is not a symlink, back it up
  if [ -h "$HOME/$f" ]
  then
    rm "$HOME/$f"
  elif [ -e "$HOME/$f" ]
  then
    mv "$HOME/$f" "$HOME/$f~"
  fi

  ln -s "$SCRIPT_PATH/$f" "$HOME/$f"

done

