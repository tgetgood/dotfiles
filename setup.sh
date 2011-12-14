# Simple script to copy dotfiles to home directory.
# If the files already exist, back them up and delete them.
# Does not back up symlinks.
# overwirtes .file~ if it exists.

# last edited by on 14/12/2011

HOME=~thomas

SCRIPT_PATH=$PWD
# This is not very good in general, just so far. 
FILES=`ls -a | grep -v 'setup\.sh|\.git'`

for f in $FILES
do
  # if file exists and is not a symlink, back it up
  if [ -e $HOME/"$f" ]
  then
    [ -h "$HOME/$f" ] && rm "$HOME/$f" || mv "$HOME/$f" "$HOME/$f~"
  fi

  ln -s "$SCRIPT_PATH/$f" "$HOME/$f"

done

