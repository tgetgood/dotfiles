# Simple script to copy dotfiles to home directory.
# If the files already exist, back them up and delete them.
# Does not back up symlinks.
# overwirtes .file~ if it exists.

# Originally copied from Alex's dotfiles, but not much remains
# last edited by Thomas on 12/08/2011

HOME=/home/thomas

SCRIPT_PATH=$(readlink -f $(dirname "${VIRTUAL_ENV}"))
# This is not very good in general, just so far. 
FILES=`ls | grep -v '\.sh$'`

for f in $FILES
do
  # if file exists and is not a symlink, back it up
  if [ -e $HOME/."$f" ]
  then
    [ -h $HOME/."$f" ] && rm $HOME/."$f" || mv $HOME/."$f" $HOME/."$f"'~'
  fi

  ln -s $SCRIPT_PATH/"$f" $HOME/."$f"

done

