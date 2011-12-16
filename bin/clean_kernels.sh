#!/bin/bash

# This script removes all kernels older than $1. 
#
# The newest kernel is never removed and if no arg is given, all kernels except the newest
#+ are removed.
#
# This script can be dangerous. Be sure you know what you're doing before you run it.
#
#TODO: Insert argument sanity checking. Right now very strange things happen if you give 
#+ odd numeric arguments (non numeric args will never delete anything).


# There's got to be a better way to do this. I just haven't seen it yet.
older () {

    [ `cut -d'.' -f1 <<< $1` -le `cut -d'.' -f1 <<< $2` ] &&\
    [ `cut -d'.' -f2 <<< $1` -le `cut -d'.' -f2 <<< $2` ] &&\
    [ `cut -d'-' -f1 <<< $(cut -d'.' -f3 <<< $1)` -le `cut -d'-' -f1 <<< $(cut -d'.' -f3 <<< $2)` ] &&\
    [ `cut -d'-' -f2 <<< $(cut -d'.' -f3 <<< $1)` -lt `cut -d'-' -f2 <<< $(cut -d'.' -f3 <<< $2)` ] 
# Note that the last comparison is strict since we never want to delete the newest kernel.

    return $?
}

E_NOT_ROOT=65

if [ "$UID" -ne 0 ]
then
    echo You must run this script as root.
    exit $E_NOT_ROOT
fi

NEWEST=$(uname -r | cut -d'-' -f1,2)

OLDEST=${1:-"$NEWEST"}
    
if older "$NEWEST" "$OLDEST"  
then
    OLDEST="$NEWEST"
fi

KERNELS=$(ls /boot | grep vmlinuz | cut -d'-' -f2,3)

PACKAGES=''

for KERNEL in $KERNELS
do
    if older "$KERNEL" "$OLDEST"
    then
	PACKAGES=$(echo -e $PACKAGES $(dpkg -l | grep ^ii | grep "$KERNEL" | awk '{ printf $2; printf " " } '))
    fi
done

echo "The following will be permenently removed:"
echo "$PACKAGES"
read -p "Do you want to continue? (y/n): " GO_ON

if [ "$GO_ON" == "y" ]
then
    # The following line should be commented to endure that no one simply runs the script without at 
    #+ least looking at it.

    apt-get -V remove $PACKAGES 

    exit $?
else
    echo "Cancelled. Exiting..."
    exit 0
fi


