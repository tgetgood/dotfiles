#!/usr/bin/env bash

# Copies all files of a given typefrom COPYDIR tree to BASEDIR preserving relative path if they are not in the tree of BASEDIR
# Works pretty well. Still some weird bugs with certain names not being recognised as duplicates and caertain directories not being made properly. 

# Error codes

E_BADARGS=66
E_BADOPT=67

# Build a list of all files of type $2 (as listed by file command)  in the directory (recursively)
# Usage: "lister DIR filetype temp_file"

lister () {

	for x in "$1"/*
	do
		if [ -d "$x" ]
		then 
			lister "$x" "$2" "$3"
		elif file -b "$x" | tr 'A-Z' 'a-z' | grep -q "$2"
		then
			echo `basename "$x"` >> "$3"
		fi
	done
}

# Copy all files of type $2 from $COPYDIR to $BASEDIR/whatever if they are not in the file list.
# Usage: "selectcopy copy_from_dir copy_to_basedir filetype tempfile"

selectcopy () { 

	for x in "$1"/*
	do
		if [ -d "$x" ]
		then
			selectcopy "$x" "$2" "$3" "$4"
		else
			if file -b "$x" | tr 'A-Z' 'a-z' | \
				grep -q "$3" && ! grep -q "$(basename "$x")" < "$4"
			then
				DIR=$(dirname "$2/$x" | tr 'A-Z' 'a-z')
				if [ ! -d "$DIR" ]
				then
					mkdir  "$DIR"
				fi
				cp "$x" "$DIR/`basename "$x"`"
				echo "new file: "$DIR" /`basename "$x"`" # Test mode
			fi
		fi
	done
}


while getopts 'mdat:' OPT
do
	case $OPT in
	t)
		TYPE="$OPTARG"
	;;
	m)
		TYPE="audio"
	;;
	d)	
		TYPE="document"
	;;	
	a)
		TYPE=''
	;;
	?)
		echo "Unrecognised argument $OPT, ignoring." >&2
	esac
done	

shift $(($OPTIND - 1))

if [[ "$#" -eq "2" ]] && [ -d "$1" ] && [ -d "$2" ] 
then
	BASEDIR="$1"
	COPYDIR="$2"
else
	echo "usage: `basename $0` [OPTIONS] basedir copyfromdir" >&2
		
	exit E_BADARGS
fi

FILE="`mktemp`"

cd "$BASEDIR"
BASEDIR=`pwd`

lister . "$TYPE" "$FILE"

cd - 
cd "$COPYDIR"

selectcopy . "$BASEDIR" "$TYPE" "$FILE"

rm "$FILE"
