#!/bin/sh

EPSXEDIR=/usr/lib/epsxe

# create a local copy of the epsxe dir and subdirs
mkdir -p ~/.epsxe/{bios,cfg,cheats,memcards,patches,plugins,snap,sstates}

cd ~/.epsxe

# symlink epsxe binary, keycodes, etc.
for i in `find $EPSXEDIR -maxdepth 1 -type f -printf '%f '`
do
	[ ! -e $i ] && ln -s $EPSXEDIR/$i $i
done

# symlink config binaries
for i in `find $EPSXEDIR/cfg -maxdepth 1 -iname 'cfg*' -type f -printf '%f '`
do
	if [ ! -e cfg/$i ]
	then
		ln -s $EPSXEDIR/cfg/$i cfg/$i
	fi
done

# copy config files (if they don't already exist locally)
for i in `find $EPSXEDIR/cfg -maxdepth 1 -iname '*.cfg' -type f -printf '%f '`
do
	if [ ! -e cfg/$i ]
	then
		echo "Copying default config: $i"
		cp $EPSXEDIR/cfg/$i cfg/
	fi
done

# symlink cheats (if any)
for i in `find $EPSXEDIR/cheats -maxdepth 1 -type f -printf '%f '`
do
	if [ ! -e cheats/$i ]
	then
		ln -s $EPSXEDIR/cheats/$i cheats/$i
	fi
done

# symlink patches (if any)
for i in `find $EPSXEDIR/patches -maxdepth 1 -type f -printf '%f '`
do
	if [ ! -e patches/$i ]
	then
		ln -s $EPSXEDIR/patches/$i patches/$i
	fi
done

# symlink plugins
for i in `find $EPSXEDIR/plugins -maxdepth 1 -type f -printf '%f '`
do
	if [ ! -e plugins/$i ]
	then
		ln -s $EPSXEDIR/plugins/$i plugins/$i
	fi
done

# symlink bios images (if there are any)
for i in `find $EPSXEDIR/bios -maxdepth 1 -type f -printf '%f '`
do
	if [ ! -e bios/$i ]
	then
		ln -s $EPSXEDIR/bios/$i bios/$i
	fi
done

# check for a bios image
if [ -z "`ls bios`" ]
then
	echo "*************************************************************"
	echo "* You need to install a PSX bios image into ~/.epsxe/bios/! *"
	echo "* Without it ePSXe won't work properly.                     *"
	echo "*************************************************************"
fi

# run the program
exec ./epsxe "$@"
