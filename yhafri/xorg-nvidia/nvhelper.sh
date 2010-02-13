#!/bin/bash

# $Id: nvhelper.sh,v 1.1.1.1 2004/08/17 20:01:42 crux Exp $

echo ""
echo "nvhelper.sh - facilitate renaming/restoring of libGL.so.1.2"
echo ""

case "$1" in
	--install)
		if [ ! -f /usr/X11R6/lib/libGL.so.1.2 ]
		then
			echo "/usr/X11R6/lib/libGL.so.1.2 doesn't exist! Exiting..."
			exit 1
		fi
		(cd /usr/X11R6/lib
		 mv libGL.so.1.2 libGL_so_1_2
		 mv libGL.a libGL_a
		 cd modules/extensions
		 mv libGLcore.a libGLcore.a.orig
		 mv libglx.a libglx.a.orig
		 /sbin/ldconfig)
		;;
	--restore)
		if [ ! -f /usr/X11R6/lib/libGL_so_1_2 ]
		then
			echo "/usr/X11R6/lib/libGL_so_1_2 doesn't exist! Exiting..."
			exit 1
		fi
		(cd /usr/X11R6/lib
		 mv libGL_so_1_2 libGL.so.1.2
		 mv libGL_a libGL.a
		 cd modules/extensions
		 mv libGLcore.a.orig libGLcore.a
		 mv libglx.a.orig libglx.a
		 /sbin/ldconfig)
		;;
	*)
		echo "Unrecognized option, please use --install or --restore."
		exit 0
		;;
esac

echo "done."
