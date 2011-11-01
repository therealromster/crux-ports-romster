#!/bin/sh
cd /usr/lib/lbz3d
export LD_LIBRARY_PATH=./lib # for libfmod-3.72.so
#export PATH="$PATH:/usr/lib/kde4/bin"
./bin/lbz $@
xset r on
