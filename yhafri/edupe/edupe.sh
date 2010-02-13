#!/bin/sh

set -e

if [ $# == 0 ]; then
        echo "Usage : $0 dirname"
        exit 1
fi

if [ ! -d $1 ]; then
	echo "'$1' isn't a valid directory"
	exit 1
fi

name=edupe
pa=/usr/lib/$name/ebin

erl -pa $pa -noshell -run $name start $1 -s erlang halt
exit 0
