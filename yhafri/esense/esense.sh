#!/bin/sh

scriptdir=`dirname $0`

erl -emu-args -noshell -pa /usr/lib/yaws/ebin -pa $scriptdir/compat -pa $scriptdir \
	-run esense start -run erlang halt -extra "$@"

