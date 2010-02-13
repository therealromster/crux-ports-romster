#!/bin/sh

ERLGUTEN=/usr/lib/erlang/lib/erlguten-svn
EBIN=$ERLGUTEN/ebin
FILEPATH=`pwd`
FILENAME=$FILEPATH/$1

if [ ! -e "$FILENAME" ]; then
  (cd $EBIN; erl -pa $ERLGUTEN/ebin -pa $ERLGUTEN/priv/fonts/bin -pa `pwd` -s erlguten batch $1 -s erlang halt)	
else
  (cd $EBIN; erl -pa $ERLGUTEN/ebin -pa $ERLGUTEN/priv/fonts/bin -pa `pwd` -s erlguten batch $FILENAME -s erlang halt)
fi
