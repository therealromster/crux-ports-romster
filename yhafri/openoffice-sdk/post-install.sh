#!/bin/sh

CURDIR=/usr/lib/openoffice-sdk
(cd $CURDIR; perl ./configure.pl $CURDIR; chmod +x *.sh *.csh)
