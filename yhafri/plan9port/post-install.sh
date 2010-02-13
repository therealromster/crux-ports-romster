#!/bin/sh

(

PLAN9=/usr/local/plan9 export PLAN9
PATH=/bin:/usr/bin:$PLAN9/bin:$PATH export PATH

cd $PLAN9

./INSTALL
rm -rf CHANGES LICENSE NOTES TODO install.log CVS/

)
