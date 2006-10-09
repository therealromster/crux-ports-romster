#!/bin/sh

export LD_LIBRARY_PATH=/usr/share/google-earth:$LD_LIBRARY_PATH
export GOOGLEEARTH_DATA_PATH=/usr/share/google-earth

cd $GOOGLEEARTH_DATA_PATH
exec ./googleearth-bin $*

# End of File
