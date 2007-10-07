#!/bin/sh

CUBE_DIR=/usr/share/cube
SYSTEM_NAME=$(uname -s)
SYSTEM_PREFIX=linux_
MACHINE_NAME=$(uname -m)
MACHINE_PREFIX=

cd ${CUBE_DIR}
exec ${CUBE_DIR}/bin_unix/${MACHINE_PREFIX}${SYSTEM_PREFIX}client $*
