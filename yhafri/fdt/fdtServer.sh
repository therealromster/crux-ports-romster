#!/bin/sh

set -e

export FDT=/usr/lib/fdt
export PATH=$FDT:$PATH

java -cp $FDT -jar $FDT/fdt.jar
