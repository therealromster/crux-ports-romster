#!/bin/sh

set -e

export FDT=/usr/lib/fdt
export PATH=$FDT:$PATH

if [ $# -lt 1 ]; then
	echo "***************************************************"
	echo "************** Parameters needed ******************"
	echo "***************************************************"
	java -cp $FDT -jar $FDT/fdt.jar -h
	exit 0
fi
found=false
for param in $*; do
	if [ "$param" == "-c" ]; then
		found=true
		break
	fi
done
if [ "$found" == "false" ]; then
	echo "***************************************************"
	echo "******* Please specify the server address *********"
	echo "***************************************************"
	java -cp $FDT -jar $FDT/fdt.jar -h	
	exit 0
fi
java -cp $FDT -jar $FDT/fdt.jar $*
