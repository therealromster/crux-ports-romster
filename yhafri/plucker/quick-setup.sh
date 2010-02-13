#!/bin/bash
echo "This software comes with no guarantee.  Dataloss is likely"
echo -n "Would you like to backup your palm now?(yes/no) "
read backup
if [ `echo $backup` == "yes" ]; then
    echo "Beginning Backup..."
    mkdir -m 700 ~/palm_bak
    pilot-xfer -b ~/palm_bak
    echo "The backup is in $HOME/palm_bak"
if [ `echo $backup` == "no" ]; then
    echo "Continuing installation, without a backup"
fi
  else
      echo "This is picky software!  Type more carefully next time."
      exit
fi

return=`pwd`
cd /usr/share/plucker/palm

### Old NLS Support ###
# echo -e "These are the available languages\n---------------------------------"
# ls viewer_*.prc
# echo -ne "\n\nPlease choose the viewer for your language: "
# read lang

lang=en
install=`ls *$lang*`
echo "\nInstalling Configuration..."
mkdir ~/.plucker
cp -i /usr/share/plucker/config/{exclusionlist.txt,home.html} ~/.plucker
echo "\nInstalling Palm Application..."
pilot-xfer -i /usr/share/plucker/palm/{SysZLib.prc,$install,PluckerUserGuide.pdb}
cd $return
