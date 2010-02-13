#!/bin/sh

printf "Which group are edonkey users part of? "
read GROUP
chgrp -R $GROUP /usr/share/edonkeyclc
chgrp $GROUP /usr/bin/edonkeyclc
chmod -R 660 /usr/share/edonkeyclc
chmod 770 /usr/share/edonkeyclc
chmod 750 /usr/bin/edonkeyclc