#!/bin/sh

printf "Which group are overnet users part of? "
read GROUP
chgrp -R $GROUP /usr/share/overnetclc
chgrp $GROUP /usr/bin/overnetclc
chmod -R 660 /usr/share/overnetclc
chmod 770 /usr/share/overnetclc
chmod 750 /usr/bin/overnetclc