#!/bin/sh

echo "downloading snowball..."
wget http://snowball.tartarus.org/texts/snowball.tgz

tar xzf snowball.tgz

echo "compiling snowball..."
gcc -Os -o Snowball p/*.c
cp q/driver-porter.c ./driver-porter.c.template

PKG=$1
website="http://snowball.tartarus.org/"

for i in porter english french spanish portuguese italian \
    german dutch swedish norwegian danish russian finnish;
do
  echo "converting the template driver to $i"
  sed "s|porter|$i|" driver-porter.c.template > q/driver-porter.c

  echo "downloading $i template language (.sbl)"
  wget "${website}${i}/stem.sbl"

  echo "creating the $i.h and $i.c stemmer source code"
  ./Snowball stem.sbl -o q/stem -ep "${i}_"
  rm -rf stem.sbl

  echo "compiling the $i stemmer (${i}_stemmer)"
  gcc -Os -o ${i}_stemmer q/api.c q/utilities.c q/driver-porter.c q/stem.c
  strip ${i}_stemmer

  install -D -m 755 ${i}_stemmer $PKG/usr/bin/${i}_stemmer


done


