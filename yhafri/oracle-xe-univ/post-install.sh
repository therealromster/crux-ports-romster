#!/bin/sh

groupadd dba
useradd -G dba oracle

/etc/rc.d/oracle-xe configure
