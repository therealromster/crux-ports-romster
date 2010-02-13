#!/bin/sh

#export ERL_FULLSWEEP_AFTER=0
export ERL_MAX_PORTS=32000

erl -pa /var/lib/ejabberd/ebin \
    -sname ejabberd \
    -s ejabberd \
    -ejabberd config \"/etc/ejabberd/ejabberd.cfg\" \
    log_path \"/var/log/ejabberd/ejabberd.log\" \
    -sasl sasl_error_logger \{file,\"/var/log/ejabberd/sasl.log\"\} \
    -mnesia dir \"/var/lib/ejabberd/spool\"
