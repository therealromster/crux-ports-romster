#!/bin/sh

function usage {
    echo
    echo "Info: View multiple 'ps' documents as one"
    echo
    echo "Usage: $0 ps_dir output_file"
    echo
    echo "      ps_dir  : directory containing ps file"
    echo "      out_file: merged view of the ps files"
    echo
    echo "Example: $0 /somewhere out.ps"
    echo
    echo "NOTE: after conversion, you may need to save the"
    echo "resulting 'ps' as 'pdf' for example:"
    echo "$ ps2pdf out.ps"
    echo
    exit 1
}


if [ $# != "2" ]; then
    usage
fi

if [ ! -d $1 ]; then
    echo
    echo "ERROR:"
    echo "      $1 directory must exists."
    usage
fi

if [ -f $2 ]; then
    echo
    echo "PROBLEM:"
    echo "      $2 exists. Please, delete it or put it somewhere before."
    usage
fi


cat <<"EOF" > $2
%!PS
% Written by Helge Blischke, see
% http://groups.google.com/groups?ic=1&selm=3964A684.49D%40srz-berlin.de
%
% The following 2 procs encapsulate the jobs to be processed
% much as is done with EPS images:
/_begin_job_
{
        /tweak_save save def
        /tweak_dc countdictstack def
        /tweak_oc count 1 sub def
        userdict begin
}bind def

/_end_job_
{
        count tweak_oc sub{pop}repeat
        countdictstack tweak_dc sub{end}repeat
        tweak_save restore
}bind def

EOF

for i in $1/*.ps;
do
cat <<EOF >> $2
% Now, add your jobs like this:
_begin_job_
($i)run
_end_job_
EOF
done

echo "NOTE: don't forget to convert the $2 file."
echo "You may need to save it as a 'pdf' like this:"
echo "$ ps2pdf $2"

