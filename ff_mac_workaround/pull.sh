#!/bin/bash


EMACS_CLIENT=emacsclient
TMP_FILE=~/.org-fireforg-mac.tmp

while [ 0==0 ]
do

# Read the first line
COM=$(head -n 1 ${TMP_FILE})

if [ "${COM}" != "" ]; then
# echo "Calling ${EMACS_CLIENT} with argument: ${COM}"
# read 
${EMACS_CLIENT} "${COM}"

# Remove the first line
perl -pi -e '$_ = "" if ($. == 1);' ${TMP_FILE}

else
 sleep 1s
fi

done
