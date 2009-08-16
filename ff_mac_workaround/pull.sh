#!/bin/bash


EMACS_CLIENT=emacsclient
TMP_FILE=~/.org-fireforg-mac.tmp

while [ 0==0 ]
do

COM=$(cat ${TMP_FILE})

if [ "${COM}" != "" ]; then
# echo "Calling ${EMACS_CLIENT} with argument: ${COM}"
# read 
${EMACS_CLIENT} "${COM}"
echo "" > ${TMP_FILE}
fi

sleep 1s
done