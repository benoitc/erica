#!/bin/sh

GIT=`which git` 
tempfile=`mktemp /tmp/couchappXXXXXXX`

rm $tempfile

if [ "$1" = "update" ]; then
    $GIT clone $2 $tempfile --depth=1
fi


