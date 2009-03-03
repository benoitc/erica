#!/bin/sh

GIT=`which git` 
tempfile=`mktemp /tmp/couchappXXXXXXX`

rm $tempfile


echo $2
# get vendor files
$GIT clone --depth=1 $2 $tempfile

# delete existing files
if [ $1 = "update" ]; then
    if [ -r $3 ]; then
        rm -rvf $3
    fi

    if [ -r $tempfile/vendor ]; then
        cp -vr $tempfile/vendor/* $4
    fi
else
    if [ -r $tempfile/vendor ]; then
        cp -vr $tempfile/vendor/* $3
    fi
fi


