#!/bin/sh

GIT=`which git` 
tempfile=`mktemp /tmp/couchappXXXXXXX`


rm $tempfile

# get vendor files
$GIT clone --depth=1 $2 $tempfile


# delete existing files
if [ $1 = "update" ]; then
    if [ -r $3 ]; then
        rm -rvf $3
    fi
 
    if [ -r $tempfile/vendor ]; then
        for f in $(find $tempfile/vendor -type d -depth 1); do
            if [ $f != "$tempfile/vendor" ]; then
                cp -vr $f $4
            fi
        done
        
    fi
else
    if [ -r $tempfile/vendor ]; then
        ls $tempfile/vendor
        for f in $(find $tempfile/vendor -type d -depth 1); do
            if [ $f != "$tempfile/vendor" ]; then
                echo "$2" > $f/.new
                cp -rv $f $3
                
            fi
        done
    fi
fi