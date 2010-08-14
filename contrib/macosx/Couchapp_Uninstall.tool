#!/bin/sh
#
# Couchapp Uninstaller Tool
# 
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information. 
#

#
# Display a simple welcome message first.
#
echo ""
echo "Welcome to the Couchapp uninstaller script."
echo ""

my_directories=""
my_files=""

test -d /Library/Python/2.6/site-packages/couchapp/ && my_directories="$my_directories /Library/Python/2.6/site-packages/couchapp/"

test -d /Library/Python/2.6/site-packages/restkit && my_directories="$my_directories /Library/Python/2.6/site-packages/restkit/"

test -d /Library/Python/2.6/site-packages/Couchapp-0.7.0-py2.6.egg-info/ && my_directories="$my_directories /Library/Python/2.6/site-packages/Couchapp-0.7.0-py2.6.egg-info/"

test -d /Library/Python/2.6/site-packages/restkit-2.1.4-py2.6.egg-info && my_directories="$my_directories /Library/Python/2.6/site-packages/restkit-2.1.4-py2.6.egg-info/"

test -f /usr/bin/couchapp && my_files="$my_files /usr/bin/couchapp"
test -f /usr/bin/restcli && my_files="$my_files /usr/bin/restcli"

#
# Display the files and directories that will be removed
# and get the user's consent before continuing.
#
if test -n "$my_files"  -o  -n "$my_directories"; then
    echo "The following files and directories (bundles) will be removed:"
    for file in $my_files;       do echo "    $file"; done
    for dir  in $my_directories; do echo "    $dir"; done
fi


if test "$my_default_prompt" != "Yes"; then
    echo "Do you wish to uninstall Couchapp (Yes/No)?"
    read my_answer
    if test "$my_answer" != "Yes"  -a  "$my_answer" != "YES"  -a  "$my_answer" != "yes"; then
        echo "Aborting uninstall. (answer: '$my_answer')".
        exit 2;
    fi
    echo ""
fi

#
# Display the sudo usage instructions and execute the command.
#
echo "The uninstallation processes requires administrative privileges"
echo "because some of the installed files cannot be removed by a normal"
echo "user. You may be prompted for your password now..."
echo ""

if test -n "$my_files"  -o  -n "$my_directories"; then
    /usr/bin/sudo -p "Please enter %u's password:" /bin/rm -Rf $my_files $my_directories
    my_rc=$?
    if test "$my_rc" -ne 0; then
        echo "An error occured durning 'sudo rm', there should be a message above. (rc=$my_rc)"
        test -x /usr/bin/sudo || echo "warning: Cannot find /usr/bin/sudo or it's not an executable."
        test -x /bin/rm       || echo "warning: Cannot find /bin/rm or it's not an executable"
        echo ""
        echo "The uninstall failed. Please retry."
        exit 1;
    fi
fi

if test "$my_rc" -eq 0; then
    echo "Successfully uninstalled Couchapp."
else
    echo "Failed to uninstall Couchapp."
fi
echo "Done."
exit 0;
