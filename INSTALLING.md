# Installing

## Python

If `sudo easy_install couchapp` doesn't work for you, you can install from source.

    git clone git://github.com/couchapp/couchapp.git
    cd couchapp
    sudo python setup.py install

You may need to satisfy some dependencies. We hope this will happen automatically but... you never know.


## Simplejson Dependency

If you are on stock Mac OS X 10.5 Leopard, your version of setuptools is out of date. If your installation of couchapp fails with "error: Could not find required distribution simplejson", try running

    sudo easy_install -U couchapp

## Questions and Comments

If you can improve this documentation, please send pull requests using Github or hit the [CouchApp mailing list](http://groups.google.com/group/couchapp). You could also join us on irc channel #couchapp at irc.freenode.net.

We want the install experience to go smoothly for **everyone**, so please send feedback and let us help you make this documentation cover troubleshooting problems.
