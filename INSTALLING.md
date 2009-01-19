# Installing

## Ruby

To run CouchApp's Ruby uploader script, you'll need Ruby 1.8.6 or better, hopefully your package management system supports Ruby and (like Debian) also a Ruby Development package, which you'll need to build the native JSON gem etc.

You'll also [need to install RubyGems.]() I've never had an easy time doing this part of a dev-env bootstrap, so let me wish you luck. There's a nacent Python implementation, so if the Ruby depedencies aren't your style, watch [Jan's CouchApp repo](http://github.com/janl/couchapp), where he's at work on the Python scripts.

### Easy Way

It's recommended that you try this first, if it doesn't work, the next option is less fancy, but maybe more reliable.

    # get the latest rubygems (optional)
    sudo gem update --system
    # install CouchApp
    sudo gem install couchapp

Once this is done, run `couchapp generate relax` and generally pick up with the README.

### Hard Way

Once you've got RubyGems, (please excuse the `sudo`, it's to install the `couchapp` script in your path) run:

    git clone git@github.com:jchris/couchapp.git && cd couchapp
    gem build couchapp.gemspec
    sudo gem install couchapp-*.gem

You might have to satisfy some dependencies, especially [CouchRest](http://github.com/jchris/couchrest). Plan B here is `gem install couchrest`, which you might have to sudo, depending on your system.

Once this is done, run `couchapp generate relax` and generally pick up with the README.

## Python

The Python code is currently in rapid development. See the `python` directory. 

We'll just do shared integration tests, at the command-line level, so each language can set about porting it's peers unit tests if there are implementation questions.


## Questions and Comments

If you can improve this documentation, please send pull requests using Github or hit the [CouchApp mailing list](http://groups.google.com/group/couchapp). 

We want the install experience to go smoothly for **everyone**, so please send feedback and let us help you make this documentation cover troubleshooting problems.
