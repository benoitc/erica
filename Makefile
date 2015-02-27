BASE_DIR = $(shell pwd)
PREFIX?= /usr/local/bin
ERICA_TAG=	$(shell git describe --tags --always)
REVISION?=	$(shell echo $(ERICA_TAG) | sed -e 's/^$(REPO)-//')
PKG_VERSION?=	$(shell echo $(REVISION) | tr - .)
PKG_NAME=erica-$(ERICA_TAG)
WITHOUT_CURL?=1

.PHONY: deps doc

all: deps compile

install: all
	@install -m 0755 -c erica $(PREFIX)/erica

compile:
	@./rebar compile
	@escript bootstrap

deps:
	@./rebar get-deps

doc:
	@./rebar doc

clean:
	@./rebar clean
	@rm -f erica erica.cmd

distclean: clean
	@./rebar delete-deps

upgrade: distclean all

update: clean
	@./rebar update-deps
	@./rebar get-deps
	@./rebar compile
	@escript bootstrap

##
## release tarballs
##
archive = git archive --format=tar --prefix=$(1)/ HEAD | (cd $(2) && tar xf -)

buildtar = mkdir distdir && \
		 git clone . distdir/erica-clone && \
		 cd distdir/erica-clone && \
		 git checkout $(ERICA_TAG) && \
		 $(call archive,$(PKG_NAME),..) && \
		 mkdir ../$(PKG_NAME)/deps && \
		 make deps; \
		 for dep in deps/*; do \
                     cd $${dep} && \
                     $(call archive,$${dep},../../../$(PKG_NAME)) && \
                     mkdir -p ../../../$(PKG_NAME)/$${dep}/priv && \
                     git rev-list --max-count=1 HEAD > ../../../$(PKG_NAME)/$${dep}/priv/git.vsn && \
                     cd ../..; done && \
		cd $(BASE_DIR)/distdir/$(PKG_NAME) && make

distdir: rebar
	$(if $(PKG_NAME), $(call buildtar), $(error "You can't generate a release tarball from a non-tagged revision. Run 'git checkout <tag>', then 'make dist'"))

cleandist:
	@rm -rf distdir
	@rm -rf *.gz

dist $(ERICA_TAG).tar.gz: cleandist distdir
	cd distdir; \
		tar czf ../$(PKG_NAME).tar.gz $(PKG_NAME)
	openssl sha1 $(PKG_NAME).tar.gz > $(PKG_NAME).tar.gz.info
	openssl md5 $(PKG_NAME).tar.gz >> $(PKG_NAME).tar.gz.info
