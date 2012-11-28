PREFIX?= /usr/local
ERICA_TAG=	$(shell git describe --tags --always)
REVISION?=	$(shell echo $(ERICA_TAG) | sed -e 's/^$(REPO)-//')
PKG_VERSION?=	$(shell echo $(REVISION) | tr - .)
WITHOUT_CURL?=1

.PHONY: deps doc

all: deps compile

install: all
	@install -m 0755 -c erica $(PREFIX)/bin
	
compile:
	@./rebar compile
	@escript bootstrap

deps:
	@./rebar get-deps

doc:
	@./rebar doc


clean:
	@./rebar clean
	@rm -f erica

distclean: clean
	@./rebar delete-deps

##
## release tarballs
##
archive = git archive --format=tar --prefix=$(1)/ HEAD | (cd $(2) && tar xf -)

buildtar = mkdir distdir && \
		 git clone . distdir/erica-clone && \
		 cd distdir/erica-clone && \
		 git checkout $(ERICA_TAG) && \
		 $(call archive,$(ERICA_TAG),..) && \
		 mkdir ../$(ERICA_TAG)/deps && \
		 make deps; \
		 for dep in deps/*; do \
                     cd $${dep} && \
                     $(call archive,$${dep},../../../$(ERICA_TAG)) && \
                     mkdir -p ../../../$(ERICA_TAG)/$${dep}/priv && \
                     git rev-list --max-count=1 HEAD > ../../../$(ERICA_TAG)/$${dep}/priv/git.vsn && \
                     cd ../..; done

distdir: rebar
	$(if $(ERICA_TAG), $(call buildtar), $(error "You can't generate a release tarball from a non-tagged revision. Run 'git checkout <tag>', then 'make dist'"))

cleandist:
	@rm -rf distdir
	@rm -rf *.gz

dist $(ERICA_TAG).tar.gz: cleandist distdir
	cd distdir; \
	tar czf ../$(ERICA_TAG).tar.gz $(ERICA_TAG)
	openssl sha1 $(ERICA_TAG).tar.gz > $(ERICA_TAG).tar.gz.info
	openssl md5 $(ERICA_TAG).tar.gz >> $(ERICA_TAG).tar.gz.info
