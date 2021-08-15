REBAR     = rebar3
env  		 ?= test
APP_ROOT ?= /tmp/$(env)
PROJECT   = $(shell sed -n '/application/{s/^.*{application, *\([^,]\+\),.*$$/\1/p; q}' $(wildcard src/*.app.src))
VSN       = $(shell sed -n '/{payments, *"/{s/^.*payments, *"\([^\"]\+\)".*$$/\1/p}' rebar.config)

all: deps compile

compile dialyzer:
	@# The TERM=dumb removes bold colors, or else vim doesn't figure out the error location properly
	@#TERM=dumb APP_ROOT=$(ROOT) $(REBAR) $@
	@APP_ROOT=$(ROOT) $(REBAR) $@

clean:
	@$(REBAR) $@

dialyze: dialyzer

deps:
	@$(REBAR) get-deps

test eunit: dialyze
	@$(REBAR) eunit

docs:
	@$(REBAR) edoc

vsn:
	@echo "Version: $(VSN)"

set-version:
	@[ -z $(version) ] && echo "Missing version=X.Y.Z!" && exit 1 || true
	@sed -i 's/{$(PROJECT),\( \+\)"[[:digit:]]\+\(\.[[:digit:]]\+\)\{1,\}"}/{$(PROJECT),\1"$(version)"}/' rebar.config
	@sed -i 's/{vsn,\( \+\)"[[:digit:]]\+\(\.[[:digit:]]\+\)\{1,\}"}/{vsn,\1"$(version)"}/' src/$(PROJECT).app.src

release:
	@rm -fr install
	@mkdir install
	@$(REBAR) release
	@cd _build/default/rel && tar zcf $(PROJECT)-$(VSN).tgz $(PROJECT)
	@tar zxf _build/default/rel/$(PROJECT)-$(VSN).tgz -C install
	@echo "================================================================================"
	@echo "Project installation is in:   $(PWD)/install"
	@echo "Deployment system tarball is: _build/default/rel/$(PROJECT)-$(VSN).tgz"
	@echo "================================================================================"

start: install/bin/$(PROJECT)
	@APP_ROOT=$(APP_ROOT) install/bin/$(PROJECT) console

start-daemon: install/bin/$(PROJECT)
	@APP_ROOT=$(APP_ROOT) install/bin/$(PROJECT) daemon

stop-daemon: install/bin/$(PROJECT)
	@install/bin/$(PROJECT) eval 'init:stop().' || true

ping:
	@install/bin/$(PROJECT) ping || true

# Create Github Pages
gh-pages: VSN=$(shell git describe --always --tags --abbrev=1 | sed 's/^v//')
gh-pages:
	make docs
	make clean
	@if git branch | grep -q gh-pages ; then \
		git checkout gh-pages; \
	else \
		git checkout -b gh-pages; \
	fi
	rm -f rebar.lock
	mv doc/*.* .
	rm -fr src c_src include Makefile *.*dump priv rebar.* README* _build ebin doc bin etc test .github
	@FILES=`git st -uall --porcelain | sed -n '/^?? [A-Za-z0-9]/{s/?? //p}'`; \
	for f in $$FILES ; do \
		echo "Adding $$f"; git add $$f; \
	done
	# Commit & push changes to origin, switch back to master, and restore 'doc' directory
	@sh -c "ret=0; set +e; \
		if   git commit -a --amend -m 'Documentation updated'; \
		then git push origin +gh-pages; echo 'Pushed gh-pages to origin'; \
		else ret=1; git reset --hard; \
		fi; \
		set -e; \
    git checkout master && echo 'Switched to master' && mkdir doc && git --work-tree=doc checkout gh-pages -- .; \
    exit $$ret"
