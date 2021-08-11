REBAR     = rebar3
env  		 ?= test
APP_ROOT ?= /tmp/$(env)

all: deps compile

compile dialyzer:
	@# The sed removes remaining colors, or else vim doesn't figure out the error location properly
	@REBAR_COLOR=none APP_ROOT=$(ROOT) $(REBAR) $@ | sed -r "s/\x1B\[([0-9]{1,3}(;[0-9]{1,2})?)?[mGK]//g"

clean:
	@$(REBAR) $@

dialyze: dialyzer

deps:
	@$(REBAR) get-deps

test eunit: dialyze
	@$(REBAR) eunit

docs:
	@$(REBAR) edoc

release: VER=$(shell erl -eval '{ok, B} = file:consult("rebar.config"),\
			                          {release,{payments,V}, _} = lists:keyfind(release, 1, proplists:get_value(relx, B)),\
												        io:format("~s\n", [V]), halt(0).' -noinput)
release:
	@rm -fr install
	@mkdir install
	@$(REBAR) tar
	@tar zxf _build/default/rel/payments/payments-$(VER).tar.gz -C install
	@echo "================================================================================"
	@echo "Project installation is in:   $(PWD)/install"
	@echo "Deployment system tarball is: _build/default/rel/payments/payments-$(VER).tar.gz"
	@echo "================================================================================"

start: install/bin/payments
	@APP_ROOT=$(APP_ROOT) install/bin/payments console

start-daemon: install/bin/payments
	@APP_ROOT=$(APP_ROOT) install/bin/payments daemon

stop-daemon: install/bin/payments
	@install/bin/payments eval 'init:stop().' || true

ping:
	@install/bin/payments ping || true

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
