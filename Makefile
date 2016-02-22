ERL = $(shell which erl)

ifeq ($(ERL),)
$(error "Erlang not available on this system")
endif

REBAR=./rebar3

.PHONY: all compile test shell clean release docker-image docker-run docker-stop deb

all: compile test

compile:
	$(REBAR) compile

test: compile
	$(REBAR) eunit

shell: compile
	$(REBAR) shell

clean:
	$(REBAR) clean --all
	rm -rf _build/
	rm -rf docker/resm/

release: all
	$(REBAR) as prod release

RELEASE_DIR=_build/prod/rel/resm

DOCKER_IMAGE=injecto/resm:1.0.0

DOCKER_CONTAINER=resm

docker-image: release
	cp -r $(RELEASE_DIR) docker/
	docker build -t $(DOCKER_IMAGE) docker/
	rm -rf docker/resm/

docker-run:
	docker run -d -p 8888:8888 --name $(DOCKER_CONTAINER) $(DOCKER_IMAGE)

docker-stop:
	docker stop $(DOCKER_CONTAINER)

DEB_ROOT=_build/deb/resm_1.0-0

deb: release
	mkdir -p $(DEB_ROOT)/opt/
	cp -r $(RELEASE_DIR) $(DEB_ROOT)/opt/
	mkdir -p $(DEB_ROOT)/DEBIAN/
	mkdir -p $(DEB_ROOT)/etc/init.d/
	sed -e 's/\(SCRIPT_DIR=\)".\+"/\1\/opt\/resm\/bin/' $(DEB_ROOT)/opt/resm/bin/resm > $(DEB_ROOT)/etc/init.d/resm
	chmod +x $(DEB_ROOT)/etc/init.d/resm
	cp LICENSE $(DEB_ROOT)/DEBIAN/copyright
	cp deb/* $(DEB_ROOT)/DEBIAN/
	fakeroot dpkg-deb --build $(DEB_ROOT)
	mv _build/deb/resm_1.0-0.deb _build/deb/resm_1.0-0_amd64.deb

