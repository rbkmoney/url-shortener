REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = schemes/swag-url-shortener build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build_utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := url-shortener
# Service image default tag
SERVICE_IMAGE_TAG ?= $(shell git rev-parse HEAD)
# The tag for service image to be pushed with
SERVICE_IMAGE_PUSH_TAG ?= $(SERVICE_IMAGE_TAG)

# Base image for the service
BASE_IMAGE_NAME := service_erlang
BASE_IMAGE_TAG := 16e2b3ef17e5fdefac8554ced9c2c74e5c6e9e11

# Build image tag to be used
BUILD_IMAGE_TAG := eee42f2ca018c313190bc350fe47d4dea70b6d27

CALL_ANYWHERE := all submodules rebar-update compile xref lint dialyze start devrel release clean distclean

CALL_W_CONTAINER := $(CALL_ANYWHERE) test

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

.PHONY: $(CALL_W_CONTAINER)

# CALL_ANYWHERE
$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

rebar-update:
	$(REBAR) update

compile: submodules rebar-update generate
	$(REBAR) compile

xref: submodules
	$(REBAR) xref

lint:
	elvis rock

dialyze:
	$(REBAR) dialyzer

start: submodules
	$(REBAR) run

devrel: submodules
	$(REBAR) release

release: submodules generate
	$(REBAR) as prod release

clean::
	$(REBAR) clean

distclean::
	$(REBAR) clean -a
	rm -rf _build

# CALL_W_CONTAINER
test: submodules generate
	$(REBAR) ct

# Swagger stuff
SWAGGER_CODEGEN = $(call which, swagger-codegen)
SWAGGER_SCHEME_PATH = schemes/swag-url-shortener
SWAGGER_SCHEME = $(SWAGGER_SCHEME_PATH)/swagger.yaml

$(SWAGGER_SCHEME): $(SWAGGER_SCHEME_PATH)/.git

SWAGGER_SERVER_PATH = apps/swag_server
SWAGGER_CLIENT_PATH = apps/swag_client

generate:: swag.server.generate swag.client.generate

swag.server.generate: $(SWAGGER_SERVER_PATH)
swag.client.generate: $(SWAGGER_CLIENT_PATH)

distclean:: swag.server.distclean swag.client.distclean

swag.server.distclean:
	rm -rf $(SWAGGER_SERVER_PATH)
swag.client.distclean:
	rm -rf $(SWAGGER_CLIENT_PATH)

$(SWAGGER_SERVER_PATH) $(SWAGGER_CLIENT_PATH): $(SWAGGER_SCHEME)
	$(SWAGGER_CODEGEN) generate \
		-i $^ \
		-l $(if $(findstring server,$@),erlang-server,erlang-client) \
		-o $@ \
		--additional-properties packageName=$(notdir $(basename $@))
