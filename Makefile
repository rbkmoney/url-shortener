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
BASE_IMAGE_NAME := service-erlang
BASE_IMAGE_TAG  := 54a794b4875ad79f90dba0a7708190b3b37d584f

# Build image tag to be used
BUILD_IMAGE_NAME := build-erlang
BUILD_IMAGE_TAG := 491bc06c745a07c6fe9e8b5dbbe958e8e0b82c4c

CALL_ANYWHERE := all submodules rebar-update compile xref lint dialyze \
				release clean distclean check_format format

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

check_format:
	$(REBAR) fmt -c

format:
	$(REBAR) fmt -w

dialyze:
	$(REBAR) dialyzer

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
