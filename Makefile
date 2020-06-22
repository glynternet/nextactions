ROOT_DIR ?= $(shell git rev-parse --show-toplevel)
UNTRACKED ?= $(shell test -z "$(shell git ls-files --others --exclude-standard "$(ROOT_DIR)")" || echo -untracked)
VERSION ?= $(shell git describe --tags --dirty --always)$(UNTRACKED)

IMAGE_NAME ?= glynhanmer/nextactions

BUILD_DIR ?= ./build/$(VERSION)

$(BUILD_DIR):
	mkdir -p $@

SRC_DIR ?= ./src

elm-live:
	elm-live $(SRC_DIR)/Main.elm --open --start-page=$(SRC_DIR)/index.html -- --output=elm.js


SRC_COPIES ?= index.html manifest.json
$(SRC_COPIES):
	cp -v "$(SRC_DIR)/$@" "$(BUILD_DIR)/"

# phony because elm-live produces this and I can't work out how to produce to to another path and still work in dev mode.
.PHONY: elm.js
elm.js:
	elm make $(SRC_DIR)/Main.elm --output=$(BUILD_DIR)/$@

build: $(BUILD_DIR) $(SRC_COPIES) elm.js

image: build
	docker build \
		"--build-arg=BUILD_DIR=$(BUILD_DIR)" \
		--tag $(IMAGE_NAME):$(VERSION) \
		.

push-image: image
	docker push $(IMAGE_NAME):$(VERSION)

