ROOT_DIR ?= $(shell git rev-parse --show-toplevel)
UNTRACKED ?= $(shell test -z "$(shell git ls-files --others --exclude-standard "$(ROOT_DIR)")" || echo -untracked)
VERSION ?= $(shell git describe --tags --dirty --always)$(UNTRACKED)

IMAGE_NAME ?= glynhanmer/nextactions

BUILD_DIR ?= ./build/$(VERSION)

$(BUILD_DIR):
	mkdir -p $@

SRC_DIR ?= ./src

elm-live: index.html
	elm-live $(SRC_DIR)/Main.elm --open --start-page=$(BUILD_DIR)/index.html -- --output=elm.js


SRC_COPIES ?= manifest.json
$(SRC_COPIES):
	cp -v "$(SRC_DIR)/$@" "$(BUILD_DIR)/"

# phony because elm-live produces this and I can't work out how to produce to to another path and still work in dev mode.
.PHONY: elm.js
elm.js: $(BUILD_DIR)
	elm make $(SRC_DIR)/Main.elm --output=$(BUILD_DIR)/$@

API_KEY ?= "0fc0a5a1dd4723f1e621672ea7ae8b97"
BOARD_ID ?= "LR3ShJNh"
LIST_NAME ?= "Projects"
LOGIN_REDIRECT ?= "https://glynternet.com:8082"

index.html: $(BUILD_DIR)
	sed 's@{{API_KEY}}@$(API_KEY)@g' $(SRC_DIR)/index.html.template \
	| sed 's@{{BOARD_ID}}@$(BOARD_ID)@g' \
	| sed 's@{{LIST_NAME}}@$(LIST_NAME)@g' \
	| sed 's@{{LOGIN_REDIRECT}}@$(LOGIN_REDIRECT)@g' \
	> $(BUILD_DIR)/index.html

build: $(BUILD_DIR) $(SRC_COPIES) elm.js index.html

image: build
	docker build \
		"--build-arg=BUILD_DIR=$(BUILD_DIR)" \
		--tag $(IMAGE_NAME):$(VERSION) \
		.

push-image: image
	docker push $(IMAGE_NAME):$(VERSION)

