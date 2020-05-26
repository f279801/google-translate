.PHONY : all test unit-test ecukes clean tag lint build

EMACS ?= emacs
CASK ?= cask
PKG_DIR := $(shell $(CASK) package-directory)
FEATURES = $(wildcard features/*.feature)
VERSION = $(shell $(CASK) version)

all: lint build test marmalade tag

test: lint build $(PKG_DIR)
	$(MAKE) unit-test
	$(MAKE) ecukes

unit-test: $(PKG_DIR)
	$(CASK) exec ert-runner

$(PKG_DIR):
	$(CASK) install

ecukes: $(PKG_DIR)
	$(CASK) exec ecukes --reporter magnars --script $(FEATURES) --no-win

marmalade: marmalade-tar marmalade-upload

marmalade-tar: $(PKG_DIR)
	$(CASK) package

marmalade-upload:
	marmalade-upload -u atykhonov dist/google-translate-$(VERSION).tar || true

version:
	@echo $(VERSION)

tag:
	git tag v$(VERSION) && git push origin --tags

clean:
	rm -rf $(PKG_DIR)
	rm -rf dist
	$(CASK) clean-elc

lint:
	@echo Linting elisp packages
	@./selflint.sh

build: $(PKG_DIR)
	$(CASK) build
