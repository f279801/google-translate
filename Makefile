.PHONY : all test unit-test ecukes clean tag lint

EMACS ?= emacs
SRC = $(filter-out %-pkg.el, $(wildcard *.el reporters/*.el))
ELC = $(SRC:.el=.elc)
CASK ?= cask
PKG_DIR := $(shell $(CASK) package-directory)
FEATURES = $(wildcard features/*.feature)
VERSION = 1.0.0
TARGET_DIR = google-translate-ng-$(VERSION)

all: test tag

test: $(PKG_DIR)
	$(MAKE) unit-test
	$(MAKE) ecukes

unit-test: $(PKG_DIR)
	$(CASK) exec ert-runner

$(PKG_DIR):
	$(CASK) install
	touch $@

ecukes: $(PKG_DIR)
	$(CASK) exec ecukes --reporter magnars --script $(FEATURES) --no-win

version:
	@echo $(VERSION)

tag:
	git tag v$(VERSION) && git push origin --tags

clean:
	rm -rf $(PKG_DIR)

lint:
	./selflint.sh
