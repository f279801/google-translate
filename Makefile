.PHONY : all test unit-test ecukes clean tag lint

EMACS ?= emacs
SRC = $(filter-out %-pkg.el, $(wildcard *.el reporters/*.el))
ELC = $(SRC:.el=.elc)
CASK ?= cask
PKG_DIR := $(shell $(CASK) package-directory)
FEATURES = $(wildcard features/*.feature)
VERSION = 0.11.18
TARGET_DIR = google-translate-$(VERSION)

all: lint test marmalade tag

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

marmalade: marmalade-tar marmalade-upload marmalade-rm

marmalade-tar:
	mkdir $(TARGET_DIR)
	cp google-translate-core-ui.el $(TARGET_DIR)
	cp google-translate-core.el $(TARGET_DIR)
	cp google-translate-default-ui.el $(TARGET_DIR)
	cp google-translate-query-auto-complete.el $(TARGET_DIR)
	cp google-translate-smooth-ui.el $(TARGET_DIR)
	cp google-translate.el $(TARGET_DIR)
	cp README.md $(TARGET_DIR)
	cp google-translate-pkg.el $(TARGET_DIR)
	tar -cf google-translate-$(VERSION).tar $(TARGET_DIR)

marmalade-upload:
	marmalade-upload -u atykhonov google-translate-$(VERSION).tar || true

marmalade-rm:
	rm -rf google-translate-$(VERSION).tar
	rm -rf $(TARGET_DIR)

version:
	@echo $(VERSION)

tag:
	git tag v$(VERSION) && git push origin --tags

clean: marmalade-rm
	rm -rf $(PKG_DIR)

lint:
	@./selflint.sh
