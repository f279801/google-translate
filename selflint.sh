#!/bin/sh -e

EMACS="${EMACS:=emacs}"

NEEDED_PACKAGES="package-lint"

INIT_PACKAGE_EL="(progn \
  (require 'package) \
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
  (package-initialize) \
  (unless package-archive-contents \
     (package-refresh-contents)) \
  (dolist (pkg '(${NEEDED_PACKAGES})) \
    (unless (package-installed-p pkg) \
      (package-install pkg))))"

GOOGLE_TRANSLATE_NG_PACKAGE_FILES="google-translate-ng-backend.el google-translate-ng-core-ui.el \
        google-translate-ng-core.el google-translate-ng-default-ui.el google-translate-ng-pkg.el \
        google-translate-ng-smooth-ui.el google-translate-ng-tk.el google-translate-ng.el"
GOOGLE_TRANSLATE_NG_PACKAGE_MAIN_FILE="google-translate-ng.el"

# Byte compile, failing on byte compiler errors, or on warnings unless ignored
if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi

echo "linting source files: $GOOGLE_TRANSLATE_NG_PACKAGE_FILES..."
# Lint failures are ignored if EMACS_LINT_IGNORE is defined, so that lint
# failures on Emacs 24.2 and below don't cause the tests to fail, as these
# versions have buggy imenu that reports (defvar foo) as a definition of foo.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
	 --eval "(setq package-lint-main-file \"$GOOGLE_TRANSLATE_NG_PACKAGE_MAIN_FILE\")" \
         -f package-lint-batch-and-exit \
         $GOOGLE_TRANSLATE_NG_PACKAGE_FILES || [ -n "${EMACS_LINT_IGNORE+x}" ]

if [ $? -eq 0 ]; then
    echo "linting success."
    exit 0
else
    echo "linting failed, please chekc the report"
    exit 1
fi
